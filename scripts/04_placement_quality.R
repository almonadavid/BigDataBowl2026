
library(tidyverse)
library(data.table)
library(mgcv)
library(corrplot)
future::plan("multisession")


main_data <- suppressMessages(fread('data/main_data.csv')) 

plays <- main_data |>
  mutate(dir_rad = (90 - dir) * pi/180) |> 
  filter(player_role == 'Targeted Receiver') |>
  filter(!(game_id == 2023122100 & play_id == 1450) &
           !(game_id == 2023091100 & play_id == 3167) &
           !(game_id == 2023122402 & play_id == 1054) &
           !(game_id == 2023091013 & play_id == 3974) &
           !(game_id == 2023091000 & play_id == 3369)) # problematic plays

## smooth acceleration values with GAM
plays <- plays |>
  group_by(game_id, play_id) |>
  mutate(
    smoothed_a = predict(gam(a ~ s(frame_id, bs = "cs"), data = pick(everything())))
  ) |> ungroup() |> relocate(smoothed_a, .after = a)


## extending post_throw analysis to 2 frames (0.2s) before throw frame accounting for QB already made decision
plays <- plays |>
  group_by(game_id, play_id) |>
  mutate(
    throw_fid = frame_id[throw_frame == 1][1],
    pre_throw_shifted = if_else(frame_id <= throw_fid - 2, 1, 0)
  ) |>
  ungroup()


## isolating the play descriptions
descriptions <- plays |> 
  group_by(game_id, play_id) |> 
  slice(1) |> ungroup() |> 
  select(game_id, play_id, play_description)


## play result
play_result <- plays |> 
  group_by(game_id, play_id) |>
  summarise(pass_result = unique(pass_result), .groups = "drop")

# ---------------------------------------------------------------------------------
circular_mean <- function(theta) {
  atan2(mean(sin(theta)), mean(cos(theta)))
}

placement_errors <- plays |>
  group_by(game_id, play_id) |>
  summarise(
    
    # at throw frame, project where receiver would naturally arrive
    throw_x = x[throw_frame == 1],
    throw_y = y[throw_frame == 1],
    throw_vx = mean(s[pre_throw_shifted == 1] * cos(dir_rad[pre_throw_shifted == 1])),
    throw_vy = mean(s[pre_throw_shifted == 1] * sin(dir_rad[pre_throw_shifted == 1])),
    pre_dir_rad = circular_mean(dir_rad[pre_throw_shifted == 1]),
    air_frames = sum(pre_throw_frame == 0),
    
    expected_catch_x = throw_x + throw_vx * (air_frames * 0.1), # xcatch = x0 + vx0 * tair
    expected_catch_x = pmax(0, pmin(120, expected_catch_x)),
    
    expected_catch_y = throw_y + throw_vy * (air_frames * 0.1),
    expected_catch_y = pmax(0, pmin(53.3, expected_catch_y)),
    
    actual_catch_x = last(x),
    actual_catch_y = last(y),
    
    error_x = actual_catch_x - expected_catch_x,
    error_y = actual_catch_y - expected_catch_y,
    
    # lateral and depth error. essentially, lateral_error is too far left/right, depth_error is too short/deep relative to receiver's path not field x-y axes
    lateral_error = abs(error_x * sin(pre_dir_rad) - error_y * cos(pre_dir_rad)),
    depth_error = abs(error_x * cos(pre_dir_rad) + error_y * sin(pre_dir_rad)),
    .groups = "drop"
  ) |> select(game_id, play_id, lateral_error, depth_error)


adjustment_work <- plays |>
  filter(pre_throw_shifted == 0) |>
  group_by(game_id, play_id) |>
  mutate(
    delta_dir = dir - lag(dir),
    delta_dir = ((delta_dir + 180) %% 360) - 180,
    weighted_turn = abs(delta_dir) * s / max(s, na.rm = TRUE) # reducing the effect of small wiggles on a spot
  ) |> 
  summarise(
    turn_rate = mean(weighted_turn / 0.1, na.rm = TRUE),
    total_accel_effort = sum(abs(smoothed_a), na.rm = TRUE),
    .groups = "drop"
  )


ball_placement_quality <- placement_errors |>
  left_join(adjustment_work, by = c("game_id", "play_id")) |>
  mutate(
    placement_score = -(scale(lateral_error)[,1] + scale(depth_error)[,1] + scale(turn_rate)[,1] + scale(total_accel_effort)[,1])) |>
  arrange(desc(placement_score))

# # add to mutate() if  to covert to 0-100% scale
# placement_z = scale(placement_score)[,1],
# placement_pct = 100 / (1 + exp(-placement_z))


## checking correlation
# cor(ball_placement_quality |> select(turn_rate, lateral_error:total_accel_effort), use = "pairwise.complete.obs")

ball_placement_quality |> 
  select(turn_rate, lateral_error:total_accel_effort) |> 
  cor(use = "pairwise.complete.obs") |>
  corrplot(method = "color", type = "upper", 
           addCoef.col = "black", number.cex = 0.7,
           tl.col = "black", tl.srt = 45)

ball_placement_quality <- ball_placement_quality |>
  left_join(plays |>
              group_by(game_id, play_id) |>
              slice(1) |>
              ungroup() |> 
              select(game_id, play_id, player_name, route_of_targeted_receiver, player_position, possession_team, team_coverage_man_zone, team_coverage_type, pass_length),
            by = c("game_id", "play_id")) |> 
  drop_na()


fwrite(ball_placement_quality, "data/ball_placement_quality.csv")
