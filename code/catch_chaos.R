
# the idea is that a perfect QB throw should land to the receiver in stride, not breakage.


library(tidyverse)
library(data.table)

main_data <- suppressMessages(fread('data/main_data.csv'))

# deep routes: 'GO', 'CORNER', 'POST' (WHEEL...??)

deep_plays <- main_data |> 
  filter(player_role == 'Targeted Receiver' & pass_length >= 15) |>  #  %in% c('GO', 'POST', 'CORNER', 'WHEEL')
  filter(!(game_id == 2023122100 & play_id == 1450)) # problematic play

# smooth acceleration values with GAM
library(mgcv)

deep_plays <- deep_plays |>
  group_by(game_id, play_id) |>
  mutate(
    smoothed_a = predict(gam(a ~ s(frame_id, bs = "cs"), data = pick(everything())))
  ) |> ungroup() |> relocate(smoothed_a, .after = a)


# Isolating the play descriptions
descriptions <- deep_plays |> 
  #filter(route_of_targeted_receiver == 'CROSS') |> 
  group_by(game_id, play_id) |> 
  slice(1) |> 
  ungroup() |> 
  select(game_id, play_id, play_description)


ggplot(aes(x, y), data = subset(deep_plays, game_id == 2023090700 & play_id == 1679)) +
  geom_point(size = 2) +
  geom_vline(aes(xintercept = x[throw_frame == 1]), color = 'red', linetype = 'dashed', linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, data = subset(deep_plays, game_id == 2023090700 & play_id == 1679 & pre_throw_frame == 1), color = "blue", xseq = c(44.5, 68)) +
  geom_smooth(method = "lm", se = FALSE, data = subset(deep_plays, game_id == 2023090700 & play_id == 1679 & pre_throw_frame == 0), color = "green") +
  coord_fixed() +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))

#-------------------------------------------------------------------------------------------------------
### Angle between the line of best fit before vs. after the throw frame
angle_between_lines <- function(x, y, pre_throw_frame) {
  
  before_throw_model <- lm(y ~ x, subset = pre_throw_frame == 1)
  after_throw_model <- lm(y ~ x, subset = pre_throw_frame == 0)
  
  # Slopes
  m1 <- coef(before_throw_model)[2]
  m2 <- coef(after_throw_model)[2]
  
  # Angle between lines
  theta <- atan(abs(m1 - m2) / (1 + m1*m2))
  theta * 180 / pi
}

angle_between <- deep_plays |>
  group_by(game_id, play_id) |>
  summarise(theta_degrees = abs(angle_between_lines(x, y, pre_throw_frame)),
            .groups = 'drop')

# theta_degrees: How much the receiver’s actual movement path (x–y trajectory) changed before vs. after the throw.
# High: they changed running direction sharply.
# Low: their path stayed smooth and consistent.



dir_rotation <- deep_plays |> 
  group_by(game_id, play_id) |> 
  summarise(
    avg_dir_pre = mean(dir[pre_throw_frame == 1]),
    avg_dir_post = mean(dir[pre_throw_frame == 0]),
    dir_rot = avg_dir_post - avg_dir_pre,
    dir_rot = abs(((dir_rot + 180) %% 360) - 180),
    .groups = 'drop'
  )

# if someone moves from 90 to 181, dir change is 91. but is someone moves from 90 to 359, dir change is 269 when it should actually be 91
# perhaps create a function() and within, temporarily adjust degree such than moving right is 180 deg and left is 0 deg, 90 up and 270 down

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Counting changes in dir_bin when ball-in-air

# Direction bins based on Lucca's TEndencIQ
deep_plays <- deep_plays |> 
  mutate(
    dir_bin = case_when(
      dir < 0 | dir > 180 ~ 'backwards',
      (dir >= 0 & dir < 36) | (dir >= 144 & dir <= 180) ~ 'steep inside/outside',
      (dir >= 36 & dir < 72) | (dir >= 108 & dir < 144) ~ 'inside/outside',
      dir >= 72 & dir < 108 ~ 'forward'
    )
  ) |> 
  relocate(dir_bin, .after = dir)

num_dir_changes <- deep_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  select(game_id, play_id, dir_bin) |> 
  mutate(is_change = dir_bin != lag(dir_bin) & !is.na(lag(dir_bin))) |> 
  summarise(n_changes = sum(is_change, na.rm = TRUE), .groups='drop')

# High n_changes: The receiver kept switching directional categories.
# This means lots of adjustment while the ball was in the air.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Lateral drift
find_lateral_drift <- function(x, y, throw_frame, pre_throw_frame, frame_id) {

  pre_throw_model <- lm(y ~ x, subset = pre_throw_frame == 1)
  baseline_slope <- coef(pre_throw_model)[2]
  
  expected_y <- y[throw_frame == 1] + baseline_slope * (x[last(frame_id)] - x[throw_frame == 1])

  abs(y[last(frame_id)] - expected_y)
}

lat_diff <- deep_plays |>
  group_by(game_id, play_id) |> 
  summarise(
    lateral_drift = find_lateral_drift(x, y, throw_frame, pre_throw_frame, frame_id),
    .groups = "drop"
  )

# lateral_drift: How much the receiver deviated laterally from their pre-throw trajectory.
# This accounts for already "slanted" go routes by measuring deviation from their pre_throw path.

#-------------------------------------------------------------------------------------------------------

  
#-------------------------------------------------------------------------------------------------------
### Avg. speed before vs after throw
avg_speed_change <- deep_plays |> 
  group_by(game_id, play_id) |>
  summarise(
    avg_speed_pre_throw = mean(s[pre_throw_frame == 1], na.rm = TRUE),
    avg_speed_post_throw = mean(s[pre_throw_frame == 0], na.rm = TRUE),
    speed_change = abs(avg_speed_post_throw - avg_speed_pre_throw),
    .groups = "drop"
  ) |> select(game_id, play_id, speed_change)

# High speed change: The receiver slowed down or sped up significantly after the throw compared to before it.
  
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Mean Turning Rate while ball-in-air in deg/sec
turning_rate <- deep_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  mutate(
    delta_dir = dir - lag(dir),
    delta_dir = ((delta_dir + 180) %% 360) - 180 # this correct for fluctations between 360 deg and 0 deg
    ) |> 
  summarise(avg_turn_rate = mean(abs(delta_dir) / 0.1, na.rm = TRUE), .groups = "drop")
  
# High turning rate: The receiver was making large or frequent directional changes 
# while the ball was in the air — lots of adjustment, possibly reacting to ball trajectory or defender.
# Low turning rate: The receiver ran very straight with minimal directional change, 
# indicating a clean, uninterrupted path to the ball.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Deceleration at catch
catch_deceleration <- deep_plays |>
  group_by(game_id, play_id) |>
  slice_tail(n = 5) |>
  summarise(
    decel_at_catch = abs(round(first(smoothed_a) - last(smoothed_a), 4)),
    .groups = "drop"
  )

# Specifically looking at last five frames before catch as the catch window.
# decel_at_catch > 0: The receiver slowed down approaching the catch.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Acceleration while ball-in-air
avg_acceleration <- deep_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  summarise(
    avg_accel = abs(mean(smoothed_a, na.rm = TRUE)),
    .groups = "drop"
  )

# Higher value shows that the receiver was changing speed more aggressively while the ball was in the air.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Play result
play_result <- deep_plays |> 
  group_by(game_id, play_id) |>
  summarise(pass_result = unique(pass_result), .groups = "drop")

# C: complete
# I: incomplete
# IN: interception

#-------------------------------------------------------------------------------------------------------


deep_summary <- play_result |> 
  left_join(angle_between, by = c('game_id', 'play_id')) |>
  left_join(num_dir_changes, by = c('game_id', 'play_id')) |> 
  left_join(lat_diff, by = c('game_id', 'play_id')) |> 
  left_join(avg_speed_change, by = c('game_id', 'play_id')) |> 
  left_join(turning_rate, by = c('game_id', 'play_id')) |> 
  left_join(catch_deceleration, by = c('game_id', 'play_id')) |>
  left_join(avg_acceleration, by = c('game_id', 'play_id'))
  

### Get Chaos Score
chaos_result <- deep_summary |>
  left_join(deep_plays |>
              group_by(game_id, play_id) |>
              slice(1) |>
              ungroup() |> 
              select(game_id, play_id, player_name, route_of_targeted_receiver, player_position, possession_team, team_coverage_man_zone, team_coverage_type, pass_length),
            by = c("game_id", "play_id"))


scaled <- chaos_result |>
  group_by(route_of_targeted_receiver) |>
  mutate(
    across(theta_degrees:avg_accel, scale)
    ) |>
  ungroup() |> 
  select(theta_degrees:avg_accel)

chaos_result$chaos_score <- sqrt(rowSums(scaled^2))
chaos_result <- chaos_result |> arrange(desc(chaos_score)) |> filter(!is.na(route_of_targeted_receiver)) # just one play didn't have a tagged route


# histogram of chaos score
ggplot(data = chaos_result, aes(x = chaos_score)) +
  geom_histogram(fill = 'lightblue', color = 'black') +
  theme_bw()
  


# histogram of all features
chaos_result |> #filter(route_of_targeted_receiver == 'C') |>
  select(theta_degrees:avg_accel) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "lightblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Histograms of Deep Go Features",
    x = "Value",
    y = "Frequency"
  ) +
  theme_classic()


















# limitation: does not account for changes in player orientation



# compare score with cp (completion probability), yac, yacoe in nflreadr
# for receivers, compare number of catches to avg adjustment score??
# separation at throw


# chaos score by receiver
chaos_result |> 
  group_by(player_name) |> 
  summarise(
    n = n(),
    mean_score = mean(chaos_score),
    comp_rate = round(sum(pass_result == 'C') / n, 3)
  ) |> filter(n >= 15) |>  View() 
  ggplot() +
  geom_point(aes(x = mean_score, y = comp_rate)) +
  geom_smooth(aes(x = mean_score, y = comp_rate), method = 'lm', se = FALSE)


# chaos score by routes
chaos_result |> 
  group_by(route_of_targeted_receiver) |> 
  summarise(
    n = n(),
    mean_score = mean(chaos_score),
    comp_rate = round(sum(pass_result == 'C') / n, 3)
  )


# chaos score by QB
play_passer <- arrow::read_parquet("data/pre_throw_tracking.parquet") |> 
  filter(player_role == "Passer") |> 
  distinct(game_id, play_id, .keep_all = TRUE) |> 
  transmute(
    game_id,
    play_id,
    passer = player_name
  )

qb_data <- fread("data/2023_NFL_Passing.csv") |> 
  select(Player, Rate, QBR)

chaos_result |>
  left_join(play_passer, by = c('game_id', 'play_id')) |>
  group_by(passer) |> 
  summarise(
    n = n(),
    mean_score = mean(chaos_score),
    comp_rate = round(sum(pass_result == 'C') / n, 3)
  ) |> filter(n >= 20) |> 
  arrange(desc(mean_score)) |>
  left_join(qb_data, by = join_by(passer == Player)) |> 
  drop_na() |> 
  ggplot() +
  geom_point(aes(x = mean_score, y = Rate)) +
  geom_smooth(aes(x = mean_score, y = Rate), method = 'lm', se=FALSE) +
  theme_bw()
  




