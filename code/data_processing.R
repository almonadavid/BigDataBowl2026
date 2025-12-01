
library(tidyverse, quietly = TRUE)
library(gganimate, quietly = TRUE)
library(janitor, quietly = TRUE)
library(cowplot, quietly = TRUE)
library(arrow, quietly = TRUE)
library(sportyR, quietly = TRUE)


## BDB Data
post_throw <- arrow::read_parquet("data/post_throw_tracking.parquet")
pre_throw <- arrow::read_parquet("data/pre_throw_tracking.parquet")
supplement <- suppressMessages(read_csv("data/supplementary_data.csv")) |> filter(season == 2023)

## Getting team colors
team_colors <- suppressMessages(readr::read_tsv("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"))
team_colors <- rbind(team_colors, c("football","#935e38","black","#935e38"))

## Standardizing tracking data & join with supplementary data
pre_throw <- pre_throw |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y),
    ball_land_x = ifelse(play_direction == "left", 120 - ball_land_x, ball_land_x),
    ball_land_y = ifelse(play_direction == "left", 160 / 3 - ball_land_y, ball_land_y),
    absolute_yardline_number = ifelse(play_direction == "left", 120 - absolute_yardline_number, absolute_yardline_number),
    dir = ifelse(play_direction == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(play_direction == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  ) |>
  left_join(supplement, by = c('game_id', 'play_id')) |>
  mutate(player_team = ifelse(player_side == "Offense", possession_team, defensive_team)) |>
  left_join(team_colors, by = c("player_team" = "teams")) |> 
  group_by(game_id, play_id) |>
  mutate(
    throw_frame = ifelse(frame_id == max(frame_id), 1, 0)
  ) |>
  ungroup() |> 
  mutate(
    pre_throw_frame = 1
  )


post_throw <- post_throw |>
  left_join(pre_throw |> select(game_id, play_id, nfl_id, play_direction, player_name, player_position, 
                                player_side, player_role, player_height, player_weight, player_team, absolute_yardline_number) |> distinct(), 
            by = c("game_id", "play_id", "nfl_id")
  ) |>
  group_by(game_id, play_id, nfl_id) |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y),
    dx = x - lag(x),
    dy = y - lag(y),
    s = sqrt(dx^2 + dy^2) * 10,
    a = (s - lag(s)) * 10,
    dir = (90 - (atan2(dy, dx) * 180 / pi)) %% 360
  ) |> 
  ungroup() |> select(-dx, -dy) |> relocate(s, a, dir, .after = y) |>
  left_join(supplement, by = c('game_id', 'play_id')) |>
  left_join(team_colors, by = c("player_team" = "teams"))


# modifying the post_throw frame_id to continue from the last pre_throw frame_id
frame_lookup <- pre_throw |>
  group_by(game_id, play_id) |>
  summarise(last_pre_frame = max(frame_id), .groups = "drop")

post_throw <- post_throw |>
  left_join(frame_lookup, by = c("game_id", "play_id")) |>
  mutate(frame_id = frame_id + last_pre_frame) |>
  select(-last_pre_frame)



## Extrapolate the ball's play-by-play coordinates
source('code/pseudo_ball.R')


## Join pre_throw and post_throw
full_df <- pre_throw |> 
  filter(player_to_predict == TRUE) |> 
  select(-player_to_predict, -player_birth_date, -o, -num_frames_output, -ball_land_x, -ball_land_y) |> 
  bind_rows(post_throw) |> 
  arrange(game_id, play_id, nfl_id, frame_id) |> 
  group_by(game_id, play_id, nfl_id) |>
  mutate(
    s = ifelse(is.na(s) & !is.na(nfl_id), sqrt((x - lag(x))^2 + (y - lag(y))^2) * 10, s),
    a = ifelse(is.na(a) & !is.na(nfl_id), (s - lag(s)) * 10, a),
    dir = ifelse(is.na(dir) & !is.na(nfl_id), (90 - (atan2((y - lag(y)), (x - lag(x))) * 180 / pi)) %% 360, dir),
    dir_rad = pi * (dir / 180),
    dir_x = ifelse(is.na(nfl_id), NA_real_, sin(dir_rad)),
    dir_y = ifelse(is.na(nfl_id), NA_real_, cos(dir_rad)),
    s_x = dir_x * s,
    s_y = dir_y * s,
    a_x = dir_x * a,
    a_y = dir_y * a,
    throw_frame = ifelse(is.na(throw_frame), 0, throw_frame),
    pre_throw_frame = ifelse(is.na(pre_throw_frame), 0, pre_throw_frame)
  ) |> relocate(dir_rad, dir_x, dir_y, s_x, s_y, a_x, a_y, .after = dir)

## Save full_df
write_csv(full_df, 'data/main_data.csv')



# ## Sumer supp. data
# player_play_df <- arrow::read_parquet("data/sumer_coverages_player_play.parquet")
# frame_df <- arrow::read_parquet("data/sumer_coverages_frame.parquet")




