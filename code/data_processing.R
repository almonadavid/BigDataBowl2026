
library(tidyverse)
library(gganimate)
library(janitor)
library(cowplot)
library(arrow)
library(sportyR)


## BDB Data
post_throw <- arrow::read_parquet("data/post_throw_tracking.parquet")
pre_throw <- arrow::read_parquet("data/pre_throw_tracking.parquet")
supplement <- read_csv("data/supplementary_data.csv")

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
  left_join(pre_throw |> select(game_id, play_id, nfl_id, season, play_direction, player_name, player_position, 
                                player_side, player_role, player_height, player_weight, player_team, absolute_yardline_number) |> distinct(), 
            by = c("game_id", "play_id", "nfl_id")
  ) |>
  group_by(game_id, play_id, nfl_id) |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = round(ifelse(play_direction == "left", 160 / 3 - y, y), 2),
    dx = x - lag(x),
    dy = y - lag(y),
    s = round(sqrt(dx^2 + dy^2) * 10, 2),
    a = round((s - lag(s)) * 10, 2),
    dir = round((90 - (atan2(dy, dx) * 180 / pi)) %% 360, 2)
  ) |> 
  ungroup() |> select(-dx, -dy) |> relocate(s, a, dir, .after = y) |>
  left_join(supplement, by = c('game_id', 'play_id', 'season')) |>
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
    s = ifelse(is.na(s) & !is.na(nfl_id), round(sqrt((x - lag(x))^2 + (y - lag(y))^2) * 10, 2), s),
    a = ifelse(is.na(a) & !is.na(nfl_id), round((s - lag(s)) * 10, 2), a),
    dir = ifelse(is.na(dir) & !is.na(nfl_id), round((90 - (atan2((y - lag(y)), (x - lag(x))) * 180 / pi)) %% 360, 2), dir),
    throw_frame = ifelse(is.na(throw_frame), 0, throw_frame),
    pre_throw_frame = ifelse(is.na(pre_throw_frame), 0, pre_throw_frame)
  )


##
all_description <- full_df |> group_by(game_id, play_id) |> slice(1) |> ungroup() |> select(game_id, play_id, play_description)

## Save full_df
write_csv(full_df, 'data/main_data.csv')



# ## Sumer supp. data
# player_play_df <- arrow::read_parquet("data/sumer_coverages_player_play.parquet")
# frame_df <- arrow::read_parquet("data/sumer_coverages_frame.parquet")




