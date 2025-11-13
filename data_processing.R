
library(tidyverse)
library(gganimate)
library(janitor)
library(cowplot)
library(arrow)
library(sportyR)

## Read Data

# tracking <- read_csv("data/train/output_2023_w01.csv") |>
#   bind_rows(read_csv("data/train/output_2023_w02.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w03.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w04.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w05.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w06.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w07.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w08.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w09.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w10.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w11.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w12.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w13.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w14.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w15.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w16.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w17.csv")) |>
#   bind_rows(read_csv("data/train/output_2023_w18.csv"))
# 
# # write the combined tracking data object into a parquet file
# library(arrow)
# arrow::write_parquet(tracking, "data/post_throw_tracking.parquet")
# 
# tracking2 <- read_csv("data/train/input_2023_w01.csv") |>
#   bind_rows(read_csv("data/train/input_2023_w02.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w03.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w04.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w05.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w06.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w07.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w08.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w09.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w10.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w11.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w12.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w13.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w14.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w15.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w16.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w17.csv")) |>
#   bind_rows(read_csv("data/train/input_2023_w18.csv"))
# 
# # write the combined tracking data object into a parquet file
# arrow::write_parquet(tracking2, "data/pre_throw_tracking.parquet")


# BDB Data
post_throw <- arrow::read_parquet("data/post_throw_tracking.parquet")
pre_throw <- arrow::read_parquet("data/pre_throw_tracking.parquet")
supplement <- read_csv("data/supplementary_data.csv")

## Getting team colors
team_colors <- suppressMessages(readr::read_tsv("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"))
team_colors <- rbind(team_colors, c("football","#935e38","black","#935e38"))

## Standardizing tracking data
pre_throw <- pre_throw |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y),
    dir = ifelse(play_direction == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(play_direction == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  ) |>
  left_join(supplement, by = c('game_id', 'play_id')) |>
  mutate(player_team = ifelse(player_side == "Offense", possession_team, defensive_team)) |>
  left_join(team_colors, by = c("player_team" = "teams"))
  # group_by(game_id, play_id) |>
  # mutate(
  #   last_frame = ifelse(frame_id == max(frame_id), 1, 0)
  # ) |>
  # ungroup()


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


## Sumer supp. data
player_play_df <- arrow::read_parquet("data/sumer_coverages_player_play.parquet")
frame_df <- arrow::read_parquet("data/sumer_coverages_frame.parquet")

## nflverse data
library(nflreadr)

dict <- nflreadr::dictionary_pbp
# OR: https://nflreadr.nflverse.com/articles/dictionary_pbp.html

pbp_pass <- load_pbp(c(2023)) |> # add 2024 & 2025
  filter(play_type == "pass" & lateral_reception == 0) |> 
  select(game_id = old_game_id, play_id, season, pass_length_cat = pass_length, pass_location, yards_after_catch, 
         ep, epa, air_epa, yac_epa, xyac_epa, comp_air_epa, comp_yac_epa, wp, def_wp, home_wp, away_wp,
         cp, cpoe) |> 
  mutate(game_id = as.double(game_id))

## Join BDB with nflverse
pre_throw_updated <- left_join(pre_throw |> filter(season == 2023), pbp_pass, by = c('game_id', 'play_id', 'season')) |> 
  mutate(pass_length_cat = ifelse(pass_length <= 15, 'short', 'deep'))

# only deep balls
deep_balls <- pre_throw_updated |> filter(pass_length_cat == 'deep')
# deep_balls |> group_by(game_id, play_id) |> slice(1) |> pull(route_of_targeted_receiver) |> table()
# deep_balls |> group_by(game_id, play_id) |> slice(1) |> pull(team_coverage_man_zone) |> table() |> prop.table()

# filter out rows where the QB isn't the passer?



# only pre-throw rows where `player_to_predict` == TRUE
pre_throw_true <- pre_throw |> filter(player_to_predict == TRUE)

post_throw_true <- left_join(post_throw, pre_throw |> select(game_id, play_id, color1, ))







# speed_dist <- post_throw |> 
#   group_by(game_id,play_id,nfl_id) |> 
#   mutate(
#     dist = sqrt((x - lag(x))^2 + (y - lag(y))^2),
#     s = round(dist / 0.1, 2)
#   ) #|> 
#   # left_join(
#   #   supplement |> select(game_id, play_id, play_description, route_of_targeted_receiver), 
#   #   by = c('game_id', 'play_id'))

# add route type, then facet wrap by route type






# tracking <- tracking |> 
#   mutate(all_ids = paste(game_id, play_id, nfl_id),
#          game_play_id = paste(game_id, play_id))





# idea: how a receiver adjusts to the flight of the ball once they turn.
# figure out how orientation is measured...from head? neck? torso? hips?
# catch probability?
# landing zone predicition?
# 

# # filtering pre_throw for player to predict
# test <- pre_throw |> 
#   filter(player_to_predict) |> 
#   distinct(game_id, play_id, nfl_id, .keep_all = TRUE)
