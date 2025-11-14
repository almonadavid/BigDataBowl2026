
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
  left_join(team_colors, by = c("player_team" = "teams"))
  # group_by(game_id, play_id) |>
  # mutate(
  #   throw_frame = ifelse(frame_id == max(frame_id), 1, 0)
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


# modifying the post_throw frame_id to continue from the last pre_throw frame_id
frame_lookup <- pre_throw |>
  group_by(game_id, play_id) |>
  summarise(last_pre_frame = max(frame_id), .groups = "drop")

post_throw <- post_throw |>
  left_join(frame_lookup, by = c("game_id", "play_id")) |>
  mutate(frame_id = frame_id + last_pre_frame) |>
  select(-last_pre_frame)



## Extrapolate the ball's play-by-play coordinates
source('pseudo_ball.R')


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
    dir = ifelse(is.na(dir) & !is.na(nfl_id), round((90 - (atan2((y - lag(y)), (x - lag(x))) * 180 / pi)) %% 360, 2), dir)
  )



##
all_description <- pre_throw |> group_by(game_id, play_id) |> slice(1) |> ungroup() |> select(game_id, play_id, play_description)

##

# one last thing, since i'm joining pre_throw and post_throw, i wanted to add an indicator for the last frame of pre_throw (when the QB releases) [and maybe even one for 0.2s/2 frames] before release] i case I need it for future analysis.  if i can just lag(2) on the last_frame indicator to get the 2 frames before then there wont be any need for another indicator. if not, it wouldnt hurt.
# 
# and also the best place in the enter sequence to add it such that there's no errors when i bind_rows with post_throw.



















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
