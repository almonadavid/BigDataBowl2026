
library(tidyverse, quietly = TRUE)
library(gganimate, quietly = TRUE)
library(janitor, quietly = TRUE)
library(cowplot, quietly = TRUE)
library(arrow, quietly = TRUE)
library(sportyR, quietly = TRUE)
future::plan("multisession")



main_data <- suppressMessages(read_csv('data/main_data.csv'))

all_description <- main_data |> group_by(game_id, play_id) |> slice(1) |> ungroup() |> select(game_id, play_id, play_description)


# ## nflverse data
# library(nflreadr)
# 
# dict <- nflreadr::dictionary_pbp
# # OR: https://nflreadr.nflverse.com/articles/dictionary_pbp.html
# 
# pbp_pass <- load_pbp(c(2023)) |> # add 2024 & 2025
#   filter(play_type == "pass" & lateral_reception == 0) |> 
#   select(game_id = old_game_id, play_id, season, pass_length_cat = pass_length, pass_location, yards_after_catch, 
#          ep, epa, air_epa, yac_epa, xyac_epa, comp_air_epa, comp_yac_epa, wp, def_wp, home_wp, away_wp,
#          cp, cpoe) |> 
#   mutate(game_id = as.double(game_id))
# 
# ## Join BDB with nflverse
# test <- left_join(main_data |> select(-season), pbp_pass, by = c('game_id', 'play_id')) |> 
#   mutate(pass_length_cat = ifelse(pass_length <= 15, 'short', 'deep'))
# 
# test <- left_join(main_data |> filter(season == 2023), pbp_pass, by = c('game_id', 'play_id', 'season')) |>
#   mutate(pass_length_cat = ifelse(pass_length <= 15, 'short', 'deep'))
# 
# 
# ## only deep balls
# deep_balls <- test |> filter(pass_length_cat == 'deep') 
# deep_balls |> group_by(game_id, play_id) |> slice(1) |> pull(route_of_targeted_receiver) |> table()
# deep_balls |> group_by(game_id, play_id) |> slice(1) |> pull(team_coverage_man_zone) |> table() |> prop.table()

# filter out rows where the QB isn't the passer?