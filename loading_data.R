## Loading in data files
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



post_throw <- arrow::read_parquet("data/post_throw_tracking.parquet")
pre_throw <- arrow::read_parquet("data/pre_throw_tracking.parquet")
supplement <- read_csv("data/supplementary_data.csv")

player_play_df <- arrow::read_parquet("data/sumer_coverages_player_play.parquet")
frame_df <- arrow::read_parquet("data/sumer_coverages_frame.parquet")

## Getting team colors
team_colors <- suppressMessages(readr::read_tsv("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"))
team_colors <- rbind(team_colors, c("football","#935e38","black","#935e38"))


# Standardizing tracking data
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

post_throw <- post_throw |>
  left_join(pre_throw |> select(game_id, play_id, nfl_id, play_direction, player_name, player_position, 
                                player_side, player_role, player_height, player_weight, player_team) |> distinct(), 
    by = c("game_id", "play_id", "nfl_id")
  ) |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y)
  ) |> 
  left_join(supplement, by = c('game_id', 'play_id')) |>
  left_join(team_colors, by = c("player_team" = "teams"))



# only pre-throw rows where `player_to_predict` == TRUE
pre_throw_true <- pre_throw |> filter(player_to_predict == TRUE)


# plotting histogram for frames of ball-in-air
pre_throw |> 
  group_by(game_id, play_id) |> 
  slice(1) |> 
  ggplot(aes(x = num_frames_output)) +
  geom_histogram(color = 'black', binwidth = 1) +
  geom_vline(xintercept = mean(pre_throw$num_frames_output), color = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(xintercept = median(pre_throw$num_frames_output), color = 'blue', linetype = 'dotted', linewidth = 1) +
  labs(
    x = "Frames between ball throw and ball arrival",
    y = "Frequency"
  ) +
  theme_bw()

# summary(pre_throw$num_frames_output)
# prop.table(table(pre_throw$team_coverage_type)) ? focus only on one coverage type, say COVER_3_ZONE?
# prop.table(table(pre_throw$pass_result)) ? focus only on completed passes?
# nrow(pre_throw_true |> group_by(game_id, play_id) |> slice(1) |> filter(pass_length < 0)) ? remove plays where ball is thrown behind LOS (7.71% of unique game plays)



# Ravens (2024) led the NFL with 8.8 Yds/Pass Attempt, while the 49ers (2023) had 9.3, the highest that season.
# Eagles (2024) allowed the fewest with 6.0 Yds/Pass Attempt, while the Browns and Ravens (2023) tied at 5.9, lowest that year.






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
