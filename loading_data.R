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
  left_join(supplement, by = c('game_id', 'play_id'))

post_throw <- post_throw |>
  left_join(pre_throw |> select(game_id, play_id, nfl_id, frame_id, play_direction), by = c("game_id","play_id","nfl_id","frame_id")) |> 
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y)
  ) |> 
  left_join(supplement, by = c('game_id', 'play_id'))




speed_dist <- post_throw |> 
  group_by(game_id,play_id,nfl_id) |> 
  mutate(
    dist = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    s = round(dist / 0.1, 2)
  ) #|> 
  # left_join(
  #   supplement |> select(game_id, play_id, play_description, route_of_targeted_receiver), 
  #   by = c('game_id', 'play_id'))

# add route type, then facet wrap by route type






# tracking <- tracking |> 
#   mutate(all_ids = paste(game_id, play_id, nfl_id),
#          game_play_id = paste(game_id, play_id))





# idea: how a receiver adjusts to the flight of the ball once they turn.
# figure out how orientation is measured...from head? neck? torso? hips?
# catch probability?
# landing zone predicition?
# 

# filtering pre_throw for player to predict
test <- pre_throw |> 
  filter(player_to_predict) |> 
  distinct(game_id, play_id, nfl_id, .keep_all = TRUE)

ggplot(aes(x = num_frames_output), data = test) + 
  geom_histogram(bins = 30)
