
library(tidyverse, quietly = TRUE)
# library(janitor, quietly = TRUE)
# library(cowplot, quietly = TRUE)
# library(arrow, quietly = TRUE)
future::plan("multisession")



main_data <- suppressMessages(read_csv('data/main_data.csv'))

all_description <- main_data |> group_by(game_id, play_id) |> slice(1) |> ungroup() |> select(game_id, play_id, play_description)


## nflverse data
library(nflreadr)

dict <- nflreadr::dictionary_pbp
# OR: https://nflreadr.nflverse.com/articles/dictionary_pbp.html

pbp_pass <- load_pbp(c(2023)) |>
  progressr::with_progress() |> 
  filter(play_type == "pass" & lateral_reception == 0) |>
  select(game_id = old_game_id, play_id, season, pass_length_cat = pass_length, yards_after_catch,
         cp) |>
  mutate(game_id = as.double(game_id))



# filter out rows where the QB isn't the passer?