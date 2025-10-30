## Loading in data files
library(tidyverse)
library(gganimate)
library(janitor)
library(cowplot)
library(arrow)

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
  )

post_throw <- post_throw |>
  left_join(pre_throw |> select(game_id, play_id, nfl_id, frame_id, play_direction), by = c("game_id","play_id","nfl_id","frame_id")) |> 
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y)
  )


speed_dist <- post_throw |> 
  group_by(game_id,play_id,nfl_id) |> 
  mutate(
    dist = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    s = round(dist / 0.1, 2)
  )

# add route type, then facet wrap by route type






# tracking <- tracking |> 
#   mutate(all_ids = paste(game_id, play_id, nfl_id),
#          game_play_id = paste(game_id, play_id))


# plotting function
play_visualization <- function(gameid, playid){
  library(sportyR)
  field_params <- list(field_apron = "springgreen3",
                       field_border = "springgreen3",
                       offensive_endzone = "springgreen3",
                       defensive_endzone = "springgreen3",
                       offensive_half = "springgreen3",
                       defensive_half = "springgreen3")
  nfl_field <- geom_football(league = "nfl",
                             display_range = "in_bounds_only",
                             x_trans = 60,
                             y_trans = 26.6667,
                             #xlims = c(40, 100),
                             xlims = c(20, 90),
                             color_updates = field_params)
  example_play <- tracking |> 
    filter(game_id == gameid, 
           play_id == playid) |> 
    # frame_type == "BEFORE_SNAP") |> 
    mutate(
      color = case_when(
        #display_name == "Taysom Hill" ~ "red",
        #club == "NO" ~ "gold",
        nfl_id == 54577 ~ "gold",
        club == "football" ~ "brown",
        # club == "BUF" ~ "blue",
        club == "CLE" ~ "#FF3C00",
        club == "CAR" ~ "#0085CA",
        club == "GB" ~ "#203731",
        club == "DET" ~ "#0076B6",
        club == "MIN" ~ "#4F2683",
        club == "NYG" ~ "#0B2265",
        TRUE ~ "white"
      )
    )
  caption_text <- plays |>
    filter(game_play_id == paste(gameid, playid)) |> 
    pull(play_description) |> str_replace("\\)\\.", "\\)")
  nfl_field +
    geom_point(data = example_play,
               aes(x, y, fill = color),
               size = 5,
               shape = 21,
               color = "black") +
    transition_time(example_play$frame_id) + 
    # labs(title = "<span style = 'color:#203731;'>**Green Bay Packers**</span> @ <span style = 'color:#4F2683;'>**Minnesota Vikings**</span>, 2022 NFL Week 1",
    #       subtitle = str_c("Q2: ", caption_text, "\n"),
    #      fill = "") +
    # labs(title = "<span style = 'color:#203731;'>**Green Bay Packers**</span> @ <span style = 'color:#0076B6;'>**Detroit Lions**</span>, 2022 NFL Week 9",
    #      subtitle = str_c("Q2: ", caption_text, "\n"),
    #      fill = "") +
    labs(title = "<span style = 'color:#0B2265;'>**New York Giants**</span> @ <span style = 'color:#203731;'>**Green Bay Packers**</span>, 2022 NFL Week 5",
         # subtitle = str_c("Q2: ", caption_text, "\n"),
         subtitle = str_c("Q4: ", caption_text, "\n"),
         fill = "") +
    # scale_fill_identity(guide = "legend", labels = c(
    #   "#203731" = "Packers",
    #   "#4F2683" = "Vikings",
    #   "gold" = "Randall Cobb",
    #   "brown" = "Football"
    # )) +
    # scale_fill_identity(guide = "legend", labels = c(
    #   "#203731" = "Packers",
    #   "#0076B6" = "Lions",
    #   "gold" = "Marcedes Lewis",
    #   "brown" = "Football"
    # )) +
    scale_fill_identity(guide = "legend", labels = c(
      "#203731" = "Packers",
      "#0B2265" = "Giants",
      "gold" = "Daniel Bellinger",
      "brown" = "Football"
    )) +
    theme(
      plot.title = ggtext::element_markdown(size = 15, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
      legend.position = "top"
    )
  # example_play |>
  #   ggplot(aes(x, y, fill = color)) +
  #   geom_point(size = 5, shape = 21, color = "black") +
  #   scale_fill_identity() +
  #   transition_time(frame_id)
}
play_visualization(2022100900, 3109)
anim_save("over_animation.gif", play_visualization(2022091101, 2501))





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
