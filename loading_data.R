# Loading in data files
library(tidyverse)

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

library(arrow)
post_throw <- arrow::read_parquet("data/post_throw_tracking.parquet")
pre_throw <- arrow::read_parquet("data/pre_throw_tracking.parquet")
# supplement <- read_csv("data/supplementary_data.csv")
# glimpse(supplement)