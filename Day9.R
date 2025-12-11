library(tidyverse)

#Sample Data
# data <- c("7,1", "11,1", "11,7", "9,7", "9,5", "2,5", "2,3", "7,3")

#Real Data
data <- read_lines("Day9")

tictoc::tic()
#Part 1
data_tbl <- data |>
  str_split(",", simplify = TRUE) |>
  as.data.frame()
names(data_tbl) <- c("x", "y")
data_tbl <- data_tbl |>
  tibble() |>
  mutate(across(everything(), as.numeric))

max_area <- 0
for (i in 1:(nrow(data_tbl) - 1)) {
  for (j in i:nrow(data_tbl)) {
    area <- (abs(data_tbl$x[i] - data_tbl$x[j]) + 1) *
      (abs(data_tbl$y[i] - data_tbl$y[j] + 1))
    max_area <- max(area, max_area)
  }
}
max_area
tictoc::toc()
# 30 minutes to solve
# 0.6 sec to run

#Part 2
