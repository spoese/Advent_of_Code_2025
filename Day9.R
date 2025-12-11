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
# #Plan of attack:
# #-Create map of red/green
# #-For each pair of points
# #--Check if edge lines are only red/green
# #--If yes, calculate area
# #--Keep max area

# tictoc::tic()
# data_tbl <- data |>
#   str_split(",", simplify = TRUE) |>
#   as.data.frame()
# names(data_tbl) <- c("x", "y")
# data_tbl <- data_tbl |>
#   tibble() |>
#   mutate(across(everything(), as.numeric))

# data_grid <- matrix("x", nrow = max(data_tbl$y), ncol = max(data_tbl$x))

# data_grid[cbind(data_tbl$y, data_tbl$x)] <- "r"

# for (i in 1:nrow(data_grid)) {
#   gc <- FALSE
#   for (j in 1:ncol(data_grid)) {
#     if (gc & data_grid[i, j] != "r") {
#       data_grid[i, j] <- "g"
#     }
#     if (data_grid[i, j] == "r") {
#       gc <- !gc
#     }
#   }
# }

# for (j in 1:ncol(data_grid)) {
#   gc <- FALSE
#   for (i in 1:nrow(data_grid)) {
#     if (gc & data_grid[i, j] != "r") {
#       data_grid[i, j] <- "g"
#     }
#     if (data_grid[i, j] == "r") {
#       gc <- !gc
#     }
#   }
# }
# outline <- data_grid

# data_grid <- outline
# for (j in 1:ncol(data_grid)) {
#   gc <- FALSE
#   for (i in 1:nrow(data_grid)) {
#     if (gc & !(data_grid[i, j] %in% c("r", "g"))) {
#       data_grid[i, j] <- "o"
#     }
#     if (data_grid[i, j] %in% c("r", "g")) {
#       if (i == 1) {
#         gc <- !gc
#       } else if (!(data_grid[i - 1, j] %in% c("r", "g"))) {
#         gc <- !gc
#       }
#     }
#   }
# }

# max_area <- 0
# for (i in 1:(nrow(data_tbl) - 1)) {
#   for (j in i:nrow(data_tbl)) {
#     x <- seq(
#       min(data_tbl$x[i], data_tbl$x[j]),
#       max(data_tbl$x[i], data_tbl$x[j])
#     )
#     y <- seq(
#       min(data_tbl$y[i], data_tbl$y[j]),
#       max(data_tbl$y[i], data_tbl$y[j])
#     )
#     edge_vals <- c(
#       data_grid[data_tbl$y[i], x],
#       data_grid[data_tbl$y[j], x],
#       data_grid[y, data_tbl$x[i]],
#       data_grid[y, data_tbl$x[j]]
#     )
#     if (any(edge_vals == "x")) {
#       next
#     }
#     area <- (abs(data_tbl$x[i] - data_tbl$x[j]) + 1) *
#       (abs(data_tbl$y[i] - data_tbl$y[j] + 1))
#     max_area <- max(area, max_area)
#   }
# }
# max_area

#Plan of attack:
#-Create list of edges
#-For each pair of points
#--Check if non-edge of rectangle created by points is in edge list
#--If no, calculate area
data_tbl <- data |>
  str_split(",", simplify = TRUE) |>
  as.data.frame()
names(data_tbl) <- c("x", "y")
data_tbl <- data_tbl |>
  tibble() |>
  mutate(across(everything(), as.numeric))

edges <- tibble(x = integer(), y = integer())
for (i in 1:nrow(data_tbl)) {
  if (i == nrow(data_tbl)) {
    if (data_tbl$x[i] == data_tbl$x[1]) {
      edges <- bind_rows(
        edges,
        tibble(
          x = data_tbl$x[i],
          y = seq(
            min(data_tbl$y[i], data_tbl$y[1]),
            max(data_tbl$y[i], data_tbl$y[1])
          )
        )
      )
    } else {
      edges <- bind_rows(
        edges,
        tibble(
          x = seq(
            min(data_tbl$x[i], data_tbl$x[1]),
            max(data_tbl$x[i], data_tbl$x[1])
          ),
          y = data_tbl$y[i]
        )
      )
    }
  } else if (data_tbl$x[i] == data_tbl$x[i + 1]) {
    edges <- bind_rows(
      edges,
      tibble(
        x = data_tbl$x[i],
        y = seq(
          min(data_tbl$y[i], data_tbl$y[i + 1]),
          max(data_tbl$y[i], data_tbl$y[i + 1])
        )
      )
    )
  } else {
    edges <- bind_rows(
      edges,
      tibble(
        x = seq(
          min(data_tbl$x[i], data_tbl$x[i + 1]),
          max(data_tbl$x[i], data_tbl$x[i + 1])
        ),
        y = data_tbl$y[i]
      )
    )
  }
}
edges <- distinct(edges)

tictoc::toc()
