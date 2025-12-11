library(tidyverse)

#Sample Data
# data <- c(
#   ".......S.......",
#   "...............",
#   ".......^.......",
#   "...............",
#   "......^.^......",
#   "...............",
#   ".....^.^.^.....",
#   "...............",
#   "....^.^...^....",
#   "...............",
#   "...^.^...^.^...",
#   "...............",
#   "..^...^.....^..",
#   "...............",
#   ".^.^.^.^.^...^.",
#   "..............."
# )

#Actual Data
data <- read_lines("Day7")
tictoc::tic()
#Part 1
mat_data <- data |>
  str_split("") |>
  unlist() |>
  matrix(nrow = length(data), byrow = TRUE)

mat_data[2, which(mat_data == "S", arr.ind = TRUE)[[2]]] <- "|"

splits <- 0
for (i in seq(3, nrow(mat_data) - 1, by = 2)) {
  for (j in 1:ncol(mat_data)) {
    if (mat_data[i - 1, j] == "|") {
      if (mat_data[i, j] == "^") {
        mat_data[i, j - 1] <- "|"
        mat_data[i, j + 1] <- "|"
        mat_data[i + 1, j - 1] <- "|"
        mat_data[i + 1, j + 1] <- "|"
        splits <- splits + 1
      } else if (mat_data[i, j] == ".") {
        mat_data[i, j] <- "|"
        mat_data[i + 1, j] <- "|"
      }
    }
  }
}
splits

#Part 2

mat_data[which(mat_data == "^", arr.ind = TRUE)] <- "0"
mat_data <- rbind(mat_data, rep(".", ncol(mat_data)))
mat_data[nrow(mat_data), which(mat_data[nrow(mat_data) - 1, ] == "|")] <- "0"
position <- which(mat_data == "S", arr.ind = TRUE) + c(2, 0)
mat_data[position] <- as.numeric(mat_data[position]) + 1

for (i in 3:(nrow(mat_data) - 2)) {
  for (j in 2:(ncol(mat_data) - 1)) {
    if (!is.na(as.numeric(mat_data[i, j]))) {
      #Go left
      left_col <- mat_data[i:nrow(mat_data), j - 1]
      left_stop <- min(which(!is.na(as.numeric(left_col))))
      left_index <- i + left_stop - 1
      mat_data[left_index, j - 1] <- as.numeric(mat_data[left_index, j - 1]) +
        as.numeric(mat_data[i, j])
      #Go right
      right_col <- mat_data[i:nrow(mat_data), j + 1]
      right_stop <- min(which(!is.na(as.numeric(right_col))))
      right_index <- i + right_stop - 1
      mat_data[right_index, j + 1] <- as.numeric(mat_data[right_index, j + 1]) +
        as.numeric(mat_data[i, j])
    }
  }
}
mat_data
format(
  sum(as.numeric(mat_data[nrow(mat_data), ]), na.rm = TRUE),
  scientific = FALSE
)
tictoc::toc()
