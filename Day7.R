library(tidyverse)

#Sample Data
data <- c(
  ".......S.......",
  "...............",
  ".......^.......",
  "...............",
  "......^.^......",
  "...............",
  ".....^.^.^.....",
  "...............",
  "....^.^...^....",
  "...............",
  "...^.^...^.^...",
  "...............",
  "..^...^.....^..",
  "...............",
  ".^.^.^.^.^...^.",
  "..............."
)

#Actual Data
# data <- read_lines("Day7")

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
position <- which(mat_data == "S", arr.ind = TRUE)

path <- c()
if (mat_data[position] %in% c("S", "|")) {
  position <- position + c(1, 0)
} else if (mat_data[position] == "^") {}
