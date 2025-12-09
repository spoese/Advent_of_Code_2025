library(tidyverse)

#Sample Data
# data <- c(
#   "123 328  51 64 ",
#   " 45 64  387 23 ",
#   "  6 98  215 314",
#   "*   +   *   +  "
# )

#Actual Data
data <- read_lines("Day6")

#Part 1
formatted_data <- data |>
  str_trim() |>
  str_split("\\s+") |>
  unlist() |>
  matrix(nrow = length(data), byrow = TRUE)

nums <- formatted_data[-nrow(formatted_data), ] |>
  apply(2, as.numeric)

ops <- formatted_data[nrow(formatted_data), ]

results <- c()
for (i in 1:ncol(nums)) {
  if (ops[i] == "+") {
    results[i] <- sum(nums[, i])
  } else if (ops[i] == "*") {
    results[i] <- prod(nums[, i])
  }
}
format(sum(results), scientific = FALSE)


#Part 2
new_data <- str_split(data, "")
all_nums <- new_data[-length(new_data)] |>
  unlist() |>
  matrix(nrow = length(new_data) - 1, byrow = TRUE)
ops_row <- new_data[[length(new_data)]] |>
  unlist()
starts <- which(ops_row != " ")
all_ops <- ops_row[starts]
ends <- c(starts[-1] - 2, length(ops_row))

results <- c()
for (i in 1:length(ends)) {
  nums <- c()
  for (j in ends[i]:starts[i]) {
    nums <- c(nums, as.numeric(paste0(all_nums[, j], collapse = "")))
  }
  if (all_ops[i] == "+") {
    results[i] <- sum(nums)
  } else if (all_ops[i] == "*") {
    results[i] <- prod(nums)
  }
}
format(sum(results), scientific = FALSE)
