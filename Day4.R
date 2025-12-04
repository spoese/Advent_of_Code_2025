library(tidyverse)

#Sample File
# rolls <- c(
#   "..@@.@@@@.",
#   "@@@.@.@.@@",
#   "@@@@@.@.@@",
#   "@.@@@@..@.",
#   "@@.@@@@.@@",
#   ".@@@@@@@.@",
#   ".@.@.@.@@@",
#   "@.@@@.@@@@",
#   ".@@@@@@@@.",
#   "@.@.@@@.@."
# ) |>
#   str_split("")

#Actual File
rolls <- read_lines("Day4") |>
  str_split("")

rolls <- do.call(rbind, rolls)

#Part 1
access <- 0
for (i in 1:nrow(rolls)) {
  for (j in 1:ncol(rolls)) {
    if (rolls[i, j] == "@") {
      nrolls <- 0
      for (k in max(1, i - 1):min(nrow(rolls), i + 1)) {
        for (l in max(1, j - 1):min(ncol(rolls), j + 1)) {
          if (rolls[k, l] == "@" && !(k == i && l == j)) {
            nrolls <- nrolls + 1
            # print(paste("Found roll at", i, j, "with neighbor at", k, l))
          }
        }
      }
      if (nrolls < 4) {
        # rolls[i, j] <- "X"
        access <- access + 1
        # print(paste("Access granted at", i, j))
      }
    }
  }
}
access
