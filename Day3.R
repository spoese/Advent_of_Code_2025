library(tidyverse)

# Sample data
# batteries <- c(
#   "987654321111111",
#   "811111111111119",
#   "234234234234278",
#   "818181911112111"
# )

# Actual data
batteries <- read_lines("Day3")

# Part 1

sum_jolts <- 0
for (i in 1:length(batteries)) {
  temp <- as.numeric(unlist(str_split(batteries[i], "")))
  max_val <- max(temp[-length(temp)])
  max_idx <- min(which(temp == max_val))
  temp2 <- temp[(max_idx + 1):length(temp)]
  max_val2 <- max(temp2)
  jolt <- as.numeric(paste0(max_val, max_val2))
  sum_jolts <- sum(sum_jolts, jolt)
}
sum_jolts
