library(tidyverse)

# Read sample data
# input_data <- "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

# Read input data
input_data <- read_lines("Day2")

for_use <- tibble(range = unlist(str_split(input_data, ","))) |>
  mutate(
    lower = str_split_i(range, "-", 1) %>% as.numeric(),
    upper = str_split_i(range, "-", 2) %>% as.numeric(),
    lower_length = str_length(lower),
    upper_length = str_length(upper)
  )

sum <- 0
for (i in 1:nrow(for_use)) {
  #Determine all lengths in range that are even
  lengths <- for_use$lower_length[i]:for_use$upper_length[i]
  even_lengths <- lengths[lengths %% 2 == 0]
  #If the number has an even length, check if first half matches second half
  for (j in for_use$lower[i]:for_use$upper[i]) {
    if (str_length(j) %in% even_lengths) {
      if (
        str_sub(j, end = str_length(j) / 2) ==
          str_sub(j, start = str_length(j) / 2 + 1)
      ) {
        sum <- sum + j
      }
    }
  }
}

# Part 2

# sum <- 0
all_ids <- c()
for (i in 1:nrow(for_use)) {
  for (j in for_use$lower[i]:for_use$upper[i]) {
    divisors <- numbers::divisors(str_length(j))
    # invalid_ids <- c()
    for (d in divisors) {
      if (d != 1) {
        segment_length <- str_length(j) / d
        segments <- c()
        for (k in 0:(d - 1)) {
          segments <- c(
            segments,
            str_sub(
              j,
              start = k * segment_length + 1,
              end = (k + 1) * segment_length
            )
          )
        }
        if (length(unique(segments)) == 1) {
          # invalid_ids <- c(invalid_ids, j)
          all_ids <- unique(c(all_ids, j))
        }
        # sum <- sum + sum(unique(invalid_ids))
      }
    }
  }
}
# sum
sum(all_ids)
