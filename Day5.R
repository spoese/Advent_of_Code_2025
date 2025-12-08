library(tidyverse)

#Sample data
# data <- c("3-5", "10-14", "16-20", "12-18", "", "1", "5", "8", "11", "17", "32")

#Real data
data <- read_lines("Day5")

bkpt <- which(data == "")
ranges <- data[1:(bkpt - 1)]
ids <- data[(bkpt + 1):length(data)]

#Part 1
count <- 0
for (j in 1:length(ids)) {
    for (i in 1:length(ranges)) {
        min_check <- as.numeric(str_split(ranges[i], "-")[[1]][1])
        max_check <- as.numeric(str_split(ranges[i], "-")[[1]][2])
        if (as.numeric(ids[j]) >= min_check & as.numeric(ids[j]) <= max_check) {
            count <- count + 1
            break
        }
    }
}
count

#Part 2

intervals <- tibble(
    range = ranges,
    min = as.numeric(sapply(str_split(ranges, "-"), "[", 1)),
    max = as.numeric(sapply(str_split(ranges, "-"), "[", 2))
) |>
    rowwise() |>
    mutate(int = list(sets::interval(min, max))) |>
    pull(int) |>
    sets::interval_union()

count <- 0
for (i in 1:length(intervals)) {
    count <- count + (max(intervals[i]) - min(intervals[i]) + 1)
}
format(count, scientific = FALSE)
