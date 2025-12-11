library(tidyverse)

#Sample data
# data <- c(
#   "162,817,812",
#   "57,618,57",
#   "906,360,560",
#   "592,479,940",
#   "352,342,300",
#   "466,668,158",
#   "542,29,236",
#   "431,825,988",
#   "739,650,466",
#   "52,470,668",
#   "216,146,977",
#   "819,987,18",
#   "117,168,530",
#   "805,96,715",
#   "346,949,466",
#   "970,615,88",
#   "941,993,340",
#   "862,61,35",
#   "984,92,344",
#   "425,690,689"
# )
# n <- 10

#Actual data
data <- read_lines("Day8")
n <- 1000

#Part 1
data_tbl <- tibble(box_num = 1:length(data), data = data) %>%
  separate(data, into = c("x", "y", "z"), convert = TRUE)

data_dists <- data_tbl |>
  left_join(data_tbl, by = character()) |>
  filter(box_num.x < box_num.y) |>
  mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2 + (z.x - z.y)^2)) |>
  select(box_num.x, box_num.y, dist) |>
  arrange(dist) |>
  head(n)

connections <- list()
for (i in 1:length(data)) {
  connections[[i]] <- filter(data_dists, box_num.x == i | box_num.y == i) %>%
    pivot_longer(cols = -dist, values_to = "box_num") %>%
    filter(box_num != i) %>%
    pull(box_num) |>
    sort()
}

visited <- c()
circuits <- list()
circuit_num <- 1
for (i in 1:length(connections)) {
  if (i %in% visited | length(connections[[i]]) == 0) {
    next
  }
  to_visit <- c(i)
  circuits[[circuit_num]] <- c(i)
  while (length(to_visit) > 0) {
    current <- to_visit[1]
    to_visit <- to_visit[-1]
    if (!(current %in% visited)) {
      visited <- c(visited, current)
      circuits[[circuit_num]] <- unique(c(
        circuits[[circuit_num]],
        current,
        connections[[current]]
      ))
      to_visit <- circuits[[circuit_num]][
        !(circuits[[circuit_num]] %in% visited)
      ]
    }
  }
  circuit_num <- circuit_num + 1
}

circuit_lengths <- lapply(circuits, length) |>
  unlist() |>
  sort(decreasing = TRUE) |>
  head(3)

circuit_lengths |> prod()

#Part 2
