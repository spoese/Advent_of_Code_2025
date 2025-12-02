library(tidyverse)

instructions <- read_lines("Day1_1")

instructions_df <- instructions |>
  as_tibble() |>
  rename(instruction = value) |>
  mutate(
    turn = substr(instruction, 1, 1),
    steps = as.numeric(substr(instruction, 2, nchar(instruction)))
  )

sample <- c("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

sample_df <- sample |>
  as_tibble() |>
  rename(instruction = value) |>
  mutate(
    turn = substr(instruction, 1, 1),
    steps = as.numeric(substr(instruction, 2, nchar(instruction)))
  )

#Part 1

pos <- 50
count <- 0
for (i in 1:nrow(new_instructions_df)) {
  if (new_instructions_df$turn[i] == "L") {
    pos <- (pos - new_instructions_df$steps[i]) %% 100
  } else {
    pos <- (pos + new_instructions_df$steps[i]) %% 100
  }
  if (pos == 0) {
    count <- count + 1
  }
  # print(pos)
}
count

#Part 2
tictoc::tic()
instructions_df[rep(1, each = instructions_df$steps[1]), ]


new_instructions_df <- tibble()
for (i in 1:nrow(instructions_df)) {
  new_instructions_df <- new_instructions_df |>
    bind_rows(
      instructions_df[rep(i, each = instructions_df$steps[i]), ] |>
        mutate(
          steps = 1
        )
    )
}

pos <- 50
count <- 0
for (i in 1:nrow(new_instructions_df)) {
  if (new_instructions_df$turn[i] == "L") {
    pos <- (pos - new_instructions_df$steps[i]) %% 100
  } else {
    pos <- (pos + new_instructions_df$steps[i]) %% 100
  }
  if (pos == 0) {
    count <- count + 1
  }
  # print(pos)
}
count
tictoc::toc()
