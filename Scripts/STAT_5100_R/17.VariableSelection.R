library(tidyverse)

# Read in data
surgical <- read_csv("Data_Clean/surgical.csv")

# Randomly select training and test sets
set.seed(1234)
surgical <- surgical |>
  mutate(U = runif(n())) |>
  arrange(U)
train <- surgical[1:72, ] |>
  select(-U)
test <- surgical[73:108, ] |>
  select(-U)

### To be continued...
