library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stims_full <- read_csv("stimuli_fullset.csv")

set.seed(1)
random_sample <- sample(unique(stims_full$Title), 8)

stims <- stims_full %>%
  filter(Title %in% random_sample)

write_csv(stims, "stimuli.csv")
