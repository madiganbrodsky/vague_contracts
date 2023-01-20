library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stims <- read_csv("stimuli.csv")
length(unique(stims$Title))
