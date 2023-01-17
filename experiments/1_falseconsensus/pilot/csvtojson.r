library(tidyverse)
library(jsonlite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stims <- toJSON(read.csv("stimuli.csv"))

write_file(stims, "stimuli.json")






