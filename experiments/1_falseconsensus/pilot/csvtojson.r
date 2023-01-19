library(tidyverse)
library(jsonlite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stims <- toJSON(read.csv("stimuli.csv"))

write_file(stims, "stimuli.json")

stims_nodispute <- toJSON(read.csv("stimuli_nodispute.csv"))

write_file(stims_nodispute, "stimuli_nodispute.json")


