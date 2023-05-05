library(tidyverse)
library(jsonlite)

stims <- toJSON(read.csv("stimuli.csv"))

write_file(stims, "stimuli.json")






