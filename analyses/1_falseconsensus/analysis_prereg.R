library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read_csv("../../proliferate/1_falseconsensus/main-merged.csv")

# EXCLUSIONS

excludedWorkers <- (d %>%
                      filter((version == "unambiguous_uncovered" & individual_judgment == "yes") |
                               (version == "unambgiuous_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 1))$workerid

d <- d %>% 
  filter(!(workerid %in% excludedWorkers))

# DATA TRANSFORMATIONS

transformed <- d %>%
  select(workerid,title,version,individual_judgment,population_judgment,confidence) %>%
  group_by(version,title) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,title,individual_judgment) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion)

# PLOT (HISTOGRAM OF DIFFERENCES BETWEEN POPULATION ESTIMATE AND TRUE PROPORTION)

transformed %>%
  filter(version == "controversial") %>%
  ggplot(aes(x = difference)) +
  geom_histogram()

# PLOT (BY-ITEM, BY-CONDITION)

transformed %>% 
  ggplot(aes(x = individual_judgment, y = as.numeric(population_judgment))) +
  facet_wrap(version ~ title) +
  geom_point(alpha = 0.5, show.legend = FALSE, aes(colour = individual_judgment),
             position = position_nudge(x = +0.1)) +
  theme_bw() +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, colour = "red",
               position = position_nudge(x = +0.1)) + 
  stat_summary(fun = mean, shape = 18, size = 1, geom = "point", colour = "red",
               position = position_nudge(x = +0.1)) +
  stat_summary(fun=mean, geom="label", aes(label = round(..y..,2), hjust = -0.25),
               position = position_nudge(x = +0.1)) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                width=.1,                    # Width of the error bars
                position=position_nudge(-0.1)) +
  geom_point(aes(y = true_proportion), shape = 18, size = 3, 
             position = position_nudge(-0.1)) +
  geom_text(aes(y = true_proportion, label = round(true_proportion, digits = 2)),
            position=position_nudge(-0.25)) +
  ylab("Distribution of responses (and estimates), %") +
  xlab("Individual judgment") +
  ggtitle("Comparing real vs. estimated consensus")

# FISHER ONE-SAMPLE TEST TO DETERMINE WHETHER (SUBJECTIVE JUDGMENT) - (TRUE PROPORTION) > 0

fisherTest <- oneSamplePermutationTest((transformed %>% filter(version == "controversial"))$difference, 
                                       alternative = "greater", mu = 0, exact = FALSE, 
                         n.permutations = 10000, seed = NULL)
