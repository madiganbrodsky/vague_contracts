library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

source("../helpers.r")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

time <- read_csv("../../proliferate/1_falseconsensus/pilot_compare_dispute-time_in_minutes.csv")

time_summary <- inner_join(time %>% select(workerid, time_in_minutes), d %>% select(workerid, dispute),
           by = "workerid") %>%
  group_by(dispute) %>%
  summarise(completion_time = median(time_in_minutes))

d <- read_csv("../../results/1_falseconsensus/pilot_compare_dispute-merged.csv")

d_summary <- d %>%
  group_by(dispute, individual_judgment, version) %>%
  summarise(n = n(), population_estimate_mean = mean(population_judgment), 
            population_estimate_cilow = ci.low(population_judgment),
            population_estimate_cihigh = ci.high(population_judgment))

ggplot(d_summary, aes(x = individual_judgment, y = n, fill = version)) +
  facet_wrap(~dispute) +
  geom_bar(stat = "identity", position = position_dodge())

ggplot(d_summary, aes(x = individual_judgment, y = population_estimate_mean, fill = version)) +
  facet_wrap(~dispute) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=population_estimate_mean - population_estimate_cilow, ymax=population_estimate_mean + population_estimate_cihigh),
                width=.25, position = position_dodge())


# # # # EARLIER ANALYSES

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

# EXPLORATORY REGRESSION ANALYSIS - DOES FCB DEPEND ON JUDGMENT PROVIDED BY PARTICIPANT?

options(mc.cores = parallel::detectCores())
model <- brm(difference ~ individual_judgment + # individual_judgment: 'yes', 'no', 'can't decide' (reference level)
               (1|workerid) + (individual_judgment|title), 
             data = transformed %>% filter(version == "controversial"))
summary(model)

### **Auxilliary visualizations:**
  
d %>% filter(!(version %in% c("unambiguous_uncovered", "unambiguous_covered"))) %>%
  mutate(confidence = factor(confidence)) %>%
  mutate(confidence = plyr::revalue(confidence, c('0' = "Not at all confident", 
                                                  '1'  = "Slightly confident", 
                                                  '2'  = "Moderately confident",
                                                  '3' = "Very confident",
                                                  '4' = "Totally confident"))) %>%
  ggplot(aes(x = confidence)) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() +
  xlab("Confidence in answer") +
  ylab("Count") + 
  ggtitle("Answer confidence (excluding controls)")

