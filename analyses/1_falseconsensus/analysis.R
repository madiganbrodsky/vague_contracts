library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read_csv("../../results/1_falseconsensus/main-merged.csv")

# NATIVE LANGUAGE EXCLUSIONS

nonEnglishNativeLanguages <- c("Chinese", "Spanish", "spanish", "Punjabi", "Filipino", "Urdu")

excludedWorkers_nonNative <- unique((d %>%
  filter(subject_information.language %in% nonEnglishNativeLanguages))$workerid)

# EXCLUSIONS

excludedWorkers <- (d %>%
                      filter((version == "unambiguous_uncovered" & individual_judgment == "yes") |
                               (version == "unambiguous_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 1))$workerid

d <- d %>% 
  filter(!(workerid %in% excludedWorkers)) %>%
  filter(!(workerid %in% excludedWorkers_nonNative))

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
  mutate(difference = as.numeric(population_judgment) - true_proportion) %>%
  mutate(versionPretty = factor(version))

levels(transformed$versionPretty)
levels(transformed$versionPretty) <- c("Controversial", "Covered", "Not\ncovered")

transformed %>%
  group_by(version) %>%
  mutate(yes = case_when(individual_judgment == "yes" ~ 1,
                                      TRUE ~ 0),
            no = case_when(individual_judgment == "no" ~ 1,
                            TRUE ~ 0),
            cantdecide = case_when(individual_judgment == "cantdecide" ~ 1,
                                    TRUE ~ 0)) %>%
  summarise(nYes = sum(yes), nNo =sum(no), nCantDecide = sum(cantdecide),
    propYes = sum(yes)/n(), propNo = sum(no)/n(), propCantDecide = sum(cantdecide)/n())

# transformed %>% 
#   filter(title == "Wind Damage" & version == "controversial") %>%
#   group_by(individual_judgment) %>%
#   summarise(meanjudgment = mean(population_judgment)) 

# PLOT (HISTOGRAM OF DIFFERENCES BETWEEN POPULATION ESTIMATE AND TRUE PROPORTION)

transformed %>%
  ggplot(aes(x = difference, fill = versionPretty)) +
  geom_histogram(position="identity", alpha = 0.3) +
  xlab("Participant error") +
  ylab("Count") +
  labs(fill='Condition') +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size=8)) + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

ggsave("viz/errorHist.pdf", width = 3, height = 3, units = "in")

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
