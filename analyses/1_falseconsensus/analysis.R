library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

d <- read_csv("../../results/1_falseconsensus/main-merged.csv")
source("../helpers.R")


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

# NOTE: THIS TAKES A SUPER LONG TIME TO RENDER ON THE WHOLE DATASET...
# RUN AT YOUR OWN RISK

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

# 
props = transformed %>% 
  mutate(Yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         No = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0),
         CantDecide = case_when(individual_judgment == "cantdecide" ~ 1,
                                  TRUE ~ 0)) %>% 
  group_by(version) %>% 
  summarise(ProportionYes = mean(Yes), YesCILow=ci.low(Yes), YesCIHigh=ci.high(Yes),
            ProportionNo = mean(No), NoCILow=ci.low(No), NoCIHigh=ci.high(No),
            ProportionCantDecide = mean(CantDecide), CantDecideCILow=ci.low(CantDecide), CantDecideCIHigh=ci.high(CantDecide)) %>% 
  ungroup() %>% 
  mutate(YesYMin=ProportionYes-YesCILow,YesYMax=ProportionYes+YesCIHigh,
         NoYMin=ProportionNo-NoCILow,NoYMax=ProportionNo+NoCIHigh,
         CantDecideYMin=ProportionCantDecide-CantDecideCILow,CantDecideYMax=ProportionCantDecide+CantDecideCIHigh)

yes = props %>% 
  select(version,ProportionYes,YesYMax,YesYMin) %>% 
  rename(Proportion=ProportionYes,YMax=YesYMax,YMin=YesYMin) %>% 
  mutate(Response="yes",ResponseType="individual")

no = props %>% 
  select(version,ProportionNo,NoYMax,NoYMin) %>% 
  rename(Proportion=ProportionNo,YMax=NoYMax,YMin=NoYMin) %>% 
  mutate(Response="no",ResponseType="individual")

cantdecide = props %>% 
  select(version,ProportionCantDecide,CantDecideYMax,CantDecideYMin) %>% 
  rename(Proportion=ProportionCantDecide,YMax=CantDecideYMax,YMin=CantDecideYMin) %>% 
  mutate(Response="cantdecide",ResponseType="individual")

pop_judgments = transformed %>% 
  group_by(version,individual_judgment) %>% 
  summarise(Proportion = mean(population_judgment/100), CILow=ci.low(population_judgment/100), CIHigh=ci.high(population_judgment/100)) %>% 
  ungroup() %>% 
  mutate(Response=individual_judgment,ResponseType="agreement_estimate", YMin=Proportion-CILow,YMax=Proportion+CIHigh) %>% 
  select(-individual_judgment)

responses = bind_rows(yes,no,cantdecide,pop_judgments) %>% 
  mutate(Condition = fct_relevel(version,"unambiguous_covered","controversial"),
         ResponseType = fct_relevel(ResponseType,"individual")) 

dodge=position_dodge(.9)

ggplot(responses, aes(x=Condition,y=Proportion,fill=Response,alpha=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) +
  scale_fill_manual(values=cbPalette) +
  scale_alpha_discrete(range = c(1, .4)) +
  ylab("Proportion of responses")

names(transformed)
nrow(transformed)

# plot just histograms of "yes" response proportions
unique(transformed[,c("true_proportion","title","version","individual_judgment")]) %>% 
  filter(individual_judgment=="yes") %>% 
  # droplevels() %>% 
  # mutate(Condition = fct_relevel(as.factor(version),"unambiguous_covered","controversial")) %>% 
  ggplot(aes(x=true_proportion)) +
  geom_histogram() +
  # geom_density() +
  facet_wrap(~version)

# plot proportions of "yes" responses by condition and item
unique(transformed[,c("true_proportion","title","version","individual_judgment")]) %>% 
  filter(individual_judgment=="yes") %>% 
  ggplot(aes(x=version,y=true_proportion)) +
  geom_point() +
  geom_line(aes(group=1)) +
  facet_wrap(~title)
