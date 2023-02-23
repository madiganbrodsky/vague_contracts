library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("../../helpers.R")

# load core dataset
d <- read_csv("../../../results/1_falseconsensus/main-merged.csv")

# load extra information
metadata = read_csv("stims-structureannotation.csv") %>% 
  select(Title, version,header_passive, header_sentence_length) %>% 
  rename(title=Title)

d = d %>% 
  left_join(metadata,by=c("title","version"))

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
  select(workerid,title,version,individual_judgment,population_judgment,confidence,header_passive, header_sentence_length) %>%
  group_by(version,title) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,title,individual_judgment,header_passive, header_sentence_length) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100,
         agreement_mean = mean(population_judgment)) %>% 
  mutate(SentenceType = ifelse(header_passive == 1, 
                               "passive", 
                               "not passive")) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion) %>%
  mutate(versionPretty = factor(version))


levels(transformed$versionPretty)
levels(transformed$versionPretty) <- c("Controversial", "Covered", "Not\ncovered")


#plot histogram of proportion of yes responses for passive vs not passive definitions for all three conditions 

props = transformed %>% 
  mutate(Yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         No = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0)) %>% 
  group_by(version, SentenceType) %>% 
  summarise(ProportionYes = mean(Yes), YesCILow=ci.low(Yes), YesCIHigh=ci.high(Yes),
            ProportionNo = mean(No), NoCILow=ci.low(No), NoCIHigh=ci.high(No)) %>% 
  ungroup() %>% 
  mutate(YesYMin=ProportionYes-YesCILow,YesYMax=ProportionYes+YesCIHigh,
         NoYMin=ProportionNo-NoCILow,NoYMax=ProportionNo+NoCIHigh)

yes = props %>% 
  select(version,ProportionYes,YesYMax,YesYMin, SentenceType) %>% 
  rename(Proportion=ProportionYes,YMax=YesYMax,YMin=YesYMin) %>% 
  mutate(Response="yes",ResponseType="individual")

responses = bind_rows(yes) %>% 
  mutate(Condition = fct_relevel(version,"unambiguous_covered","controversial"),
         Response = fct_relevel(Response,"yes","no")) 

dodge=position_dodge(.9)

ggplot(responses, aes(x=Condition,y=Proportion,fill=SentenceType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) +
  scale_fill_manual(values=cbPalette,labels=c("unambiguous_covered"="covered","unambiguous_uncovered"="not covered")) +
  ylab("Proportion of yes responses") +
  theme(legend.position="top",legend.direction="vertical")
ggsave("graphs/sentencetypeprop.pdf",width=5,height=3.5)

#bar plot of error levels for sentence types in all three conditions

transformed_meanError_byResponse = transformed %>% 
  group_by(SentenceType, version, individual_judgment) %>%
  summarise(meanError = mean(difference), 
            ciLowError = ci.low(difference),
            ciHighError = ci.high(difference))

ggplot(transformed_meanError_byResponse, aes(x=individual_judgment, y = meanError, fill = SentenceType)) +
  facet_wrap(~version) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=meanError - ciLowError, ymax=meanError + ciHighError),width=.2,position=dodge) +
  # scale_fill_manual(values=cbPalette,labels=c("unambiguous_covered"="covered","unambiguous_uncovered"="not covered"))  +
  ylab("Difference in Individual\nObservation and Population Judgements") +
  # xlab("Condition") +
  theme(legend.position="top")
ggsave("graphs/sentencetypeerror_byresponse.pdf",width=5,height=3.5)

transformed_meanError = transformed %>% 
  group_by(SentenceType, version) %>%
  summarise(meanError = mean(difference), 
            ciLowError = ci.low(difference),
            ciHighError = ci.high(difference))

ggplot(transformed_meanError, aes(x=version, y = meanError, fill = SentenceType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=meanError - ciLowError, ymax=meanError + ciHighError),width=.2,position=dodge) +
  # scale_fill_manual(values=cbPalette,labels=c("unambiguous_covered"="covered","unambiguous_uncovered"="not covered"))  +
  ylab("Difference in Individual\nObservation and Population Judgements") +
  # xlab("Condition") +
  theme(legend.position="top")
ggsave("graphs/sentencetypeerror.pdf",width=5,height=3.5)

d_yes_byitem = unique(transformed[,c("true_proportion","ci_low","ci_high","title","version","individual_judgment","SentenceType")]) %>% 
  filter(individual_judgment=="yes") %>% 
  droplevels()

as_tibble(d_yes_byitem) %>% 
  mutate(Condition = fct_relevel(as.factor(version),"unambiguous_covered","controversial")) %>% 
  # mutate(title=fct_reorder(as.factor(title),as.factor(locus_of_uncertainty_location),.fun = identity)) %>% 
  ggplot(aes(x=Condition,y=true_proportion/100,color=SentenceType)) +
  geom_point() +
  geom_errorbar(aes(ymin=ci_low/100,ymax=ci_high/100),width=.1) +
  geom_line(aes(group=1)) +
  ylab("Proportion of 'yes' judgments") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2),labels=c("unambiguous_covered" = "covered", "unambiguous_uncovered" = "not covered",
                                                            "controversial" = "controversial")) +
  scale_color_manual(values=cbPalette[5:7],name="Definition type",labels=c("definition_exh"="exhaustive","definition_incl"="inclusion","exclusion"="exclusion")) +
  facet_wrap(~title,labeller = labeller(title = label_wrap_gen(width = 20))) +
  theme(legend.position = c(0.8, 0.05),legend.direction="horizontal")

ggsave("graphs/prop_yes_byitem.pdf",width=12,height=10)

#scatter plot of error levels for sentence length in all three conditions

transformed = transformed %>% 
  mutate(abs_diff = abs(difference))
transformed <- transformed[order(transformed$title),]
transformed <- transformed[!duplicated(transformed$title)]
ggplot(transformed, aes(x=header_sentence_length, y = abs_diff))+
  geom_point()+
  ylab("Difference in Individual Observation and Population Judgements") +
  xlab("Sentence Lenglth") +
  theme(legend.position="top",legend.direction="vertical")
ggsave("graphs/sentencelengtherror.pdf",width=5,height=3.5)




