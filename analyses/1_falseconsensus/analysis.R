library(tidyverse)
library(jsonlite)
library(binom)
library(brms)

## for bootstrapping 95% confidence intervals
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

base = 6
expand = 4
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d_subj <- read_csv("../../../results/pilots/pollution/pollution-subject_information.csv") %>%
  select(workerid, attention, language, gender,comments)

d_data <- read_csv("../../../results/pilots/pollution/pollution-trials.csv") %>%
  filter(!is.na(item)) %>%
  mutate(population_judgment = as.numeric(population_judgment))

d <- d_subj %>%
  bind_cols(d_data) %>%
  mutate(workerid = workerid...1) %>%
  filter(attention == TRUE) %>%
  distinct(workerid, .keep_all = TRUE) %>%
  mutate(version = factor(version))

levels(d$version) <- c("Exclusion", "Inclusion")

d %>%
  group_by(gender,version) %>%
  summarize(n = n())

d %>%
  group_by(prompt) %>%
  summarize(n = n())

d %>%
  group_by(version, gender, individual_judgment) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

transformed_gender_version <- d %>%
  group_by(prompt,gender,version) %>%
  # filter(gender %in% c("Male", "Female")) %>%
  summarize(mean = mean(population_judgment), YMin = mean - ci.low(population_judgment),
            YMax = mean + ci.high(population_judgment), n = n()) 

transformed_gender_version %>% 
  ggplot(aes (x = prompt, y = mean, fill = prompt)) +
  facet_grid(version~gender) +
  geom_bar(stat="identity",position = "dodge") +
  theme_bw() +
  theme(text = element_text(size = base * expand / 2, face = "bold"),
        legend.position = "none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = "dodge") +  
  labs(fill = "Prime type") +
  ylab("Mean population\nconsensus estimate") +
  xlab("Prompt type") +
  scale_x_discrete(labels=c("1000_control" = "Control", "1000_days" = "'Days'",
                            "1000_gender" = "Gender")) +
  ggtitle("Consensus estimates, Solan et al's 'pollution' vignette") +
  geom_label(aes(label = paste("n = ",n)),size = 2)

ggsave("pilot_results_pollution_genderVersion.pdf", width = 6, height = 3, units = "in")

transformed_ag <- d %>%
  group_by(prompt) %>%
  summarize(mean = mean(population_judgment), YMin = mean - ci.low(population_judgment),
            YMax = mean + ci.high(population_judgment), n = n()) 

transformed_ag %>% 
  ggplot(aes (x = prompt, y = mean, fill = prompt)) +
  geom_bar(stat="identity",position = "dodge") +
  theme_bw() +
  theme(text = element_text(size = base * expand / 2, face = "bold"),
        legend.position = "none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = "dodge") +  
  labs(fill = "Prime type") +
  ylab("Mean population\nconsensus estimate") +
  xlab("Prompt type") +
  scale_x_discrete(labels=c("1000_control" = "Control", "1000_days" = "'Days'",
                            "1000_gender" = "Gender")) +
  ggtitle("Consensus estimates, Solan et al's 'pollution' vignette") +
  geom_label(aes(label = paste("n = ",n)),size = 2)

ggsave("pilot_results_pollution.pdf", width = 6, height = 3, units = "in")

m <- brm(population_judgment ~ actual + (individual_judgment|item), data = transformed,
         control = list(adapt_delta = 0.9))

summary(m)

hypothesis(m, "actual > 0")
