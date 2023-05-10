library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("/Users/madiganbrodsky/Desktop/legal_interpretation/vague_contracts/analyses/helpers.R")

# load core dataset
d <- read_csv("/Users/madiganbrodsky/Desktop/legal_interpretation/vague_contracts/results/1_falseconsensus/main-merged.csv")

# load extra information
metadata = read_csv("/Users/madiganbrodsky/Desktop/legal_interpretation/vague_contracts/experiments/1_falseconsensus/main/stimuli.csv") %>% 
  select(Title, version, locus_of_uncertainty, locus_of_uncertainty_location,name_gender) %>% 
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
  select(workerid,title,version,individual_judgment,population_judgment,confidence,locus_of_uncertainty,locus_of_uncertainty_location,name_gender) %>%
  group_by(version,title) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,title,individual_judgment,locus_of_uncertainty_location,name_gender) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100,
         agreement_mean = mean(population_judgment)) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion) %>%
  mutate(versionPretty = factor(version))

levels(transformed$versionPretty)
levels(transformed$versionPretty) <- c("Controversial", "Covered", "Not\ncovered")

# how many of each type of item
items = unique(transformed[,c("locus_of_uncertainty_location","title")])
table(items$locus_of_uncertainty_location)
#  definition_exh definition_incl       exclusion 
#         35               3               8 

new <- transformed %>%
  filter(version == "controversial") %>% #%>% 
  group_by(title) %>% ## 'title' is added for posthoc analysis
  mutate(yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         no = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0),
         cantdecide = case_when(individual_judgment == "cantdecide" ~ 1,
                                TRUE ~ 0)) %>%
  summarise(nYes = sum(yes), nNo =sum(no), nCantDecide = sum(cantdecide), 
            propYes = sum(yes)/n(), propNo = sum(no)/n(), propCantDecide = sum(cantdecide)/n(), meanError = mean(difference)) %>%
  as.data.frame() %>%  
  arrange(propYes) %>% 
  mutate(title = replace(title, title == 'Goods Carrying Vehicle: Loss or Damage to Vehicle', 'Goods Carrying Vehicle')) %>% 
  filter(grepl('Escape of Oil II|Vehicle Glass II|Emergency Damages I|Malicious Acts or Vandalism II|Ground Heave I|Storm Damage|Vehicle Fire III|House Removal I|General Damages|Escape of Water I|Trace and Access I', title)) #REMOVE THIS FILTER TO SEE PLOT OF ALL ITEMS!!!


#Turn your 'title' column into a character vector
new$title <- as.character(new$title)
#Then turn it back into a factor with the levels in the correct order
new$title <- factor(new$title, levels=unique(new$title))


#################################################################################################

#PLOT #1: BY-ITEM / PROP YES FOR CONTROVERSIAL STIMS 

ggplot(data = new, 
       mapping = aes(x = title, 
                     y = propYes)) + 
  geom_point(alpha = 0.8, 
             color = 'brown2') + 
  geom_hline(yintercept=0.5,linetype=2)+
  xlab("Item Name") +
  ylab("Proportion of Yes Responses") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Proportion of Yes Responses for Controversial Items") 


#PLOT #2: PROP YES / ERROR FOR CONTROVERSIAL STIMS 

ggplot(data = new, 
       mapping = aes(x = propYes, 
                     y = meanError)) +
  geom_point(alpha = 0.8,
             color = 'darkmagenta') +
  # geom_jitter(alpha = 0.8, 
  #             color = 'darkmagenta')+ 
  xlab("Proportion of Yes Responses") +
  ylab("Mean Error (Individual Estimate - Actual Judgement)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Relationship between the proportion of 'yes' responses and estimation error for controversial items ") 


  
