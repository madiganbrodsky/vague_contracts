setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(modelr)
library(viridisLite)

source("../../analyses/helpers.R")

comparison <- read_csv("comparison_3ex_10runs_davinci_batch1.csv") %>%
  rbind(read_csv("comparison_3ex_10runs_davinci_batch2.csv")) %>%
  rbind(read_csv("comparison_3ex_10runs_davinci_batch3.csv")) 

table(comparison$predictions_zeroshot) / nrow(comparison)

table(comparison$predictions_fewshot) / nrow(comparison)

ag_predictions = comparison %>% 
  pivot_longer(cols = c(predictions_fewshot_numeric, predictions_zeroshot_numeric), 
               names_to = "prediction_type",
               values_to = "value") %>%
  group_by(prediction_type,title,version,completion_numeric) %>%
  summarise(predicted_rate = mean(value), cilow_predicted_rate = ci.low(value),
            cihigh_predicted_rate = ci.high(value)) %>%
  ungroup() %>%
  mutate(prediction_type = factor(prediction_type)) %>%
  mutate(version = fct_relevel(version,ref="unambiguous_covered"))

levels(ag_predictions$prediction_type) <- c("Few-shot prompting", "Zero-shot prompting")

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(ag_predictions, aes(x = predicted_rate, y = completion_numeric, color = version)) +
    facet_wrap(~prediction_type) +
    theme_bw() +
    geom_point(alpha = 0.25) +
    theme(legend.position = "bottom",text = element_text(size=12)) +
    scale_color_manual(values=cbPalette,name="Condition",labels=c("unambiguous_covered"="Covered","unambiguous_uncovered"="Not covered", "controversial" = "Controversial")) +
    geom_errorbar(aes(xmin = predicted_rate - cilow_predicted_rate,
                      xmax = predicted_rate + cihigh_predicted_rate),
                  width = 0.5, alpha = 0.25) +
    geom_smooth(method = "lm") +
    theme() +
    xlab("Predicted label") +
    ylab("Gold label") 

ggsave("../../analyses/1_falseconsensus/graphs/results-model.pdf",width=6,height=4)

m <- lm(predicted_rate ~ completion_numeric, data = ag_predictions %>% filter(prediction_type == "Zero-shot prompting"))
summary(m)

m <- lm(predicted_rate ~ completion_numeric, data = ag_predictions %>% filter(prediction_type == "Few-shot prompting"))
summary(m)
