# WOA graph for experiment 2a
source('scripts_and_filters/general_setup.R')
library(BayesFactor)
library(magrittr)

# Experiment 7 (02-02-behavioural-experiments.R)
select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'confidenceExploration', version == 'v0-0-10')
)

AdvisedTrial <- annotate_responses(AdvisedTrial)
Trial <- annotate_responses(Trial)

nMaxOutliers <- 0
maxTrialRT <- 60000
minChangeRate = .1     ## some advice taken on 10%+ of trials

exclusions <- AdvisedTrial %>% 
  nest(d = -pid) %>%
  mutate(
    `Multiple attempts` = map_lgl(
      pid,
      ~ {
        h <- unique(AdvisedTrial$pidHash[AdvisedTrial$pid == .])
        p <- unique(AdvisedTrial$pid[AdvisedTrial$pidHash == h])
        length(p) > 1
      }
    ),
    `Not enough changes` = map_lgl(
      d,
      ~ {
        diff <- mutate(
          .,
          diff = responseConfidence != responseConfidenceFinal,
          diff = if_else(is.na(diff), F, diff)
        ) %>%
          pull(diff) %>% 
          mean() 
        diff < minChangeRate
      }
    )
  ) %>% 
  select(-d)

do_exclusions(exclusions)

nMaxTrials <- AdvisedTrial %>% 
  filter(block %in% c(3, 5)) %>%
  group_by(pid) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  pull(n) %>% 
  max()

AdvisedTrial <- AdvisedTrial %>% filter(timeEnd < maxTrialRT)

exclusions <- exclusions %>% 
  mutate(
    `Too many outlying trials` = 
      map_lgl(pid, ~ AdvisedTrial %>% 
                filter(pid == .x, block %in% c(3, 5)) %>%
                summarise(n = n(), .groups = 'drop') %>% 
                pull(n) < (nMaxTrials - nMaxOutliers)),
    # if participants were already dropped don't mark them here
    `Too many outlying trials` = if_else(!(pid %in% AdvisedTrial$pid),
                                         F, `Too many outlying trials`)
  )

do_exclusions(exclusions, backup = F)

tmp <- AdvisedTrial %>%
  filter(!is.na(advisor0idDescription)) %>%
  mutate(
    Advisor = advisor_description_name(advisor0idDescription),
    Advisor = factor(Advisor),
    Feedback = if_else(feedback, "Feedback", "No feedback")
  ) %>%
  order_factors()

Familiarisation <- tmp %>% filter(feedback)
Test <- tmp %>% filter(!feedback)


adv_inf <- bind_rows(
  mutate(Familiarisation, Type = "Familiarisation"), 
  mutate(Test, Type = "Test")
) %>%
  group_by(pid, Advisor, Type) %>%
  select(c(matches('(influence|WOA)'), group_vars(.))) %>%
  summarise_all(mean)

bf <- adv_inf %>% 
  nest(d = -Type) %>%
  mutate(
    bf = map(d, ~ select(., pid, Advisor, advisor0Influence) %>%
               pivot_wider(names_from = Advisor, 
                           values_from = advisor0Influence) %>%
               filter_if(is.numeric, ~ !is.na(.)) %>%
               as.data.frame() %$%
               ttestBF(x = `Consistent individual`, y = `Group member`, data = ., paired = T))
  ) 

bf$BF <- sapply(1:nrow(bf), function(i) bf2str(exp(bf$bf[[i]]@bayesFactor$bf)))

# Plot graph

dw <- .1

adv_inf %>%
  mutate(
    `Trial type` = Type,
    Advisor = if_else(str_detect(Advisor, "Group"), "Multiple", "Single")
  ) %>%
  ggplot(aes(x = Advisor, y = advisor0Influence, 
                    colour = `Trial type`, fill = `Trial type`)) +
  geom_hline(yintercept = 0, colour = 'lightgrey', size = 1) +
  geom_line(aes(group = pid), alpha = .25) +
  geom_split_violin(aes(x = nudge(Advisor, dw),
                        group = Advisor), width = 1,
                    colour = NA) +
  geom_boxplot(outlier.shape = NA, size = 1, width = dw/2,
               aes(x = nudge(Advisor, dw), 
                   group = Advisor),
               colour = 'black') +
  geom_segment(x = 1, xend = 2, y = 1.55, yend = 1.55, colour = 'black') +
  geom_label(y = 1.55, x = 1.5, colour = 'black', fill = 'white', 
             aes(label = paste0('BF = ', BF)), data = bf) +
  scale_y_continuous(limits = c(-.5, 1.6), 
                     breaks = seq(-.5, 1.5, length.out = 6)) +
  facet_grid(~`Trial type`) +
  labs(x = "Advisor presentation", y = 'Weight of Advice') +
  scale_fill_feedback(aesthetics = c('fill', 'colour')) +
  broken_axis_y

source('papers/rational-egocentric-discounting/helpers.R')
save_figure(filename = "./papers/rational-egocentric-discounting/woa-2a-figure.png")
