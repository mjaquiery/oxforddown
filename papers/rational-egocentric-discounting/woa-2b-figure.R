# WOA graph for Experiment 2b
source('scripts_and_filters/general_setup.R')
library(BayesFactor)
library(magrittr)

# Experiment 8 (02-02-behavioural-experiments.R)
source('scripts_and_filters/general_setup.R')

select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'calibrationKnowledge', version == 'v0-1-3')
)

AdvisedTrial <- AdvisedTrial %>%
  filter(studyId == "calibrationKnowledge") %>%
  annotate_responses()
Trial <- Trial %>%
  filter(studyId == "calibrationKnowledge") %>%
  annotate_responses()

nMinKeyTrials <- 4 * 2 * 2
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
          diff = responseConfidence != responseConfidenceFinal | 
            responseAnswerSide != responseAnswerSideFinal,
          diff = if_else(is.na(diff), F, diff)
        ) %>%
          pull(diff) %>% 
          mean() 
        diff < minChangeRate
      }
    ),
    `Study incomplete` = !map_lgl(pid, ~ . %in% `debrief-form`$pid)
  ) %>% 
  select(-d)

do_exclusions(exclusions)

nMaxTrials <- AdvisedTrial %>% 
  filter(
    block == 4,
    str_detect(advisor0actualType, "agree")
  ) %>%
  group_by(pid) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  pull(n) %>% 
  max()

AdvisedTrial <- AdvisedTrial %>% filter(timeEnd < maxTrialRT)

exclusions <- exclusions %>% 
  mutate(
    `Too many outlying trials` = 
      map_lgl(pid, ~ AdvisedTrial %>% 
                filter(
                  block == 4,
                  str_detect(advisor0actualType, "agree")
                ) %>%
                summarise(n = n(), .groups = 'drop') %>% 
                pull(n) < nMinKeyTrials),
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
    Feedback = if_else(feedback, "Feedback", "No feedback"),
    Hybrid = if_else(
      is.na(advisor0hybridDescriptions),
      "Unambiguous presentation",
      "Hybrid presentation"
    ),
    KeyTrial = str_detect(advisor0actualType, "agree")
  ) %>%
  order_factors()

Familiarisation <- tmp %>% filter(feedback)
Test <- tmp %>% filter(!feedback)

Key <- Test %>% filter(KeyTrial)

adv_inf <- Key %>%
  group_by(pid, Advisor, Hybrid) %>%
  summarise(inf = mean(advisor0InfluenceCapped), .groups = "drop") %>%
  mutate(across(-inf, factor))

bf <- adv_inf %>% 
  nest(d = -Advisor) %>%
  mutate(
    bf = map(d, ~ select(., pid, Hybrid, inf) %>%
               pivot_wider(names_from = Hybrid, 
                           values_from = inf) %>%
               filter_if(is.numeric, ~ !is.na(.)) %>%
               as.data.frame() %$%
               ttestBF(x = `Hybrid presentation`, y = `Unambiguous presentation`, data = ., paired = T))
  ) 

bf$BF <- sapply(1:nrow(bf), function(i) bf2str(exp(bf$bf[[i]]@bayesFactor$bf)))


## Graph

# Merge datasets

# Plot graph

dw <- .1

adv_inf %>% 
  mutate(
    Hybrid = str_extract(Hybrid, "\\w+"),
    Hybrid = if_else(str_detect(Hybrid, "Hybrid"), "Anonymous", "Identified")
  ) %>%
  ggplot(aes(x = Hybrid, y = inf, 
             colour = Advisor, fill = Advisor)) +
  geom_hline(yintercept = 0, colour = 'lightgrey', size = 1) +
  geom_line(aes(group = pid), alpha = .25) +
  geom_split_violin(aes(x = nudge(Hybrid, dw),
                        group = Hybrid), width = 1,
                    colour = NA) +
  geom_boxplot(outlier.shape = NA, size = 1, width = dw/2,
               aes(x = nudge(Hybrid, dw), 
                   group = Hybrid),
               colour = 'black') +
  geom_segment(x = 1, xend = 2, y = 1.25, yend = 1.25, colour = 'black') +
  geom_label(y = 1.25, x = 1.5, colour = 'black', fill = 'white', 
             aes(label = paste0('BF = ', BF)), data = bf) +
  scale_y_continuous(limits = c(-1, 1.5), 
                     breaks = seq(-.5, 1, length.out = 4)) +
  facet_grid(~Advisor) +
  labs(x = 'Advisor presentation', y = 'Weight of Advice') +
  scale_fill_advisor(aesthetics = c('fill', 'colour')) +
  broken_axis_y

source('papers/rational-egocentric-discounting/helpers.R')
save_figure(filename = "./papers/rational-egocentric-discounting/woa-2b-figure.png")
