# We want to combine the data for experiments 5 and 6 into a faceted graph.
source('scripts_and_filters/general_setup.R')
library(BayesFactor)
library(magrittr) # for %$% exploding pipe operator

# Experiment 5 (02-02-behavioural-experiments.R)
graph_data <- NULL

select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'directBenevolence', version == 'v3-0-1')
)

## Version of which the analysed version is a replication
.prevStudy <- new.env()
select_experiment(
  'datequiz', 
  function(x) filter(x, study == 'directBenevolence', version == 'v3-0-0'), 
  envir = .prevStudy
)

for (E in c(.GlobalEnv, .prevStudy)) {
  E$AdvisedTrial <- annotate_responses(E$AdvisedTrial)
  E$Trial <- annotate_responses(E$Trial)
}

tmp <- bind_rows(
  mutate(practiceTrial, type = "practice", block = 0),
  mutate(practiceAdvisedTrial, type = "advice_practice", block = 1),
  mutate(Trial, type = "atn_check") %>% filter(!is.na(block)),
  mutate(AdvisedTrial, type = "core") %>% filter(!is.na(block))
) %>%
  group_by(pid, type, block) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type, block) %>%
  summarise(across(-pid, mean), .groups = "drop") %>%
  arrange(block)


nMaxOutliers <- 1
maxTrialRT <- 60000
qqLabelWhitelist = c(  ## Advice questionnaire responses must be one of:
  'Deceptive',
  'Possibly Deceptive',
  'Honest',
  NA
)
minChangeRate = .1     ## some advice taken on 10%+ of trials

for (E in c(.GlobalEnv, .prevStudy)) {
  E$exclusions <- E$AdvisedTrial %>% 
    nest(d = -pid) %>%
    mutate(
      atn_check = map_lgl(
        pid, 
        ~ !all(E$Trial$responseCorrect[E$Trial$pid == .]) |
          any(E$Trial$responseMarkerWidth[E$Trial$pid == .] != 11)
      ),
      `Attention check` = if_else(is.na(atn_check), T, atn_check),
      `Multiple attempts` = map_lgl(
        pid,
        ~ {
          h <- unique(E$AdvisedTrial$pidHash[E$AdvisedTrial$pid == .])
          p <- unique(E$AdvisedTrial$pid[E$AdvisedTrial$pidHash == h])
          length(p) > 1
        }
      ),
      `Missing advice rating` = map_lgl(
        d,
        ~ any(is.na(.$advisor0questionnaireHonestyLabel))
      ),
      `Odd advice rating labels` = map_lgl(
        d,
        ~ !all(.$advisor0questionnaireHonestyLabel %in% qqLabelWhitelist)
      ),
      `Not enough changes` = map_lgl(
        d,
        ~ {
          diff <- mutate(
            .,
            diff = responseEstimateLeft != responseEstimateLeftFinal,
            diff = if_else(is.na(diff), F, diff)
          ) %>%
            pull(diff) %>% 
            mean() 
          diff < minChangeRate
        }
      )
    ) %>% 
    select(-atn_check, -d)
  
  do_exclusions(E$exclusions, envir = E)
  
  E$nMaxTrials <- E$AdvisedTrial %>% 
    group_by(pid) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    pull(n) %>% 
    max()
  
  E$AdvisedTrial <- E$AdvisedTrial %>% filter(timeEnd < maxTrialRT)
  
  E$exclusions <- E$exclusions %>% 
    mutate(
      `Too many outlying trials` = 
        map_lgl(pid, ~ E$AdvisedTrial %>% 
                  filter(pid == .x) %>%
                  summarise(n = n(), .groups = 'drop') %>% 
                  pull(n) < (E$nMaxTrials - nMaxOutliers)),
      # if participants were already dropped don't mark them here
      `Too many outlying trials` = if_else(!(pid %in% E$AdvisedTrial$pid),
                                           F, `Too many outlying trials`)
    )
  
  do_exclusions(E$exclusions, envir = E, backup = F)
}


for (E in c(.GlobalEnv, .prevStudy)) {
  
  fb <- E$AdvisedTrial %>% 
    group_by(pid) %>%
    summarise(Feedback = !all(is.na(timeFeedbackOn))) %>%
    mutate(Feedback = if_else(Feedback == 1, "Feedback", "No feedback"))
  
  tmp <- E$AdvisedTrial %>%
    left_join(fb, by = "pid") %>%
    filter(!is.na(advisor0idDescription)) %>%
    mutate(
      Advisor = advisor_description_name(advisor0idDescription),
      Advisor = factor(Advisor),
      responseCentre = responseEstimateLeft + (responseMarkerWidth / 2),
      Rating = factor(
        advisor0questionnaireHonestyLabel
      ),
      Rating = fct_relevel(
        Rating, 
        "Deceptive",
        "Possibly Deceptive", 
        "Honest"
      )
    ) %>%
    order_factors()
  
  E$Familiarisation <- tmp
}

for (E in c(.GlobalEnv, .prevStudy)) {
  E$tmp <- E$Familiarisation %>% 
    filter(Rating == "Honest") %>%
    group_by(pid, Advisor) %>%
    summarise(WOA = mean(advisor0WOA), .groups = "drop") %>%
    pivot_wider(names_from = Advisor, values_from = WOA)
  
  # Stats
  E$.T <- E$tmp %>%
    drop_na(-pid) %$%
    md.ttest(
      `Always honest`, 
      `Sometimes deceptive`, 
      paired = T,
      labels = c("M~AlwaysHonest~", "M~SometimesDeceptive~")
    )
  
  # Graph stats
  E$bf <- E$tmp %>%
    drop_na(-pid) %$%
    ttestBF(
      `Always honest`, 
      `Sometimes deceptive`, 
      paired = T
    )
  
  E$lost <- nrow(E$tmp) - nrow(drop_na(E$tmp, -pid))
  
  E$tmp <- E$tmp %>% 
    pivot_longer(-pid, names_to = "Advisor", values_to = "WOA") %>%
    mutate(Feedback = "No feedback")
}


bf <- bf2str(exp(bf@bayesFactor$bf))

## Graph
dw <- .1

graph_data <- tmp %>% mutate(
  experiment = '1a',
  Advisor = if_else(Advisor == "Always honest", "Honest", "Deceptive"),
  bf = bf
  )
save(graph_data, file = "./papers/rational-egocentric-discounting/woa-figure_graph_data.Rdata")

# Experiment 6 (02-02-behavioural-experiments.R)
source('scripts_and_filters/general_setup.R')

select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'directBenevolenceContexts', version == 'v0-0-1')
)

AdvisedTrial <- AdvisedTrial %>%
  filter(studyId == "directBenevolenceContexts") %>%
  annotate_responses()
Trial <- annotate_responses(Trial)


tmp <- bind_rows(
  mutate(practiceTrial, type = "practice", block = 0),
  mutate(practiceAdvisedTrial, type = "advice_practice", block = 1),
  mutate(Trial, type = "atn_check") %>% filter(!is.na(block)),
  mutate(AdvisedTrial, type = "core") %>% filter(!is.na(block))
) %>%
  group_by(pid, type, block) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type, block) %>%
  summarise(across(-pid, mean), .groups = "drop") %>%
  arrange(block)


nMaxOutliers <- 1
maxTrialRT <- 60000
qqLabelWhitelist = c(  ## Advice questionnaire responses must be one of:
  'Deceptive',
  'Possibly Deceptive',
  'Honest',
  NA
)
minChangeRate = .1     ## some advice taken on 10%+ of trials

exclusions <- AdvisedTrial %>% 
  nest(d = -pid) %>%
  mutate(
    atn_check = map_lgl(
      pid, 
      ~ !all(Trial$responseCorrect[Trial$pid == .]) |
        any(Trial$responseMarkerWidth[Trial$pid == .] != 11)
    ),
    `Attention check` = if_else(is.na(atn_check), T, atn_check),
    `Multiple attempts` = map_lgl(
      pid,
      ~ {
        h <- unique(AdvisedTrial$pidHash[AdvisedTrial$pid == .])
        p <- unique(AdvisedTrial$pid[AdvisedTrial$pidHash == h])
        length(p) > 1
      }
    ),
    `Missing advice rating` = map_lgl(
      d,
      ~ any(is.na(.$advisor0questionnaireHonestyLabel))
    ),
    `Odd advice rating labels` = map_lgl(
      d,
      ~ !all(.$advisor0questionnaireHonestyLabel %in% qqLabelWhitelist)
    ),
    `Not enough changes` = map_lgl(
      d,
      ~ {
        diff <- mutate(
          .,
          diff = responseEstimateLeft != responseEstimateLeftFinal,
          diff = if_else(is.na(diff), F, diff)
        ) %>%
          pull(diff) %>% 
          mean() 
        diff < minChangeRate
      }
    )
  ) %>% 
  select(-atn_check, -d)

do_exclusions(exclusions)

nMaxTrials <- AdvisedTrial %>% 
  group_by(pid) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  pull(n) %>% 
  max()

AdvisedTrial <- AdvisedTrial %>% filter(timeEnd < maxTrialRT)

exclusions <- exclusions %>% 
  mutate(
    `Too many outlying trials` = 
      map_lgl(pid, ~ AdvisedTrial %>% 
                filter(pid == .x) %>%
                summarise(n = n(), .groups = 'drop') %>% 
                pull(n) < (nMaxTrials - nMaxOutliers)),
    # if participants were already dropped don't mark them here
    `Too many outlying trials` = if_else(!(pid %in% AdvisedTrial$pid),
                                         F, `Too many outlying trials`)
  )

do_exclusions(exclusions, backup = F)

fb <- AdvisedTrial %>% 
  group_by(pid) %>%
  summarise(Feedback = !all(is.na(timeFeedbackOn))) %>%
  mutate(Feedback = if_else(Feedback == 1, "Feedback", "No feedback"))

tmp <- AdvisedTrial %>%
  left_join(fb, by = "pid") %>%
  filter(!is.na(advisor0idDescription)) %>%
  mutate(
    Advisor = advisor_description_name(advisor0idDescription),
    Advisor = factor(Advisor),
    responseCentre = responseEstimateLeft + (responseMarkerWidth / 2),
    Rating = factor(
      advisor0questionnaireHonestyLabel
    ),
    Rating = fct_relevel(
      Rating, 
      "Deceptive",
      "Possibly Deceptive", 
      "Honest"
    )
  ) %>%
  order_factors()

Familiarisation <- tmp

tmp <- Familiarisation %>% 
  filter(Rating == "Honest") %>%
  group_by(pid, Advisor) %>%
  summarise(WOA = mean(advisor0WOA), .groups = "drop") %>%
  pivot_wider(names_from = Advisor, values_from = WOA)

# Stats
.T <- tmp %>%
  drop_na(-pid) %$%
  md.ttest(
    `Honest group`, 
    `Deceptive group`, 
    paired = T,
    labels = c("M~HonestGroup~", "M~DeceptiveGroup~")
  )

# Graph stats
bf <- tmp %>%
  drop_na(-pid) %$%
  ttestBF(
    `Honest group`, 
    `Deceptive group`, 
    paired = T
  )

lost <- nrow(tmp) - nrow(drop_na(tmp, -pid))

tmp <- tmp %>% 
  pivot_longer(-pid, names_to = "Advisor", values_to = "WOA") %>%
  mutate(Feedback = "No feedback")

bf <- bf2str(exp(bf@bayesFactor$bf))

## Graph
dw <- .1

# Merge datasets

load("./papers/rational-egocentric-discounting/woa-figure_graph_data.Rdata")

graph_data <- rbind(
  graph_data,
  tmp %>% mutate(
    experiment = "1b",
    Advisor = if_else(Advisor == "Deceptive group", "Deceptive", "Honest"),
    bf = bf
  )
)

# Plot graph
labels <- graph_data %>% select(experiment, bf) %>% unique()
graph_data %>% 
  drop_na(everything()) %>%
  ggplot(aes(x = Advisor, y = WOA, fill = Advisor, colour = Advisor)) +
  facet_grid(paste("Experiment", experiment) ~ .) +
  geom_line(aes(group = pid), alpha = .25) +
  geom_split_violin(aes(x = nudge(Advisor, dw),
                        group = Advisor), width = .9,
                    colour = NA) +
  geom_boxplot(outlier.shape = NA, size = 1, width = dw/2,
               aes(x = nudge(Advisor, dw), 
                   group = Advisor),
               colour = 'black') +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1, colour = 'black') +
  geom_label(data = labels, y = 1, x = 1.5, colour = 'black', fill = 'white',
             aes(label = paste0('BF = ', bf))) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, length.out = 6),
                     expand = expansion(add = c(0, 0.1))) +
  scale_fill_advisor(aesthetics = c('fill', 'colour')) +
  labs(x = 'Advisor advice profile', y = 'Weight of "Honest" advice') 

source('papers/rational-egocentric-discounting/helpers.R')
save_figure(filename = "./papers/rational-egocentric-discounting/woa-figure.png")
