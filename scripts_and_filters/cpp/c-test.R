# C++ test

if (!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}
if (!require(esmData)) {
  if (!require(remotes)) install.packages('remotes')
  remotes::install_github('oxacclab/esmData', upgrade = F)
  library(esmData)
}
if (!require(Rcpp)) {
  install.packages('Rcpp')
}

set.seed(20201020)

# Load up some dots task data. Like all of it. Do need to check the ones where 
select_experiment('dotstask')
trials <- trials %>% 
  mutate(uid = factor(paste0(studyId, studyVersion, ' p', pid)))

# Take a subset for testing stuff
d <- trials %>% 
  nest(data = -uid) %>%
  mutate(okay = map_lgl(data, ~ any(.$hasChoice, na.rm = T))) %>%
  filter(okay) %>% 
  slice_sample(n = 1) %>%
  unnest(cols = data)

# Format how the C++ code wants the data:
# * @param trials a data frame of trials with 5 columns (names may vary):
# * initialConfidence - initial confidence rating (standardized within participant)
# * advisorIndex - index of the advisor chosen (0 or 1; NA if no choice made)
# * choice0 - index of the first advisor offered in the choice (NA if no choice offered)
# * choice1 - index of the second advisor offered in the choice (NA if no choice offered)
# * advisorAgrees - whether the chosen advisor agrees (NA if no advice provided)
# * confidenceShift - amount confidence shifted from initial to final judgement (standardized within participant)
d <- d %>%
  select(
    uid,
    initialConfidence, 
    advisorId, 
    choice0,
    choice1,
    advisorAgrees, 
    advisorInfluenceRaw
  ) %>%
  nest(d = -uid) %>%
  mutate(
    d = map(d, ~mutate(
      ., 
      initialConfidence = scale(initialConfidence),
      advisorIndex = as.numeric(factor(advisorId)) - 1, # C likes 0-indexing
      across(
        .cols = matches('choice[01]'), 
        ~ as.numeric(factor(., levels = levels(factor(advisorId)))) -1
      ),
      advisorInfluenceRaw = scale(advisorInfluenceRaw)
    ))
  ) %>%
  unnest(cols = d) %>%
  select(-uid, -advisorId)

# Let's have everyone run their data through a model where they are cumulatively 
# updating their trust in an advisor and that is driving their p(pick) and also
# their woa. 
Rcpp::sourceCpp('model-fit.cpp')

gradientDescent(d)
