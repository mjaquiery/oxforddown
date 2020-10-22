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

#set.seed(20201020)

Rcpp::sourceCpp('model-fit.cpp')
# Rcpp::sourceCpp('scripts_and_filters/cpp/model-fit.cpp')

x <- data.frame(
  initialConfidence = rep(0, sample(10:30, 1)),
  advisorIndex = 0,
  choice0 = NA,
  choice1 = NA,
  advisorAgrees = 1,
  advisorInfluenceRaw = .5
)

print(paste0('gradientDescent(x = ', nrow(x), ' x ', ncol(x), ' )'))
gradientDescent(x)

# Let's have everyone run their data through a model where they are cumulatively 
# updating their trust in an advisor and that is driving their p(pick) and also
# their woa. 

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
      advisorInfluenceRaw = scale(advisorInfluenceRaw),
      # round for debugging
      initialConfidence = round(initialConfidence, 4),
      advisorInfluenceRaw = round(advisorInfluenceRaw, 4)
    ))
  ) %>%
  unnest(cols = d) %>%
  select(-uid, -advisorId)

print('gradientDescent(d)')
mdl <- gradientDescent(d)
mdl

-0.626045 -1.11867 -1.095 -2.96538 -3.03012 -1.12288 -1.01929 -2.95858 -1.09517 -1.01461 -0.818914 -2.96765 -2.96692 -1.08117 -1.08397 -3.03659 -1.03948 -1.02872 -0.940258 -2.98242 -3.18158 -1.01329 -3.03874 -3.11239 -0.913488 -3.0642 -3.08397 -3.00949 -1.02602 -0.945744 -0.818768 -2.62154 -1.0413 -3.05879 -0.766288 -2.86488 -0.732131 -2.9155 -2.8041 -0.881341 -2.87834 -0.997345 -1.11509 -2.90028 -2.85336 -2.71107 -0.881561 -0.900687 -2.9204 -1.07012 -0.945268 -0.773639 -0.957555 -0.832373 -2.83958 -0.809516 -0.963407 -0.998297 -2.99844 -0.928446 -0.903837 -5 -5.01666 -2.07257 -0.912161 -1.8346 -4.90416 -2.36931 -5.02587 -4.95631 -1.98835 -2.52494 -5.08131 -2.67801 -4.99888 -0.627171 -4.84678 -1.9599 -2.11315 -0.0217357 -4.88614 -4.89143 -1.17772 -1.73151 -3.94881 -1.22313 -2.45682 -1.90169 -1.53784 -4.89547 -4.9599 -1.98553 -0.277432 -4.9373 -2.12774 -5.03948 -2.48618 -4.94863 -1.80355 -4.86591 -1.93591 -4.7348 -1.88661 -1.468 -4.95485 -4.94498 -1.7093 -1.98784 -4.7348 -1.64363 -1.89858 -4.58841 -1.56687 -1.53318 -4.71867 -1.81507 -4.87765 -4.7179 -4.90489 -1.92322
SS=977.364; n=120; MSE=8.1447
-0.923114 -1.03786 -0.733582 -3.06529 -2.79677 -0.859955 -0.954719 -3.06529 -0.891532 -0.970515 -1.27065 -3.11268 -3.0337 -0.938922 -0.859955 -2.95472 -0.907329 -0.954719 -1.11268 -3.0179 -2.76518 -0.938922 -2.93892 -2.81256 -1.14428 -2.93892 -2.85995 -2.97052 -0.938922 -1.08109 -1.30224 -3.27065 -0.954719 -2.90733 -1.17587 -3.25485 -1.23906 -3.06529 -3.22326 -1.09689 -3.12848 -1.00211 -0.844158 -3.25485 -3.14428 -3.22326 -1.11268 -1.08109 -3.08109 -0.923126 -1.09689 -1.23906 -1.0337 -1.20746 -3.17587 -1.16007 -1.0337 -1.00211 -3.00211 -1.09689 -1.8763 -5 -4.97052 -1.89153 -1.95706 -2.33384 -5.06529 -1.93892 -4.97052 -5.06529 -2.00211 -1.90733 -4.90733 -1.87574 -5.00211 -2.25485 -5.20746 -2.0337 -1.85995 -2.31804 -5.0179 -5.25485 -2.14428 -2.20746 -5.17587 -2.12848 -1.92313 -2.0179 -2.08109 -5.0179 -5.0337 -2.0179 -2.30224 -5.08109 -1.85995 -4.90733 -1.92313 -5.09689 -2.20746 -5.11268 -2.06529 -5.30224 -2.08109 -2.09689 -5.0495 -5.06529 -2.0495 -2.00211 -5.30224 -2.25485 -2.0179 -5.31804 -2.30224 -2.33384 -5.19167 -2.16007 -5.11268 -5.0495 -5.09689 -2.08109
SS=1091.11; n=120; MSE=9.09258
-1.16418 -0.982391 -0.972668 -2.88432 -2.97915 -1.2481 -1.08022 -2.88432 -1.19216 -1.05224 -0.520519 -3.01156 -3.00346 -1.10821 -1.2481 -2.99535 -1.16418 -0.995354 -0.800369 -2.96828 -3.41601 -1.10821 -2.99373 -2.98077 -0.744399 -3.10821 -3.2481 -2.99698 -1.10821 -1.00832 -0.464548 -2.52052 -0.995354 -2.99049 -0.688429 -3.02615 -0.576489 -3.0067 -3.0229 -1.00994 -3.01318 -0.996265 -1.27609 -2.5485 -3.0148 -2.60447 -1.01156 -0.85634 -3.00832 -1.13619 -0.828355 -0.576489 -0.940295 -0.632459 -3.01804 -0.716414 -0.940295 -0.996265 -2.99627 -0.828355 -1.85775 -5 -5 -2 -1.89535 -2 -5 -2.03634 -5 -5 -1.99875 -2.05514 -5 -2.07394 -5 -1.84835 -5 -2 -2 -1.81075 -4.98935 -5 -1.91415 -2 -4.89535 -1.92355 -2.04574 -1.98935 -1.95175 -4.98935 -5 -2 -1.82015 -5 -2 -5 -2.04574 -5 -2 -5 -2 -5 -2 -1.94235 -5 -5 -1.97055 -1.99875 -5 -2 -1.98935 -5 -2 -2 -5 -2 -5 -4.97055 -5 -2
SS=1035.27; n=120; MSE=8.62722