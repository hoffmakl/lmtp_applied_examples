# Toy data for point treatment time to event

library(tidyverse)
library(lmtp)

progressr::handlers(global = TRUE)
set.seed(7)

n <- 5000 # samp size
max_fu_day <- 5 # max follow up days

# toy data has no treatment effect
id <- 1:n
fu <- sample(seq(1,max_fu_day), n, replace = T)
event <- rbinom(n, 1, .5)
W_1 <- rbinom(n, 1, .5)
W_2 <- rbinom(n, 1, .5)
A <- rbinom(n, 1, .5)

toy <- tibble(id, fu, event, W_1, W_2, A)

# vector to replace with all 0s for censoring
replace_obs <- as.list(rep(0,max_fu_day)) 
names(replace_obs) <- paste0("C_",1:max_fu_day)

# censoring wide format
cens <-
  toy %>%
  mutate(day = fu) %>%
  group_by(id) %>%
  complete(id, day = full_seq(1:day, 1)) %>%
  fill(fu, event, .direction = "up") %>%
  mutate(C = 1) %>%
  pivot_wider(id_cols = c("id"), 
    names_from = day,
    values_from = C, 
    names_prefix = "C_") %>%
  replace_na(replace_obs)

# outcome wide format
outcome <-
  toy %>%
  mutate(day = fu) %>%
  group_by(id) %>%
  complete(id, day = full_seq(1:max_fu_day, 1)) %>%
  fill(fu, event, .direction = "downup") %>%
  mutate(
    Y = case_when(
      day < fu ~ 0, # patients who died should not have the event before their last follow up day
      event == 0 & day == fu ~ 0, # patients who are censored on final day have an outcome observed
      event == 0 & day > fu ~ NA_real_, # after patients are censored, they should have NAs for outcome
      event == 1 & day >= fu ~ 1  # once a patient has an event, they always have an outcome status of 1
  )) %>%
  pivot_wider(id_cols = c("id"), 
              names_from = day,
              values_from = Y, 
              names_prefix = "Y_")

# make lmtp data, wide format. can compare with lmtp::sim_point_surv
dat_lmtp <- reduce(list(toy, outcome, cens), full_join) %>%
  select(id, fu, event, A, W_1, W_2, C_1, Y_1, C_2, Y_2, C_3, Y_3, C_4, Y_4, C_5, Y_5)

# run lmtp - no trt effect so static binary on and off should be similar

lmtp_tmle(
    dat_lmtp,
    trt  = "A",
    baseline = c("W_1","W_2"),
    outcome = c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5"),
    cens = c("C_1", "C_2", "C_3", "C_4", "C_5"),
    outcome_type = "survival",
    folds = 2, 
    .SL_folds = 2,
    shift = static_binary_off
)

lmtp_tmle(
  dat_lmtp,
  trt  = "A",
  baseline = c("W_1","W_2"),
  outcome = c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5"),
  cens = c("C_1", "C_2", "C_3", "C_4", "C_5"),
  outcome_type = "survival",
  folds = 2, 
  .SL_folds = 2,
  shift = static_binary_on
)



