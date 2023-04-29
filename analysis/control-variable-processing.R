# PURPOSE: Prepare senator control variables as matrices for network regression


# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(rio)
# quick list of states
library(datasets)
library(igraph)

# Load data ---------------------------------------------------------------

i_am("analysis/control-variable-processing.R")

governor_data <- import(here("raw-data/governor_twitter_May-Oct.csv"))

# get states present in dataset
states <- governor_data |>
  select(name, state) |>
  filter(!duplicated(name)) |>
  pull(state) |>
  sort()

# missing governors from AR, MS, NV, NY, WV, they liekly don't have twitter
setdiff(state.abb, states)


# Gender Matrix -----------------------------------------------------------

gender <- governor_data |>
  select(name, gender, state) |>
  filter(!duplicated(name)) |>
  mutate(state_j = state,
         gender_j = gender)

combos <- gender |>
  expand(state, state_j) |>
  filter(state != state_j)

gender_1 <- gender |>
  select(state, gender)

gender_2 <- gender |>
  select(state_j, gender_j)

# Merge data
joined_gender <- left_join(combos, gender_1, by = "state")
joined_gender <- left_join(joined_gender, gender_2, by = "state_j")

binary_gender <- joined_gender |>
  mutate(same_gender = ifelse(gender == gender_j, 1, 0)) |>
  filter(same_gender == 1) |>
  select(state, state_j)

# https://stackoverflow.com/questions/16584948/how-to-create-weighted-adjacency-list-matrix-from-edge-list
gender_graph = graph.data.frame(binary_gender)
gender_adj = get.adjacency(gender_graph, sparse = TRUE)

gender_mat <- as.matrix(gender_adj)

# Party -------------------------------------------------------------------

party <- governor_data |>
  select(name, state, party) |>
  filter(!duplicated(name)) |>
  mutate(state_j = state,
         party_j = party)

party_1 <- party |>
  select(state, party)

party_2 <- party |>
  select(state_j, party_j)

# Merge data
joined_party <- left_join(combos, party_1, by = "state")
joined_party <- left_join(joined_party, party_2, by = "state_j")

binary_party <- joined_party |>
  mutate(same_party = ifelse(party == party_j, 1, 0)) |>
  filter(same_party == 1) |>
  select(state, state_j)

party_graph = graph.data.frame(binary_party)
party_adj = get.adjacency(party_graph, sparse = TRUE)
party_mat <- as.matrix(party_adj)

# Age ---------------------------------------------------------------------

age <- governor_data |>
  select(name, age, state) |>
  filter(!duplicated(name)) |>
  mutate(state_j = state,
         age_j = age)

age_1 <- age |>
  select(state, age)

age_2 <- age |>
  select(state_j, age_j)

# Merge data
joined_age <- left_join(combos, age_1, by = "state")
joined_age <- left_join(joined_age, age_2, by = "state_j")

num_age <- joined_age |>
  mutate(age_diff = abs(age_j - age)) |>
  select(-c(age, age_j))

age_graph = graph.data.frame(num_age)
age_adj = get.adjacency(age_graph, sparse = TRUE, attr = "age_diff")
age_mat <- as.matrix(age_adj)


# Bills -------------------------------------------------------------------

bill_edge <- import(here("data/bill_edgelist.Rds"))

states_to_filter <- setdiff(unique(bill_edge$state), states)

bill_edge_filtered <- bill_edge |>
  filter(!(state %in% states_to_filter), !(state_j %in% states_to_filter))

bill_graph = graph.data.frame(bill_edge_filtered)
bill_adj = get.adjacency(bill_graph, sparse = TRUE, attr = "score")
bill_mat <- as.matrix(bill_adj)



# BLM Tweets --------------------------------------------------------------

governor_tweets <- import(here("data/full-labelled-tweets.csv"))

blm <- governor_tweets |>
  rename(blm_prop = "topic_blm_ratio") |>
  select(name, state, blm_prop) |>
  filter(!duplicated(name)) |>
  mutate(state_j = state,
         blm_prop_j = blm_prop,
         )

blm_1 <- blm |>
  select(state, blm_prop)

blm_2 <- blm |>
  select(state_j, blm_prop_j)

# Merge data
joined_blm <- left_join(combos, blm_1, by = "state")
joined_blm <- left_join(joined_blm, blm_2, by = "state_j")

edge_blm <- joined_blm |>
  mutate(prop_diff = abs(blm_prop - blm_prop_j)) |>
  select(state, state_j, prop_diff)

blm_graph = graph.data.frame(edge_blm)
blm_adj = get.adjacency(blm_graph, sparse = TRUE, attr = "prop_diff")
blm_mat <- as.matrix(blm_adj)


# Pro-BLM Tweets ----------------------------------------------------------

pro_blm <- governor_tweets |>
  rename(pro_blm_prop = "pro_blm_ratio") |>
  select(name, state, pro_blm_prop) |>
  filter(!duplicated(name)) |>
  mutate(state_j = state,
         pro_blm_prop_j = pro_blm_prop,
  )

pro_blm_1 <- pro_blm |>
  select(state, pro_blm_prop)

pro_blm_2 <- pro_blm |>
  select(state_j, pro_blm_prop_j)

# Merge data
joined_pro_blm <- left_join(combos, pro_blm_1, by = "state")
joined_pro_blm <- left_join(joined_pro_blm, pro_blm_2, by = "state_j")

edge_pro_blm <- joined_pro_blm |>
  mutate(prop_diff = abs(pro_blm_prop - pro_blm_prop_j)) |>
  select(state, state_j, prop_diff)

pro_blm_graph = graph.data.frame(edge_pro_blm)
pro_blm_adj = get.adjacency(pro_blm_graph, sparse = TRUE, attr = "prop_diff")
pro_blm_mat <- as.matrix(pro_blm_adj)



# Contiguity --------------------------------------------------------------

state_traits <- import(here("raw-data/state-traits-edgelist.csv"))

contiguity <- import(here("raw-data/state-traits-edgelist.csv")) |>
  select(state_01, state_02, contig)


# Population --------------------------------------------------------------



# Proportion Black --------------------------------------------------------



# Proportion White --------------------------------------------------------



# Modeling ----------------------------------------------------------------

state_mats <- array(NA, c(5, length(bill_mat[1,]), length(bill_mat[1,])))

state_mats[1,,] <- gender_mat
state_mats[2,,] <- party_mat
state_mats[3,,] <- age_mat
state_mats[4,,] <- blm_mat
state_mats[5,,] <- pro_blm_mat

set.seed(2023)

latent_lm <- sna::netlm(bill_mat, state_mats, reps=100)

latent_model <- list()
latent_model <- summary(latent_lm)
latent_model$names <- c("Intercept", "Same Gender", "Same Party", "Age Difference", "BLM Tweet Prop Diff", "Pro-BLM Tweet Prop Diff")

latent_model

#plot(latent_model$residuals, latent_model$fitted.values)
