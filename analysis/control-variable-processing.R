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

governor_data <- import(here("raw-data/governor_twitter_May-Oct.csv")) |>
  filter(state != "DC")

# get states present in dataset
states <- governor_data |>
  select(name, state) |>
  filter(!duplicated(name)) |>
  pull(state) |>
  sort()

# missing governors from AR, MS, NV, NY, WV, they likely don't have twitter
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

saveRDS(gender_mat, here("data/gov_gender_mat.Rds"))

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

saveRDS(party_mat, here("data/gov_party_mat.Rds"))

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

saveRDS(age_mat, here("data/gov_age_mat.Rds"))

# Bills -------------------------------------------------------------------

bill_edge <- import(here("data/full-edge-list.csv"))

states_to_filter <- setdiff(unique(bill_edge$state), states)

bill_edge_filtered <- bill_edge |>
  filter(!(state %in% states_to_filter), !(state_j %in% states_to_filter)) |>
  select(state, state_j, score)

bills <- graph.data.frame(bill_edge_filtered) |>
        get.adjacency(sparse = TRUE, attr = "score") |>
        as.matrix()

bills_nz <- bill_edge_filtered |>
  filter(score > 0) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "score") |>
  as.matrix()

zero_states <- bill_edge_filtered |>
  filter(score == 0) |>
  mutate(concat = paste0(state, state_j)) |>
  pull(concat)

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

blm = graph.data.frame(edge_blm) |>
              get.adjacency(sparse = TRUE, attr = "prop_diff") |>
              as.matrix()

blm_nz = edge_blm |>
  mutate(concat = paste0(state, state_j)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "prop_diff") |>
  as.matrix()

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

pro_blm <- graph.data.frame(edge_pro_blm) |>
  get.adjacency(sparse = TRUE, attr = "prop_diff") |>
  as.matrix()

pro_blm_nz = edge_pro_blm |>
  mutate(concat = paste0(state, state_j)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "prop_diff") |>
  as.matrix()

# Contiguity --------------------------------------------------------------

missing_states <- c("AR", "MS", "NV", "NY", "WV", "DC")

state_traits <- import(here("raw-data/state-traits-edgelist.csv")) |>
  filter(!(state_01 %in% missing_states), !(state_02 %in% missing_states))

contiguity <- state_traits |>
  select(state_01, state_02, contig) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "contig") |>
  as.matrix()

contiguity_nz <- state_traits |>
  select(state_01, state_02, contig) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "contig") |>
  as.matrix()

# Population --------------------------------------------------------------

population <- state_traits |>
  select(state_01, state_02, dif_population) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_population") |>
  as.matrix()

population_nz <- state_traits |>
  select(state_01, state_02, dif_population) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_population") |>
  as.matrix()


# Proportion Black --------------------------------------------------------

black <- state_traits |>
  select(state_01, state_02, dif_Black) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_Black") |>
  as.matrix()

black_nz <- state_traits |>
  select(state_01, state_02, dif_Black) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_Black") |>
  as.matrix()

# Ideology ----------------------------------------------------------------

ideology <- state_traits |>
  select(state_01, state_02, dif_mrp_ideology) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_mrp_ideology") |>
  as.matrix()

ideology_nz <- state_traits |>
  select(state_01, state_02, dif_mrp_ideology) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_mrp_ideology") |>
  as.matrix()

# Income ------------------------------------------------------------------

income <- state_traits |>
  select(state_01, state_02, dif_med_inc) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_med_inc") |>
  as.matrix()

income_nz <- state_traits |>
  select(state_01, state_02, dif_med_inc) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_med_inc") |>
  as.matrix()

# Urban -------------------------------------------------------------------

urban <- state_traits |>
  select(state_01, state_02, dif_urban_index) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_urban_index") |>
  as.matrix()

urban_nz <- state_traits |>
  select(state_01, state_02, dif_urban_index) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_urban_index") |>
  as.matrix()

# Proportion White --------------------------------------------------------

white <- state_traits |>
  select(state_01, state_02, dif_White) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_White") |>
  as.matrix()

white_nz <- state_traits |>
  select(state_01, state_02, dif_White) |>
  mutate(concat = paste0(state_01, state_02)) |>
  filter(!(concat %in% zero_states)) |>
  graph.data.frame() |>
  get.adjacency(sparse = TRUE, attr = "dif_White") |>
  as.matrix()

# Modeling ----------------------------------------------------------------

# BLM net reg

state_mats <- array(NA, c(8, length(bills[1,]), length(bills[1,])))

state_mats[1,,] <- blm
state_mats[2,,] <- contiguity
state_mats[3,,] <- black
state_mats[4,,] <- white
state_mats[5,,] <- urban
state_mats[6,,] <- income
state_mats[7,,] <- ideology
state_mats[8,,] <- population

set.seed(2023)

latent_lm <- sna::netlm(bills, state_mats, reps=100)

latent_model <- list()
latent_model <- summary(latent_lm)
latent_model$names <- c("Intercept", "BLM", "Contiguity", "Black", "White", "Urban", "Income", "Ideology", "Population")

latent_model

saveRDS(latent_model, here("final-paper/figures/blm_net_reg.Rds"))

#plot(latent_model$residuals, latent_model$fitted.values)

# pro-BLM net reg

state_mats <- array(NA, c(8, length(bills[1,]), length(bills[1,])))

state_mats[1,,] <- pro_blm
state_mats[2,,] <- contiguity
state_mats[3,,] <- black
state_mats[4,,] <- white
state_mats[5,,] <- urban
state_mats[6,,] <- income
state_mats[7,,] <- ideology
state_mats[8,,] <- population

latent_lm <- sna::netlm(bills, state_mats, reps=100)

latent_model <- list()
latent_model <- summary(latent_lm)
latent_model$names <- c("Intercept", "pro-BLM", "Contiguity", "Black", "White", "Urban", "Income", "Ideology", "Population")
latent_model

saveRDS(latent_model, here("final-paper/figures/pro_blm_net_reg.Rds"))


# Modeling no zero bills --------------------------------------------------

# BLM net reg

state_mats <- array(NA, c(8, length(bills_nz[1,]), length(bills_nz[1,])))

state_mats[1,,] <- blm_nz
state_mats[2,,] <- contiguity_nz
state_mats[3,,] <- black_nz
state_mats[4,,] <- white_nz
state_mats[5,,] <- urban_nz
state_mats[6,,] <- income_nz
state_mats[7,,] <- ideology_nz
state_mats[8,,] <- population_nz

latent_lm <- sna::netlm(bills_nz, state_mats, reps=100)

latent_model <- list()
latent_model <- summary(latent_lm)
latent_model$names <- c("Intercept", "BLM", "Contiguity", "Black", "White", "Urban", "Income", "Ideology", "Population")

latent_model

saveRDS(latent_model, here("final-paper/figures/blm_net_reg_nz.Rds"))

#plot(latent_model$residuals, latent_model$fitted.values)

# pro-BLM net reg

state_mats <- array(NA, c(8, length(bills_nz[1,]), length(bills_nz[1,])))

state_mats[1,,] <- pro_blm_nz
state_mats[2,,] <- contiguity_nz
state_mats[3,,] <- black_nz
state_mats[4,,] <- white_nz
state_mats[5,,] <- urban_nz
state_mats[6,,] <- income_nz
state_mats[7,,] <- ideology_nz
state_mats[8,,] <- population_nz

latent_lm <- sna::netlm(bills_nz, state_mats, reps=100)

latent_model <- list()
latent_model <- summary(latent_lm)
latent_model$names <- c("Intercept", "pro-BLM", "Contiguity", "Black", "White", "Urban", "Income", "Ideology", "Population")
latent_model

saveRDS(latent_model, here("final-paper/figures/pro_blm_net_reg_nz.Rds"))

# Governor Regression -----------------------------------------------------

governors <- import(here("data/full-labelled-tweets.csv")) |>
  select(name, party, gender, topic_blm_ratio, pro_blm_ratio) |>
  filter(!duplicated(name))

governor_mod <- lm(topic_blm_ratio ~ gender + party, data = governors)
summary(governor_mod)
plot(governor_mod)

pro_governor_mod <- lm(pro_blm_ratio ~ gender + party, data = governors)
summary(pro_governor_mod)
plot(governor_mod)

saveRDS(governor_mod, here("data/governor_mod.Rds"))
saveRDS(pro_governor_mod, here("final-paper/figures/pro_governor_mod.Rds"))

