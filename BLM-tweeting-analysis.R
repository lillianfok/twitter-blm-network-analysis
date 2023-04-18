# PURPOSE: Construct a measure of pro-BLM-tweeting

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rio)
library(here)

# Get keywords ------------------------------------------------------------

senator_tweets <- import(here("senator_twitter_May-Oct.csv"))

sen <- incidence.from.congress(session = 117, types = c("s"),
                               areas = c("All"), format = "data",
                               narrative = TRUE)
