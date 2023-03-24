
library(incidentally)
library(here)

here::i_am("data-pull.R")

url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/26589/WWK7YE"
dest <- "~/Desktop/Classes/SDS-338/final-paper/senator_data.csv"
download.file(url, dest)

library(rio)

import("~/Desktop/Classes/SDS-338/final-paper/senator_data.csv")

sen <- incidence.from.congress(session = 117, types = c("s"), areas = c("All"), format = "data", narrative = TRUE)

mat <- as.data.frame(sen$matrix[1:101, 1:5357])

mat <- mat |>
  select(S597)