
library(incidentally)
library(here)
library(tidyverse)

here::i_am("data-pull.R")

url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/26589/WWK7YE"
dest <- "~/Desktop/Classes/SDS-338/final-paper/senator_data.csv"
download.file(url, dest)

library(rio)

import("~/Desktop/Classes/SDS-338/final-paper/senator_data.csv")

sen <- incidence.from.congress(session = 117, types = c("s"), areas = c("All"), format = "igraph", narrative = TRUE)

mat <- as.data.frame(sen$matrix[1:101, 1:5357])

mat <- mat |>
  select(S597, S353) |>
  mutate(tie = case_when(S597 == 1 ~ 1,
                         S353 == 1 ~ 1,
                         TRUE ~ 0
                         )
        ) |>
  select(tie) |>
  rownames_to_column("senator") |>
  expand() |>
  as.matrix()
  
typeof(mat)

library(igraph)

plot(graph_from_edgelist(mat))
