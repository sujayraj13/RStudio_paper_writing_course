# statistics for paper - to be sourced and inserted into the text
library(readr)
library(dplyr)

#read some pre-processed data text/csv file from the /data directory
syn <- read_csv2(here::here("analysis/data/head_celltypes_syn_matrix.csv"))

# extract some data -------------------------

max_PRC <- syn |>
  select(PRC) |>
  max()


# one of our datasets -----------------------------------------------------

data_Ashwini <- readxl::read_excel(
  here::here("analysis/data/24_01_22qpcr_1 - ash pal.xls")
  )

Gene <- c(rep("Nanog", 15), rep("oct4", 15), rep("sox2", 15),
  rep("Nestin", 15), rep("pax6", 15), rep("Foxg1", 15),
  rep("GAPDH", 15))
Gene

data_Ashwini$Gene <- Gene
head(data_Ashwini)

data_Ashwini_sel <- data_Ashwini %>%
  select(1:6) %>%
  janitor::clean_names()

data_Ashwini_sel_M_SD <- data_Ashwini_sel %>%
  group_by(gene) %>%
  mutate(mean2dct = mean(x2_dct)) %>%
  mutate(sd2dct = sd(x2_dct))

Nanog_mean <- data_Ashwini_sel_M_SD %>%
  filter(gene == "Nanog") %>%
  select(mean2dct) %>%
  pull() %>% unique() %>%
  format(scientific = FALSE, big.mark = ",", digits = 3)
Nanog_mean
