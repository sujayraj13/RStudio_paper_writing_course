source("analysis/scripts/packages_and_functions.R")

data_Syn <- read_csv("analysis/data/a-Syn-Data.csv")
data_Syn
# rename
data_Syn_clean <- data_Syn  %>%
  rename_with(~ gsub("_", "-", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("...", "_", .x, fixed = TRUE))

tb_syn <- data_Syn_clean |>
  pivot_longer(matches("aSyn"), 
               names_to = c("condition", "sample"), 
               names_sep = "_",
               values_to = "fluorescence")

plot_syn <- tb_syn %>%
  ggplot(aes(x = Time, y = fluorescence, color = condition)) +
  geom_smooth(method = 'loess') +
  theme_minimal()
plot_syn

plot_syn +
  annotate("segment", x = 20, xend = 50, y = 1, yend = 1, linewidth = 1)+
  annotate("text", x = 34, y = 300, label = "30 sec", size = 3)
