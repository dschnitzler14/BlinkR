library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(rstatix)

data <- read.csv("BlinkR_app/data/Caf_Dummy_Data.csv", header = TRUE)

average_trs_paired <- data %>%
  group_by(ID, Caffeine_Status) %>%
  summarise(Average_HR = mean(HR, na.rm = TRUE), .groups = 'drop')

hist <- hist(average_trs_paired$Average_HR,
             main = "Distribution of HR",
             xlab = "Average HR/Minute",
             ylab = "Frequency",
             col = "grey49",
             border = "black")


average_trs_paired_wide <- average_trs_paired %>%
  pivot_wider(names_from = Caffeine_Status, values_from = Average_HR)

t_test_paired <- t.test(
  average_trs_paired_wide$Caf,
  average_trs_paired_wide$"No-Caf",
  paired = TRUE
)

p_val <- t_test_paired$p.value

effect_size_paired_t <- average_trs_paired %>%
  cohens_d(Average_HR ~ Caffeine_Status, paired = TRUE)


data_summary <- average_trs_paired %>%
  group_by(Caffeine_Status) %>%
  summarise(
    n = n(),
    mean = mean(Average_HR, na.rm = TRUE),
    sd = sd(Average_HR, na.rm = TRUE),
    sem = sd / sqrt(n)
  )

data_summary$Caffeine_Status <- factor(data_summary$Caffeine_Status, levels = c("No-Caf", "Caf"))
average_trs_paired$Caffeine_Status <- factor(average_trs_paired$Caffeine_Status, levels = c("No-Caf", "Caf"))

boxplot <- ggplot(average_trs_paired, aes(x = Caffeine_Status, y = Average_HR, fill = Caffeine_Status)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + 
  geom_jitter(
    data = average_trs_paired, 
    aes(x = Caffeine_Status, y = Average_HR), 
    width = 0.2, 
    size = 2, 
    color = "maroon"
  ) +
  scale_fill_manual(values = c("No-Caf" = "grey49", "Caf" = "lightgrey")) +
  labs(
    x = "Caffeine Status",
    y = "HR/Minute",
    title = "HR/Minute by Caffeine Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(70, max(data_summary$mean + data_summary$sem) * 1.2)


