library(dplyr)
library(ggplot2)
library(car)
library(tidyr)

data <- read.csv(here("BlinkR_app", "data","dummy_blinking_data.csv"))
data

data_paired <- read.csv(here("BlinkR_app", "data","dummy_data_repeated_measures.csv"), header = TRUE)
data_paired

average_trs <- data %>%
  group_by(id, stress_status) %>%
  summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
as.data.frame(average_trs)

average_trs_paired <- data_paired %>%
  group_by(id, stress_status) %>%
  summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
as.data.frame(average_trs)



summary(average_trs)

variance_qq_plot <- qqPlot(average_trs$average_blinks_per_minute,
                           main = "Q-Q Plot of Average Blinks/Minute",
                           xlab = "Theoretical Quantiles",
                           ylab = "Sample Quantiles",
                           col = "blue",
                           pch = 20)

average_trs$stress_status <- factor(average_trs$stress_status, levels = c("unstressed", "stressed"))

variance_boxplot <- boxplot(average_blinks_per_minute ~ stress_status, 
                            data = average_trs,
                            xlab = "Stress Status",
                            ylab = "Blinks Per Minute",
                            main = "Variance: Blinks/Minute by Stress Status",
                            col = c("grey49", "lightgrey"))  
variance_boxplot <- stripchart(average_blinks_per_minute ~ stress_status, 
           data = average_trs,
           add = TRUE, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 21, 
           bg = "maroon")

hist <- hist(average_trs$average_blinks_per_minute,
             main = "Distribution of Blinks/Minute",
             xlab = "Average Blinks/Minute",
             ylab = "Frequency",
             col = "grey49",
             border = "black")

t_test <- t.test(average_blinks_per_minute ~ stress_status, var.equal = TRUE, data = average_trs)

average_trs_paired_wide <- average_trs_paired %>%
  pivot_wider(names_from = stress_status, values_from = average_blinks_per_minute)

t_test_paired <- t.test(
  average_trs_paired_wide$stressed,
  average_trs_paired_wide$unstressed,
  paired = TRUE
)

data_summary <- average_trs %>%
  group_by(stress_status) %>%
  summarise(
    n = n(),
    mean = mean(average_blinks_per_minute, na.rm = TRUE),
    sd = sd(average_blinks_per_minute, na.rm = TRUE),
    sem = sd / sqrt(n)
  )

data_summary$stress_status <- factor(data_summary$stress_status, levels = c("unstressed", "stressed"))

barplot <- ggplot(data_summary, aes(x = stress_status, y = mean, fill = stress_status)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, position = position_dodge(.9)) +
  geom_jitter(
    data = average_trs, 
    aes(x = stress_status, y = average_blinks_per_minute), 
    width = 0.2, 
    size = 2, 
    color = "maroon"
  ) +
  scale_fill_manual(values = c("unstressed" = "grey49", "stressed" = "lightgrey")) +
  labs(x = "Stress Status",
       y = "Mean Blinks/Minute",
       title = "Mean Blinks/Minute by Stress Status") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)



boxplot <- ggplot(average_trs, aes(x = stress_status, y = average_blinks_per_minute, fill = stress_status)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + 
  geom_jitter(
    data = average_trs, 
    aes(x = stress_status, y = average_blinks_per_minute), 
    width = 0.2, 
    size = 2, 
    color = "maroon"
  ) +
  scale_fill_manual(values = c("unstressed" = "grey49", "stressed" = "lightgrey")) +
  labs(
    x = "Stress Status",
    y = "Blinks/Minute",
    title = "Blinks/Minute by Stress Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)



