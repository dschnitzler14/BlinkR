library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(rstatix)

library(googlesheets4)
library(googledrive)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "BlinkR_app/.secrets"
)

#googledrive::drive_auth()
#googlesheets4::gs4_auth()

  combined_class_data_sheet <- drive_get("BlinkR_Combined_Class_Data")$id
  
  combined_class_data_read <- read_sheet(combined_class_data_sheet)


#data <- read.csv(here("BlinkR_app", "data","dummy_blinking_data.csv"))
data <- as.data.frame(combined_class_data_read)



#data_paired <- read.csv(here("BlinkR_app", "data","dummy_data_repeated_measures.csv"), header = TRUE)
data_paired <- as.data.frame(combined_class_data_read)

average_trs <- data %>%
  group_by(ID, Stress_Status) %>%
  summarise(Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE), .groups = 'drop')
as.data.frame(average_trs)

average_trs_paired <- data_paired %>%
  group_by(ID, Stress_Status) %>%
  summarise(Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE), .groups = 'drop')
as.data.frame(average_trs_paired)



summary(average_trs)

variance_qq_plot <- qqPlot(average_trs$Average_Blinks_Per_Minute,
                           main = "Q-Q Plot of Average Blinks/Minute",
                           xlab = "Theoretical Quantiles",
                           ylab = "Sample Quantiles",
                           col = "blue",
                           pch = 20)

average_trs$Stress_Status <- factor(average_trs$Stress_Status, levels = c("Unstressed", "Stressed"))

variance_boxplot <- boxplot(Average_Blinks_Per_Minute ~ Stress_Status, 
                            data = average_trs,
                            xlab = "Stress Status",
                            ylab = "Blinks Per Minute",
                            main = "Variance: Blinks/Minute by Stress Status",
                            col = c("grey49", "lightgrey"))  
variance_boxplot <- stripchart(Average_Blinks_Per_Minute ~ Stress_Status, 
           data = average_trs,
           add = TRUE, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 21, 
           bg = "maroon")

hist <- hist(average_trs$Average_Blinks_Per_Minute,
             main = "Distribution of Blinks/Minute",
             xlab = "Average Blinks/Minute",
             ylab = "Frequency",
             col = "grey49",Average_Blinks_Per_Minute
             border = "black")

t_test <- t.test(Average_Blinks_Per_Minute ~ Stress_Status, var.equal = TRUE, data = average_trs)

average_trs_paired_wide <- average_trs_paired %>%
  pivot_wider(names_from = Stress_Status, values_from = Average_Blinks_Per_Minute)

t_test_paired <- t.test(
  average_trs_paired_wide$Stressed,
  average_trs_paired_wide$Unstressed,
  paired = TRUE
)

data_summary <- average_trs %>%
  group_by(Stress_Status) %>%
  summarise(
    n = n(),
    mean = mean(Average_Blinks_Per_Minute, na.rm = TRUE),
    sd = sd(Average_Blinks_Per_Minute, na.rm = TRUE),
    sem = sd / sqrt(n)
  )

data_summary$Stress_Status <- factor(data_summary$Stress_Status, levels = c("Unstressed", "Stressed"))

barplot <- ggplot(data_summary, aes(x = Stress_Status, y = mean, fill = Stress_Status)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, position = position_dodge(.9)) +
  geom_jitter(
    data = average_trs, 
    aes(x = Stress_Status, y = Average_Blinks_Per_Minute), 
    width = 0.2, 
    size = 2, 
    color = "maroon"
  ) +
  scale_fill_manual(values = c("Unstressed" = "grey49", "Stressed" = "lightgrey")) +
  labs(x = "Stress Status",
       y = "Mean Blinks/Minute",
       title = "Mean Blinks/Minute by Stress Status") +
  theme_minimal() +
  theme(legend.position = "none")
   +
  #ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)



boxplot <- ggplot(average_trs, aes(x = Stress_Status, y = Average_Blinks_Per_Minute, fill = Stress_Status)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + 
  geom_jitter(
    data = average_trs, 
    aes(x = Stress_Status, y = Average_Blinks_Per_Minute), 
    width = 0.2, 
    size = 2, 
    color = "maroon"
  ) +
  scale_fill_manual(values = c("Unstressed" = "grey49", "Stressed" = "lightgrey")) +
  labs(
    x = "Stress Status",
    y = "Blinks/Minute",
    title = "Blinks/Minute by Stress Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)





### manual

stressed_data <- average_trs %>%
  filter(Stress_Status == "Stressed")
unstressed_data <- average_trs %>%
  filter(Stress_Status == "Unstressed")

# mean
stressed_mean <- mean(stressed_data$Average_Blinks_Per_Minute, na.rm = TRUE)
unstressed_mean <- mean(unstressed_data$Average_Blinks_Per_Minute, na.rm = TRUE)

#sd
stressed_sd <- sd(stressed_data$Average_Blinks_Per_Minute, na.rm = TRUE)
unstressed_sd <- sd(unstressed_data$Average_Blinks_Per_Minute, na.rm = TRUE)

#sem
stressed_n <- nrow(stressed_data)
unstressed_n <- nrow(unstressed_data)

stressed_sem <- stressed_sd / sqrt(stressed_n)
unstressed_sem <- unstressed_sd / sqrt(unstressed_n)

effect_size_paired_t <- average_trs %>%
  cohens_d(Average_Blinks_Per_Minute ~ Stress_Status, paired = TRUE)


df_effect_size <- effect_size_paired_t %>%
    dplyr::select("effsize")


df_effect_size$effsize[1]

df_effect_size$effsize[1] %>% unname()

as.numeric(df_effect_size$effsize[1] %>% unname())
