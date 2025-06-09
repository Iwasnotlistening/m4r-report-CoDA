# Import libraries
library(dplyr)
library(ggplot2)

# Import dataframe
df <- read.csv("df_fuel_ckan.csv", 
               header = TRUE,
               stringsAsFactors = FALSE) 

df_3cats <- data.frame(
  Datetime <- df$DATETIME,
  Fossil <- df$GAS + df$COAL,
  Renewables <- df$WIND + df$HYDRO + df$SOLAR + df$BIOMASS,
  Other <- df$NUCLEAR + df$IMPORTS + df$STORAGE + df$OTHER
)

df_3cats$Datetime <- as.POSIXct(df_3cats$Datetime, format = "%Y-%m-%d %H:%M:%S")
df_3cats$year <- format(df_3cats$Datetime, "%Y")
df_3cats$month <- format(df_3cats$Datetime, "%m")
df_3cats$day <- format(df_3cats$Datetime, "%d")


# Cluster the data for each month
df_monthly <- aggregate(
  list(
    Fossil      = df_3cats$Fossil,
    Renewables  = df_3cats$Renewables,
    Other       = df_3cats$Other
  ),
  by = list(
    year  = df_3cats$year,
    month = df_3cats$month
  ),
  FUN = sum
)

# Convert into percentages
df_monthly <- df_monthly %>%
  mutate(
    total = Fossil + Renewables + Other,
    Fossil_perc = 100 * Fossil / total,
    Renewables_perc = 100 * Renewables / total,
    Other_perc = 100 * Other / total
  )

# Aggregate month dataset
df_by_month_fossil <- df_monthly %>%
  group_by(month) %>%
  summarise(Fossil_perc = list(Fossil_perc),
            .groups = "drop")

# Similarly, for renewables
df_by_month_renewables <- df_monthly %>%
  group_by(month) %>%
  summarise(Renewables_perc = list(Renewables_perc),
            .groups = "drop")


# Create the bootstrap samples
set.seed(123)  # for reproducibility
N <- 250

# Create a data frame to store results for each month
results_fossil <- data.frame(
  month            = df_by_month_fossil$month,
  mean_fossil_perc = NA_real_,
  ci_lower         = NA_real_,
  ci_upper         = NA_real_
)

results_renewables <- data.frame(
  month = df_by_month_renewables$month,
  mean_renewables_perc = NA_real_,
  ci_lower = NA_real_,
  ci_upper = NA_real_
)

for (i in seq_len(nrow(df_by_month_fossil))) {
  # Extract all fraction_fossil values for this month (across all years)
  vals_fossil <- df_by_month_fossil$Fossil_perc[[i]]
  vals_renewables <- df_by_month_renewables$Renewables_perc[[i]]
  
  boot_means_fossil <- numeric(N)
  boot_means_renewables <- numeric(N)
  
  for (b in seq_len(N)) {
    # Sample with replacement from vals
    sample_idx_fossil <- sample(seq_along(vals_fossil), size = length(vals_fossil), replace = TRUE)
    sample_idx_renewables <- sample(seq_along(vals_renewables), size = length(vals_renewables), replace = TRUE)
    
    boot_means_fossil[b] <- mean(vals_fossil[sample_idx_fossil], na.rm = TRUE)
    boot_means_renewables[b] <- mean(vals_renewables[sample_idx_renewables], na.rm = TRUE)
  }
  
  # Compute overall mean of bootstrap means, plus 95% CI
  results_fossil$mean_fossil_perc[i] <- mean(boot_means_fossil)
  ci_bounds_fossil <- quantile(boot_means_fossil, probs = c(0.025, 0.975))
  results_fossil$ci_lower[i] <- ci_bounds_fossil[1]
  results_fossil$ci_upper[i] <- ci_bounds_fossil[2]
  
  results_renewables$mean_renewables_perc[i] <- mean(boot_means_renewables)
  ci_bounds_renewables <- quantile(boot_means_renewables, probs = c(0.025, 0.975))
  results_renewables$ci_lower[i] <- ci_bounds_renewables[1]
  results_renewables$ci_upper[i] <- ci_bounds_renewables[2]
}

# Convert month to a factor or numeric for proper plotting
results_fossil$month <- factor(results_fossil$month, levels = c("01","02","03","04","05","06",
                                                  "07","08","09","10","11","12"))


results_renewables$month <- factor(results_renewables$month, levels = c("01","02","03","04","05","06",
                                                         "07","08","09","10","11","12"))
# Compute the same for 2009,16, 23
df_2009 <- df_monthly %>%
  filter(year == "2009") %>%
  group_by(month) %>%
  summarise(Fossil_perc_2009 = mean(Fossil_perc, na.rm = TRUE),
            Renewables_perc_2009 = mean(Renewables_perc, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(month = factor(month, levels = c("01","02","03","04","05","06",
                                          "07","08","09","10","11","12")))

df_2016 <- df_monthly %>%
  filter(year == "2016") %>%
  group_by(month) %>%
  summarise(Fossil_perc_2016 = mean(Fossil_perc),
            Renewables_perc_2023 = mean(Renewables_perc),
            .groups = "drop") %>%
  mutate(month = factor(month, levels = c("01", "02", "03", "04", "05", "06",
                                          "07", "08", "09", "10", "11", "12")))

df_2023 <- df_monthly %>%
  filter(year == "2023") %>%
  group_by(month) %>%
  summarise(Fossil_perc_2023 = mean(Fossil_perc, na.rm = TRUE),
            Renewables_perc_2023 = mean(Renewables_perc, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(month = factor(month, levels = c("01","02","03","04","05","06",
                                          "07","08","09","10","11","12")))

month_levels <- sprintf("%02d", 1:12)
month_labels <- month.abb

recode_month <- function(x) {
  factor(x, levels = month_levels, labels = month_labels)
}

results_fossil$month <- recode_month(results_fossil$month)
results_renewables$month <- recode_month(results_renewables$month)
df_2009$month <- recode_month(df_2009$month)
df_2016$month <- recode_month(df_2016$month)
df_2023$month <- recode_month(df_2023$month)

# Fix colours
colour_lines <- c(
  "Bootstrap mean" = "#1f77b4",
  "2009" = "#d62728",
  "2016" = "#71642a",
  "2023" = "#2ca02c"
)

col_fill <- c("95 % bootstrap CI" = "#1f77b4")

# Plot fossils
ggplot(results_fossil, aes(x = month)) +
  geom_ribbon(aes(ymin = ci_lower,  ymax = ci_upper,
                  fill = "95 % bootstrap CI", group = 1),
              alpha = 0.25, colour = NA) +
  geom_line(aes(y = mean_fossil_perc, colour = "Bootstrap mean", group = 1), size = 0.9) +
  geom_line(data = df_2009, aes(x = month, y = Fossil_perc_2009, colour = "2009", group = 1), size = 0.9) +
  geom_line(data = df_2016, aes(y = Fossil_perc_2016, colour = "2016", group = 1), size = 0.9) +
  geom_line(data = df_2023, aes(y = Fossil_perc_2023, colour = "2023", group = 1), size = 0.9) +
  scale_colour_manual(values = colour_lines, name = "Series") +
  scale_fill_manual(values = col_fill, name = "") +
  labs(x = "Month", y = "Fossil-fuel share (%)",
       title = "Monthly fossil-fuel share bootstrap confidence interval \n \
       Equidistant years overlaid") +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1.8, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.margin = margin(t=6, r=6, b=18, l=6)
  ) +
  coord_cartesian(clip = "off")

# Plot renewables
ggplot(results_renewables, aes(x = month)) +
  geom_ribbon(aes(ymin = ci_lower,  ymax = ci_upper,
                  fill = "95 % bootstrap CI", group = 1),
              alpha = 0.25, colour = NA) +
  geom_line(aes(y = mean_renewables_perc, colour = "Bootstrap mean", group = 1), size = 0.9) +
  geom_line(data = df_2009, aes(x = month, y = Renewables_perc_2009, colour = "2009", group = 1), size = 0.9) +
  geom_line(data = df_2016, aes(y = Renewables_perc_2023, colour = "2016", group = 1), size = 0.9) +
  geom_line(data = df_2023, aes(y = Renewables_perc_2023, colour = "2023", group = 1), size = 0.9) +
  scale_colour_manual(values = colour_lines, name = "Series") +
  scale_fill_manual(values = col_fill, name = "") +
  labs(x = "Month", y = "Renewables share (%)",
       title = "Monthly Renewables share bootstrap confidence interval \n \
       Equidistant years overlaid") +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1.8, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.margin = margin(t=6, r=6, b=18, l=6)
  )




