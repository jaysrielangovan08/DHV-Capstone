# ==============================
# EV ADOPTION & SUSTAINABILITY ANALYSIS
# ==============================

# 1. Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

# 2. Create synthetic data
set.seed(123)

years <- 2010:2025
regions <- c("CountryA", "CountryB", "CountryC",
             "CountryD", "CountryE", "CountryF")

data_list <- list()

for (region in regions) {
  total <- sample(50000:200000, 1)
  ev <- sample(50:500, 1)
  growth_total <- 1.02
  growth_ev <- 1.25
  
  for (year in years) {
    total <- as.integer(total * growth_total)
    ev <- as.integer(ev * growth_ev)
    if (ev > total) ev <- total
    
    data_list[[length(data_list) + 1]] <-
      data.frame(year, region,
                 total_vehicles = total,
                 ev_count = ev)
  }
}

df <- bind_rows(data_list)
df$ev_share_pct <- (df$ev_count / df$total_vehicles) * 100

# 3. Aggregate global EV trend
agg <- df %>%
  group_by(year) %>%
  summarise(
    total_vehicles = sum(total_vehicles),
    ev_count = sum(ev_count),
    .groups = "drop"
  ) %>%
  mutate(
    ev_share_pct = (ev_count / total_vehicles) * 100,
    ev_count_roll3 = rollmean(ev_count, k = 3, fill = NA, align = "right")
  )

# 4. CAGR calculation
cagr <- function(start, end, years) {
  (end / start)^(1 / years) - 1
}

start_ev <- agg$ev_count[1]
end_ev <- agg$ev_count[nrow(agg)]
years_gap <- agg$year[nrow(agg)] - agg$year[1]
cagr_value <- cagr(start_ev, end_ev, years_gap) * 100
cat(sprintf("EV CAGR: %.2f%% per year\n", cagr_value))

# 5. Global EV adoption trend plot
ggplot(agg, aes(x = year)) +
  geom_line(aes(y = ev_count), linewidth = 1) +
  geom_line(aes(y = ev_count_roll3), linetype = "dashed") +
  geom_point(aes(y = ev_count)) +
  labs(
    title = "Global EV Adoption Trend",
    x = "Year",
    y = "EV Count"
  ) +
  theme_minimal()

# 6. Stacked area plot – EV share by region
ggplot(df, aes(x = year, y = ev_share_pct, fill = region)) +
  geom_area() +
  labs(
    title = "EV Share (%) by Region",
    x = "Year",
    y = "EV Share (%)"
  ) +
  theme_minimal()

# 7. Bar chart – latest year EV share
latest_year <- max(df$year)
latest <- df %>% filter(year == latest_year)

ggplot(latest, aes(x = region, y = ev_share_pct)) +
  geom_col() +
  labs(
    title = paste("EV Share (%) by Region -", latest_year),
    x = "Region",
    y = "EV Share (%)"
  ) +
  theme_minimal()

# 8. Year-over-Year EV growth heatmap
df <- df %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(
    prev_ev = lag(ev_count),
    ev_yoy_growth = (ev_count / prev_ev - 1) * 100
  )

ggplot(df, aes(x = year, y = region, fill = ev_yoy_growth)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green",
    na.value = "grey"
  ) +
  labs(
    title = "Year-over-Year EV Growth (%) by Region",
    x = "Year",
    y = "Region"
  ) +
  theme_minimal()

# 9. Avoided CO2 emissions calculation
km_per_year <- 10000
emission_factor <- 0.0002

baseline <- df %>%
  filter(year == min(year)) %>%
  select(region, ev_count)

em_df <- df %>%
  left_join(baseline, by = "region", suffix = c("", "_base")) %>%
  mutate(
    delta_ev = pmax(ev_count - ev_count_base, 0),
    avoided_tCO2 = delta_ev * km_per_year * emission_factor
  )

em_agg <- em_df %>%
  group_by(year) %>%
  summarise(avoided_tCO2 = sum(avoided_tCO2), .groups = "drop")

# 10. CO2 reduction trend plot
ggplot(em_agg, aes(x = year, y = avoided_tCO2)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Estimated Avoided CO₂ Emissions from EV Adoption",
    x = "Year",
    y = "Avoided CO₂ (tonnes)"
  ) +
  theme_minimal()

# 11. Save output files
dir.create("outputs", showWarnings = FALSE)
write.csv(df, "outputs/ev_by_region.csv", row.names = FALSE)
write.csv(agg, "outputs/ev_global_trend.csv", row.names = FALSE)
write.csv(em_agg, "outputs/ev_avoided_co2.csv", row.names = FALSE)

cat("All plots generated and data saved to 'outputs' folder.\n")
