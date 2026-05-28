library(ggplot2)
library(dplyr)
theme_set(theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank()) +
            theme(
              text = element_text(family = 'serif', size = 12),
              axis.text = element_text(color = "black", size = 11),
              strip.text = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12)
            ) +
            theme(strip.background = element_blank(),
                  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)))
mhe_discharge = clean_names(Discharge_CDMS)
mhe_discharge$flow_cfs <- as.numeric(mhe_discharge$flow_cfs)
mhe_discharge <- clean_names(mhe_discharge)

discharge <- mhe_discharge %>%
  mutate(year = year(activity_date),
         location_name_revised = 
           case_when(
             location == "East Fork Irrigation District below diversion" ~  "East Fork Hood River",
             location == "Rogers Creek" ~ location,
             location == "Neal Creek" ~ location,
             location == "Odell Creek" ~ location,
             location == "Dog River" ~ location,
             location == "Tony Creek" ~ location),
         metrics = "flow",
         decade = case_when(
           year >= 2009 & year <= 2012 ~ "2009-2012",
           year >= 2013 & year <= 2018 ~ "2013-2018",
           year >= 2019 & year <= 2024 ~ "2019-2024",
           TRUE ~ NA_character_),
         month = month(activity_date),
         water_year_season = case_when(
           month %in% c(10:12) ~ "Fall/Winter " , #(recharge)
           month %in% c(1, 2, 3) ~ "Winter/Spring ", #(accumulation)
           month %in% c(4:9) ~ "Spring/Summer ", #(depletion)
           .default = NA_character_))%>%
  filter(!is.na(flow_cfs), !is.na(decade)) 




month_count_filter <- discharge %>%
  mutate(month_loc_year = paste(month, location_name_revised, year, sep = "_"))%>%
  group_by(month, year, location_name_revised)%>% 
  mutate(n = n())%>%
  filter(n > 5) %>%
  dplyr::select(location_name_revised, month, year, month_loc_year)
  
unique_dt <- discharge %>%   
  mutate(month_loc_year = paste(month, location_name_revised, year, sep = "_"))%>%
  
  mutate(datetime = as.POSIXct(paste(activity_date, time, sep = " "),
                               format = "%Y-%m-%d %H:%M")) %>%
  dplyr::select(location_name_revised, datetime, flow_cfs, month_loc_year, month, year, water_year_season) %>%
  distinct()%>%
  ungroup() %>%
  group_by(location_name_revised, year, month) %>%
  mutate(n = n()) %>%
  filter(n >= 3)

summary_uniquedt <- unique_dt %>%
  group_by(location_name_revised, year, month) %>%
  summarise(n = n()) 
  

discharge %>%
  dplyr::select(location_name_revised, month, year, flow_cfs, decade, water_year_season) %>%
  mutate(month_loc_year = paste(month, location_name_revised, year, sep = "_"))%>%
  group_by(location_name_revised, water_year_season, decade) %>%
  filter(month_loc_year %in% month_count_filter$month_loc_year)%>%
ungroup() %>%
  ggplot(aes(x = as.factor(water_year_season), y = flow_cfs)) +
  geom_boxplot(aes(fill = decade), color = "black", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~location_name_revised, scales = "free_y") +
  labs(x = "Season", y = "Flow (cfs)") +
  theme(legend.position = "top")

# annual_averages <- discharge %>%
#   group_by(location, year) %>%
#   summarise(annual_avg_cfs = mean(flow_cfs, na.rm = TRUE), .groups = "drop")
# monthly_counts <- discharge %>%
#   mutate(month = month(activity_date))%>%
#   group_by(location, activity_date) %>%
#   summarise(
#     daily_avg_cfs = mean(flow_cfs, na.rm = TRUE),
#     year = first(year),
#     month = first(month(activity_date)),  # Extract month
#     .groups = "drop"
#   ) %>%
#   # Join with annual averages
#   left_join(annual_averages, by = c("location", "year")) %>%
#   # Count days above annual average by location, year, and month
#   group_by(location, year, month) %>%
#   filter(year %in% 2009:2024)%>%
#   summarise(
#     days_above_annual_avg = sum(daily_avg_cfs < annual_avg_cfs, na.rm = TRUE),
#     total_days = n(),
#     .groups = "drop"
#   )
# # Assign decades and calculate proportions
# monthly_counts_decades <- monthly_counts %>%
#   mutate(
#     decade = case_when(
#       year >= 2009 & year <= 2012 ~ "2009-2012",
#       year >= 2013 & year <= 2018 ~ "2013-2018",
#       year >= 2019 & year <= 2024 ~ "2019-2024",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   group_by(decade, location, month) %>%
#   summarise(
#     total_days_above = sum(days_above_annual_avg, na.rm = TRUE),
#     total_days = sum(total_days, na.rm = TRUE),
#     proportion_above = total_days_above / total_days,
#     .groups = "drop"
#   )
# 
# # Create the plot
# annual_flow <- ggplot(monthly_counts_decades, aes(x = month, y = proportion_above, fill = location)) +
#   geom_col(position = "dodge") +
#   facet_grid(rows = vars(decade)) +
#   labs(
#     x = "Month",
#     y = "Proportion of Days Below Annual Average",
#     fill = "Location"
#   ) +
#   scale_x_continuous(
#     breaks = 1:12,
#     labels = month.abb
#   ) +
#   scale_y_continuous(
#     labels = scales::percent_format()
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Dog River" = "steelblue",
#       "East Fork Irrigation District below diversion" = "goldenrod",
#       "Neal Creek" = "tomato",
#       "Odell Creek" = "darkcyan",
#       "Rogers Creek" = "darkgreen"
#     )
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#     strip.text.y = element_text(angle = 0, hjust = 1, size = 9, face = "bold"),
#     strip.background = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.spacing = unit(0.3, "lines"),
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#     panel.background = element_rect(fill = "white", color = "black"),
#     plot.background = element_rect(fill = "white"),
#     legend.position = "bottom"
#   )
# 
# ggsave("figures/annual_flow_patterns.png", annual_flow, width = 10, height = 8, dpi = 600)
# 
