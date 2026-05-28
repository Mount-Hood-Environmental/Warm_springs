
# Created by: Katie Kennedy
# Creation date: 04/16/2026
# Last updated: 
######## SCRIPT SUMMARY ###########################################################################
# This script was written to examine spatial and temporal trends in historic                      
# data from streams within the Hood River Basin. Data was sourced                                 
# from Confederated Tribes of Warm Springs.                                                       
# Data referred to with the prefix "mhe_" refers to CDMS data that was adjusted, QA/QC'd or otherwise  
# altered by MHE                                                                                  
###################################################################################################
library(tidyverse)
library(readxl)
library(janitor)


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

mhe_data <- lapply(excel_sheets("~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx"), 
                   read_excel, path = "~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx")
names(mhe_data) <- excel_sheets("~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx")

names(mhe_data) <- paste0("mhe_", names(mhe_data))
list2env(mhe_data, envir = .GlobalEnv)


mhe_discharge = mhe_Discharge

#### what are the distributions of flow per year per site
mhe_discharge$flow_cfs <- as.numeric(mhe_discharge$flow_cfs)
mhe_discharge <- mhe_discharge %>% filter(location != "Tony Creek")%>%
  group_by(location) %>%
  mutate(mean_discharge = mean(flow_cfs))%>%
  ungroup()
mhe_discharge <- mhe_discharge %>%
  filter(flow_cfs > 0,
         year %in% 2009:2024)

# Create alternating year shading data
year_bands <- mhe_discharge %>%
  group_by(year) %>%
  summarise(
    xmin = as.Date(paste0(year, "-01-01")),
    xmax = as.Date(paste0(year, "-12-31")),
    .groups = "drop"
  ) %>%
  mutate(xmax = pmin(xmax, max(mhe_discharge$activity_date))) %>%
  mutate(fill = ifelse(year %% 2 == 0, "grey90", "white"))
# Mid-point of each year for year label placement
year_labels <- mhe_discharge %>%
  group_by(year) %>%
  summarise(mid_date = min(activity_date) + (max(activity_date) - min(activity_date)) / 2,
            .groups = "drop")

# Create per-location mean for hline
location_means <- mhe_discharge %>%
  group_by(location) %>%
  summarise(mean_discharge = mean(flow_cfs, na.rm = TRUE),
            max_discharge = max(flow_cfs, na.rm = TRUE),
            min_discharge = min(flow_cfs, na.rm = TRUE),
            .groups = "drop") %>%
  distinct()

location_labs <- c(
  "Dog River"                                    = "Dog River",
  "East Fork Irrigation District below diversion" = "East Fork Irrigation\nDistrict below diversion",
  "Neal Creek"                                   = "Neal Creek",
  "Odell Creek"                                  = "Odell Creek",
  "Rogers Creek"                                 = "Rogers Creek"
)

temporal_flow <- ggplot(data = mhe_discharge, aes(x = activity_date, y = flow_cfs)) +
  geom_rect(
    data = year_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  scale_fill_identity() +
  geom_point() +
  geom_hline(
    data = location_means,
    aes(yintercept = mean_discharge),
    color = "black",
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_x_date(
    limits = c(as.Date("2009-01-01"), as.Date("2024-12-31")),
    breaks = seq(as.Date("2009-01-01"), as.Date("2024-12-31"), by = "6 months"),
    date_labels = "%b-%d",
    sec.axis = dup_axis(
      breaks = year_labels$mid_date,
      labels = year_labels$year,
      name = NULL
    )
  ) +
  facet_grid(rows = vars(location), labeller = labeller(location = location_labs), scales = "free") +
  theme(
    strip.text.y.right = element_text(angle = 0, hjust = 0, size = 7, lineheight = 0.9),
    axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.x.top = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12))
  ) +
  labs(x = "", y = "Flow (cfs)")


discharge_mean <- mhe_discharge %>%
  group_by(activity_date, location)%>%
  summarise(
    daily_mean = mean(flow_cfs)
  )
temporal_flow_means <- ggplot(data = discharge_mean, aes(x = activity_date, y = daily_mean)) +
  geom_rect(
    data = year_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  scale_fill_identity() +
  geom_point() +
  geom_hline(
    data = location_means,
    aes(yintercept = mean_discharge),
    color = "black",
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_x_date(
    limits = c(as.Date("2009-01-01"), as.Date("2024-12-31")),
    breaks = seq(as.Date("2009-01-01"), as.Date("2024-12-31"), by = "6 months"),
    date_labels = "%b-%d",
    expand = c(0, 0),  # Add this line
    sec.axis = dup_axis(
      breaks = year_labels$mid_date,
      labels = year_labels$year,
      name = NULL
    )
  ) +
  facet_grid(rows = vars(location), labeller = labeller(location = location_labs), scales = "free") +
  theme(
    strip.text.y.right = element_text(angle = 0, hjust = 0, size = 7, lineheight = 0.9),
    axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.x.top = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12))
  ) +
  labs(x = "", y = "Flow (cfs)")


monthly_discharge_mean <- mhe_discharge %>%
  mutate(month = month(activity_date)) %>%
  group_by(year, month, location) %>%
  summarise(
    monthly_mean = mean(flow_cfs),
    .groups = "drop"
  ) %>%
  # Create a date for each year-month combination
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))

# Update year_bands to use dates
year_bands_dates <- year_bands %>%
  select(year, xmin, xmax, fill)

# Create year label positions (middle of each year)
year_label_positions <- year_bands_dates %>%
  mutate(mid_date = xmin + (xmax - xmin) / 2)

monthly_flow_means <- ggplot(data = monthly_discharge_mean, aes(x = date, y = monthly_mean)) +
  geom_rect(
    data = year_bands_dates,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  scale_fill_identity() +
  geom_point() +
  geom_hline(
    data = location_means,
    aes(yintercept = mean_discharge),
    color = "black",
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_x_date(
    limits = c(as.Date("2009-01-01"), as.Date("2024-12-31")),
    breaks = seq(as.Date("2009-01-01"), as.Date("2024-12-31"), by = "6 month"),
    date_labels = "%b",
    expand = c(0, 0),
    sec.axis = dup_axis(
      breaks = year_label_positions$mid_date,
      labels = year_label_positions$year,
      name = NULL
    )
  ) +
  facet_grid(rows = vars(location), labeller = labeller(location = location_labs), scales = "free") +
  theme(
    strip.text.y.right = element_text(angle = 0, hjust = 0, size = 7, lineheight = 0.9),
    axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
    axis.text.x.top = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12))
  ) +
  labs(x = "", y = "Flow (cfs)")


ggsave(
  "~/GitHub/Warm_springs_dataproj/figures/flow_monthly_mean.png",
  plot =monthly_flow_means,
  dpi = 600,
  height = 5,
  width = 7
)
