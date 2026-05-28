library(tidyverse)
library(readxl)
library(janitor)
library(sf)

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

coords_lookup <- read.csv("~/GitHub/Warm_springs_dataproj/data/coordinates_lookup_table.csv")
mhe_data <- lapply(excel_sheets("~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx"), 
                   read_excel, path = "~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx")
names(mhe_data) <- excel_sheets("~/GitHub/Warm_springs_dataproj/data/cdms_database/Temp_Discharge_MHE.xlsx")

list2env(mhe_data, envir = .GlobalEnv)
# subbasin and stream layers
HUC_8_sf <- clean_names(st_read("S:/main/data/hydrology/Shape/WBDHU8.shp"))

HR_sub_sf <-  dplyr::filter(HUC_8_sf, huc8 == '17070105')
HR_streams <- st_read("~/GitHub/Warm_springs_dataproj/data/spatial_data/HR_streams.shp")
### cleaning and summarizing data ###
mhe_discharge = clean_names(Discharge_CDMS)

temp_2011_2023 <- clean_names(BarometricPressure_CDMS) %>%
  filter(!is.na(water_temperature_c))%>%
  mutate(date = as.Date(activity_date, format = "%Y-%m-%d"))%>%
  dplyr::select(date, location, water_temperature_c)

temp_1990_2023 <- WaterTemperature_CDMS %>%
  mutate(date = as.Date(`Activity Date`, format = "%Y-%m-%d")) %>%
  dplyr::select(date, Location, `Water Temperature (C)`)%>%
  clean_names()

temp_2011_2023_summary <- temp_2011_2023 %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month, location) %>%
  mutate(
    temp_max = max(water_temperature_c),
    temp_min = min(water_temperature_c),
    temp_avg = mean(water_temperature_c))%>%
  ungroup()%>%
  dplyr::select(-water_temperature_c, -date)%>%
  filter(!is.na(temp_avg))%>%
  distinct()

temp_1990_2023_summary <- temp_1990_2023 %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month, location) %>%
  mutate(
    temp_max = max(water_temperature_c),
    temp_min = min(water_temperature_c),
    temp_avg = mean(water_temperature_c))%>%
  ungroup()%>%
  dplyr::select(-water_temperature_c, -date)%>%
  filter(!is.na(temp_avg))%>%
  distinct()

temp_2025 <- HR_Logger_2_Survey123 %>%
  dplyr::select(c("Date", "Site Name", "Water Temp", 
                  "x", "y"))%>%
  mutate(year = year(Date))

temp_2025 <- clean_names(temp_2025) %>%
  mutate(date = as.Date(date, format= "%Y-%m-%d"))
temp_2024_2025 <- LoggerMonitoring_CDMS %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30", format = "%Y-%m-%d"))


temp_2024_2025_summary <- clean_names(temp_2024_2025) %>%
  dplyr::select(date, site_name, location, water_temp, x, y)%>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(site_name, year, month)%>%
  mutate(temp_max = max(water_temp),
         temp_min = min(water_temp),
         temp_avg = mean(water_temp))%>%
  ungroup()%>%
  dplyr::select(-water_temp, -date)%>%
  filter(!is.na(temp_avg))%>%
  distinct()%>%
  dplyr::select(-x, -y, -site_name)

temp_2025_summary <-   clean_names(temp_2025) %>%
  dplyr::select(date, site_name, water_temp, x, y)%>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(site_name, year, month)%>%
  mutate(temp_max = max(water_temp),
         temp_min = min(water_temp),
         temp_avg = mean(water_temp))%>%
  ungroup()%>%
  dplyr::select(-water_temp, -date)%>%
  filter(!is.na(temp_avg))%>%
  distinct()%>%
  dplyr::select(-x, -y)
#### what are the distributions of flow per year per site
mhe_discharge$flow_cfs <- as.numeric(mhe_discharge$flow_cfs)
mhe_discharge <- clean_names(mhe_discharge)

all_temps <- bind_rows(
  temp_1990_2023_summary %>% mutate(period = "1990-2023"),
  temp_2024_2025_summary %>% mutate(period = "2024-2025"),
  temp_2025_summary      %>% mutate(period = "2025"),
  temp_2011_2023_summary %>% mutate(period = "2011-2023")
) %>%
  mutate(decade = case_when(
    year >= 1990 & year <= 1999 ~ "1990-1999",
    year >= 2000 & year <= 2009 ~ "2000-2009",
    year >= 2010 & year <= 2019 ~ "2010-2019",
    year >= 2020              ~ "2020-2025"),
water_year_season = case_when(
  month %in% c(10, 11, 12, 1, 2, 3) ~ "Fall/Winter",
  month %in% c(4:6) ~ "Spring",
  month %in% c( 7:9) ~ "Summer",
  .default = NA_character_)
) %>%  
  mutate(location_name_revised = case_when(
    location == "East_Fork" | location == "EFID" | location == "East Fork Gauge" ~ "East Fork Hood River",
    location == "Powerdale" ~ "Powerdale",
    location == "Middle_Fork_Spring_Creek" | 
      location == "Middle_Fork_Mixed" |
      location == "Middle_Fork_Tailrace" |
      location == "Middle_Fork" |
      location == "Middle Fork Hood River" ~ "Middle Fork Hood River",
    location == "Rogers_Spring" ~ "Rogers Spring",
    location == "Rogers_Creek" ~ "Rogers Creek",
    location == "Lake_Branch" | location == "Lake Branch" ~ "Lake Branch",
    location == "Baldwin_Creek" ~ "Baldwin Creek",
    location == "Neal_Creek" | location == "Neal Creek" ~ "Neal Creek",
    location == "West_Fork" | location == "West Fork - Bridge" ~ "West Fork Hood River",
    location == "Dog River" | location == "Dog_River" ~ "Dog River",
    location == "Moving Falls - 100m DS" | 
      location == "Moving Falls - Intake" |
      location == "Moving Falls - 100mDS" ~ "Moving Falls",
    location == "Odell Air Barometer" | location == "Odell_Creek" | location == "Odell Creek" ~ "Odell Creek",
    location == "Tony DS" | location == "Tony MS" | location == "Tony_Creek" ~ "Tony Creek",
    location == "other" ~ "other"
  )) 

all_temps <- all_temps %>%
  left_join(coords_lookup, by = c("location_name_revised" = "location"))

all_temps_filtered <- all_temps %>%
  filter(!is.na(x))


         
all_temps_filtered <- all_temps_filtered %>%
 mutate(metrics = "temp")

discharge <- mhe_discharge %>%
  mutate(location_name_revised = 
           case_when(
             location == "East Fork Irrigation District below diversion" ~  "East Fork Hood River",
             location == "Rogers Creek" ~ location,
             location == "Neal Creek" ~ location,
             location == "Odell Creek" ~ location,
             location == "Dog River" ~ location,
             location == "Tony Creek" ~ location),
         metrics = "flow",
         decade = case_when(
           year >= 1990 & year <= 1999 ~ "1990-1999",
           year >= 2000 & year <= 2009 ~ "2000-2009",
           year >= 2010 & year <= 2019 ~ "2010-2019",
           year >= 2020              ~ "2020-2025"),
         month = month(activity_date))%>%
  left_join(coords_lookup, by = c("location_name_revised" = "location"))

measurements_all <- left_join(all_temps_filtered, discharge, by = join_by(decade, month, location_name_revised, year, x, y))

both <- measurements_all %>%
  filter(!is.na(metrics.x) & !is.na(metrics.y))%>%
  mutate(metrics = "Both") %>%
  dplyr::select(c(-metrics.x, -metrics.y))

temp_true <- measurements_all %>%
  filter(!is.na(metrics.x) & is.na(metrics.y))%>%
  mutate(metrics = "Temperature") %>%
  dplyr::select(c(-metrics.x, -metrics.y))

flow_true <- measurements_all %>%
  filter(is.na(metrics.x) & !is.na(metrics.y)) %>%
  mutate(metrics = "Flow") %>%
  dplyr::select(c(-metrics.x, -metrics.y))



# Prepare discharge data
discharge <- mhe_discharge %>%
  dplyr::select(flow_cfs, activity_date, location) %>%
  mutate(
    year = year(activity_date),
    location_name_revised = case_when(
      location == "East Fork Irrigation District below diversion" ~  "East Fork Hood River",
      location == "Rogers Creek" ~ location,
      location == "Neal Creek" ~ location,
      location == "Odell Creek" ~ location,
      location == "Dog River" ~ location,
      location == "Tony Creek" ~ location,
      .default = location
    ),
    decade = case_when(
      year >= 1990 & year <= 1999 ~ "1990-1999",
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020              ~ "2020-2025"
    ),
    month = month(activity_date)
  ) %>%
  left_join(coords_lookup, by = c("location_name_revised" = "location")) %>%
  filter(!is.na(x))

# Full join to capture all records (both, temp only, flow only)
measurements_all <- full_join(
  all_temps %>% select(decade, month, location_name_revised, year, x, y) %>% mutate(has_temp = TRUE),
  discharge %>% select(decade, month, location_name_revised, year, x, y) %>% mutate(has_flow = TRUE),
  by = c("decade", "month", "location_name_revised", "year", "x", "y"),
  relationship = "many-to-many"
) %>%
  mutate(
    has_temp = coalesce(has_temp, FALSE),
    has_flow = coalesce(has_flow, FALSE),
    metrics = case_when(
      has_temp & has_flow ~ "Both",
      has_temp & !has_flow ~ "Temperature",
      !has_temp & has_flow ~ "Flow",
      .default = NA_character_
    )
  ) %>%
  filter(!is.na(metrics)) %>%
  select(-has_temp, -has_flow)

# Alternative approach: if you want to keep them separate
measurements_both <- measurements_all %>% filter(metrics == "Both")
measurements_temp <- measurements_all %>% filter(metrics == "Temperature")
measurements_flow <- measurements_all %>% filter(metrics == "Flow")

# Combine all if needed for mapping
measurements_sf <- measurements_all %>%
  filter(!is.na(x) & !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)%>%
  st_zm()




bbox <- st_bbox(measurements_sf)
margin <- .05

# Remove unnecessary columns
HR_sub_sf <- HR_sub_sf %>% select(geometry)
HR_streams <- HR_streams %>% select(geometry)
measurements_sf <- measurements_sf %>% select(metrics, decade, geometry)

# Now plot and save
main_map <- ggplot() +
  geom_sf(data = HR_sub_sf, fill = "white", color = "black") +
  geom_sf(data = HR_streams, color = "lightblue") +
  geom_sf(data = measurements_sf, 
          aes(fill = metrics),
          size = 2.5, 
          shape = 21,
          alpha = 0.7) +
  coord_sf(
    xlim = c(bbox["xmin"] - margin, bbox["xmax"] + margin),
    ylim = c(bbox["ymin"] - margin, bbox["ymax"] + margin),  
    expand = FALSE
  ) + 
  facet_wrap(~decade) +
  scale_fill_manual(
    values = c(
      "Both" = 'purple4',
      "Temperature" = 'red',
      "Flow" = 'steelblue2'
    )
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/main_map.png", 
       main_map, 
       width = 7, height = 10,  # increase height
       dpi = 600)
