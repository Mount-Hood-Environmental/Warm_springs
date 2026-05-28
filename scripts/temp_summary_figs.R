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

list2env(mhe_data, envir = .GlobalEnv)

### cleaning and summarizing data ###
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

### boxplot of all of the temperatures in spatial groups? or in another way to visualize the temporal trend longitudinally


temp_2024_2025_select <- temp_2024_2025 %>%
  dplyr::select("Date",
                "Location",
                "Water Temp") %>%
  clean_names() %>%
  mutate( water_temperature_c = water_temp)%>%
  dplyr::select(-water_temp)

temp_2025_select <- temp_2025 %>%
  dplyr::select(date,
                site_name,
                water_temp) %>%
  mutate(location = site_name,
         water_temperature_c = water_temp)%>%
  dplyr::select(-water_temp, -site_name)

comb_temps <- rbind(temp_2011_2023,
      temp_2025_select,
      temp_2024_2025_select,
      temp_1990_2023) %>%
  mutate(year = year(date),
         month = month(date))%>%
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
      location == "Middle_Fork_Spring_Creek"|
      location ==  "Middle Fork Hood River - Red Hill Drive"|
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
      location == "Moving Falls - Air" |
      location == "Moving Falls - 100mDS" ~ "Moving Falls",
    location == "Odell Air Barometer" | location == "Odell_Creek" | location == "Odell Creek" ~ "Odell Creek",
    location == "Tony DS" | location == "Tony MS" | location == "Tony_Creek" ~ "Tony Creek",
    location == "Hood River Mouth"  | location == "Hood River Mouth Air" ~ "Hood River Mouth"  ,  
    location == "McGee Creek" ~ location,
    location == "other" ~ "other"
  )) 

spring_measurements_pday <- comb_temps %>%
  group_by(location_name_revised, date) %>%
  mutate(n_daily = n())%>%
  filter(n_daily >= 24,
         !is.na(water_temperature_c))%>%
  filter(water_year_season == "Spring")

summer_measurements_pday <- comb_temps %>%
  group_by(location_name_revised, date) %>%
  mutate(n_daily = n())%>%
  filter(n_daily >= 24,
         !is.na(water_temperature_c))%>%
  filter(water_year_season == "Summer")
wf_measurements_pday <- comb_temps %>%
  group_by(location_name_revised, date) %>%
  mutate(n_daily = n())%>%
  filter(n_daily >= 24,
         !is.na(water_temperature_c))%>%
  filter(water_year_season == "Fall/Winter")


summer_flowplot <- ggplot(summer_measurements_pday, aes(x = as.factor(month), y = water_temperature_c, color = decade))+
scale_x_discrete(labels = c("7" = "July", "8" = "August", "9" = "September")) +
  geom_boxplot(aes(fill = decade), color = "black", outliers =  FALSE)+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~location_name_revised, scales = "free")+
  labs(
    x = "Month",
    y = "Temperature (C)")+ 
  theme(legend.position = "top")

order <- c("10", "11", "12", "1", "2", "3")

wf_flowplot <- ggplot(wf_measurements_pday, aes(x = factor(month, levels = order), y = water_temperature_c, color = decade)) +
  scale_x_discrete(labels = c(
    "3"  = "March",
    "2"  = "February",
    "1"  = "January",
    "11" = "November",
    "12" = "December",
    "10" = "October"
  )) +
  geom_boxplot(aes(fill = decade), color = "black", outliers = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~location_name_revised, scales = "free") +
  labs(x = "Month", y = "Temperature (C)") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

spr_flowplot <- ggplot(spring_measurements_pday, aes(x = as.factor(month), y = water_temperature_c))+
  scale_x_discrete(labels = c("4" = "April", "5" = "May", "6" = "June")) +
  geom_boxplot(aes(fill = decade), color = "black", outliers =  FALSE)+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~location_name_revised, scales = "free")+
  labs(
    x = "Month",
    y = "Temperature (C)")+ 
  theme(legend.position = "top")

ggsave("figures/summer_flowplot.png", summer_flowplot, width = 10, height = 8, dpi = 150)
ggsave("figures/wf_flowplot.png", wf_flowplot, width = 10, height = 8, dpi = 150)
ggsave("figures/spr_flowplot.png", spr_flowplot, width = 10, height = 8, dpi = 150)


