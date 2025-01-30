library(here)
library(tidyverse)

temp1<-read_csv(here("Data","Temperature_backreef","MCR_LTER01_BottomMountThermistors_20230323.csv"))
temp1 <-temp1 %>%
  filter(sensor_depth_m == 1,
         time_local < ymd_hms("2023-01-21 23:52:00"))

temp2<-read_csv(here("Data","Temperature_backreef","LTER01_BAK_20230115_20240114.csv")) %>%
  filter(sensor_depth_m == 1,
         time_local > ymd_hms("2023-01-21 23:52:00"))

temp3<-read_csv(here("Data","Temperature_backreef","LTER01_BAK_20240113_20240711.csv")) %>%
  filter(sensor_depth_m == 1)

# these data are all over the place with time and frequency... get it to hourly data
Alltemp_hour<-bind_rows(temp1, temp2, temp3) %>%
  mutate(time_local = round_date(time_local, "hour")) %>%
  select(time_local, temperature_c) %>%
  group_by(time_local)%>%
  summarise(hourly_temp_c = mean(temperature_c, na.rm = TRUE))

  rm(temp1, temp2,temp3) # get rid of what we don't need
  
# calcualte different metrics for each year
  Alltemp_year<-Alltemp_hour %>%
    mutate(date = as_date(time_local)) %>%
    group_by(date) %>%
    summarise(daily_min = min(hourly_temp_c, na.rm = TRUE),
              daily_max = max(hourly_temp_c, na.rm = TRUE),
              daily_mean = mean(hourly_temp_c, na.rm = TRUE)) %>%
    ungroup()%>%
    mutate(Year = year(date)) %>%
    group_by(Year)%>%
    summarise(yearly_daily_min = mean(daily_min, na.rm = TRUE),
              yearly_daily_max = mean(daily_max, na.rm = TRUE),
              yearly_daily_mean = mean(daily_mean, na.rm = TRUE))
    
    
  Alltemp_year %>%
    filter(Year < 2024)%>% # not a complete year
    pivot_longer(cols = yearly_daily_min:yearly_daily_mean)%>%
    ggplot(aes(x = Year, value))+
      geom_point()+
    geom_smooth(method = "lm")+
      facet_wrap(~name)
  
  Alltemp_hour %>%
    mutate(Year = year(time_local),
           Month = month(time_local))%>%
    mutate(Season = case_when(Month %in% c(12,1,2)~"Summer",
                              Month %in% c(3,4,5)~"Fall",
                              Month %in% c(6,7,8)~"Winter",
                              Month %in% c(9,10,11)~"Spring"))%>%
    group_by(Year, Season)%>%
    summarise(min_temp = min(hourly_temp_c, na.rm = TRUE)) %>%
    ggplot(aes(x = Year, y = min_temp))+
    geom_point()+
    geom_smooth(method = "lm")+
    facet_wrap(~Season)
    
  yearly_min<-  Alltemp_hour %>%
    mutate(Year = year(time_local))%>%
    group_by(Year)%>%
    summarise(min_temp = min(hourly_temp_c, na.rm = TRUE)) %>%
    filter(Year<2024)

anova(lm(min_temp~Year, data = yearly_min))  
