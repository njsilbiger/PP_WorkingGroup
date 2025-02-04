### weather data

### data data are presented in semi-colon separated files sp use read_csv2

library(here)
library(tidyverse)

# quality control - remove anything that says 2 which is "questionable data being varified"

Data_1950_2023<-read_csv2(here("Data","WeatherData", "Q_987_previous-1950-2023_RR-T-Vent.csv"))  %>%
  select(NOM_USUEL, LAT, LON, ALTI, Date = AAAAMMJJ, Precip_mm = RR, QRR, daily_temp_c = TM, QTM,
         wind_m_s_10 = FFM, QFFM, wind_m_s_2 = FF2M, QFF2M, precip_duration_min = DRR, QDRR) %>%
  mutate(Date = ymd(Date),
         Precip_mm = as.numeric(Precip_mm),
         daily_temp_c = as.numeric(daily_temp_c),
         wind_m_s_2 = as.numeric(wind_m_s_2),
         wind_m_s_10 = as.numeric(wind_m_s_10))

Data_2024_2025<-read_csv(here("Data","WeatherData", "Q_987_latest-2024-2025_RR-T-Vent.csv"))  %>%
  select(NOM_USUEL, LAT, LON, ALTI, Date = AAAAMMJJ, Precip_mm = RR, QRR, daily_temp_c = TM, QTM,
         wind_m_s_10 = FFM, QFFM, wind_m_s_2 = FF2M, QFF2M, precip_duration_min = DRR, QDRR) %>%
  mutate(Date = ymd(Date),
         Precip_mm = as.numeric(Precip_mm),
         daily_temp_c = as.numeric(daily_temp_c),
         wind_m_s_2 = as.numeric(wind_m_s_2),
         wind_m_s_10 = as.numeric(wind_m_s_10))


Data_all_weather<-bind_rows(Data_1950_2023, Data_2024_2025) %>%
  arrange(Date)%>%
  filter(NOM_USUEL %in% c("HAAPITI5","HAAPITI1,PAOPAO 1","AFAAHITI 2","FAAA" )) 
# extract the sites by the SGD sites and Gump and Tahiti for sun


Data_other_2024<-read_csv(here("Data","WeatherData", "Q_987_latest-2024-2025_autres-parametres.csv")) %>%
  select(NOM_USUEL, LAT, LON, ALTI, Date = AAAAMMJJ,sun_duration_min = INST, QINST, global_rad_J_cm2 = GLOT,
         QGLOT, diffuse_rad_J_cm2 = DIFT, QDIFT, direct_rad_J_cm2 = DIRT, QDIRT, UV_J_cm2 = UV, QUV,
         SUn_fraction = SIGMA, QSIGMA, cloud_octa = NB300, QNB300, SST_min = TMERMIN, QTMERMIN, SST_max = TMERMAX, QTMERMAX)%>%
  mutate(Date = ymd(Date),
        SST_min = as.numeric(SST_min),
         SST_max = as.numeric(SST_max),
         direct_rad_J_cm2 = as.numeric(direct_rad_J_cm2),
         diffuse_rad_J_cm2 = as.numeric(diffuse_rad_J_cm2),
         UV_J_cm2 = as.numeric(UV_J_cm2)
         )

  
Data_other_1950_2023<-read_csv2(here("Data","WeatherData", "Q_987_previous-1950-2023_autres-parametres.csv")) %>%
  select(NOM_USUEL, LAT, LON, ALTI, Date = AAAAMMJJ,sun_duration_min = INST, QINST, global_rad_J_cm2 = GLOT,
         QGLOT, diffuse_rad_J_cm2 = DIFT, QDIFT, direct_rad_J_cm2 = DIRT, QDIRT, UV_J_cm2 = UV, QUV,
         SUn_fraction = SIGMA, QSIGMA, cloud_octa = NB300, QNB300, SST_min = TMERMIN, QTMERMIN, SST_max = TMERMAX, QTMERMAX)%>%
  mutate(Date = ymd(Date),
         SST_min = as.numeric(SST_min),
         SST_max = as.numeric(SST_max),
         direct_rad_J_cm2 = as.numeric(direct_rad_J_cm2),
         diffuse_rad_J_cm2 = as.numeric(diffuse_rad_J_cm2),
         UV_J_cm2 = as.numeric(UV_J_cm2))


Data_all_other<-bind_rows(Data_other_1950_2023, Data_other_2024) %>%
  arrange(Date)%>%
  filter(NOM_USUEL %in% c("HAAPITI5","HAAPITI1,PAOPAO 1","AFAAHITI 2","FAAA" )) # extract the sites by the SGD sites and Gump

Data_all<-Data_all_weather %>%
  left_join(Data_all_other)

Data_all %>%
  ggplot(aes(x = Date, y = Precip_mm, color = NOM_USUEL))+
  geom_point()

Data_all %>%
  mutate(Year = year(Date))%>%
  filter(QINST !=2)%>%
  group_by(NOM_USUEL, Year) %>%
  summarise(sun = mean(sun_duration_min, na.rm = TRUE),
            rad = mean(global_rad_J_cm2, na.rm = TRUE),
            cloud = mean(cloud_octa, na.rm = TRUE),
            UV = mean(UV_J_cm2, na.rm = TRUE))%>%
  drop_na(UV)%>%
  ggplot(aes(x = Year, y = UV, color = NOM_USUEL))+
  geom_point()+
  facet_wrap(~NOM_USUEL)
