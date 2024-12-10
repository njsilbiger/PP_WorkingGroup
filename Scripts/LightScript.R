#### Calculate integrated light #####


## load libraries #####
library(googlesheets4)
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)

## Season colors
Cols_seasons<-c("#F27F0c","#429ebd")

## bring in cloud and PAR data from remote sensing
clouds<-read_csv(here("Data","clouds_MODIS.csv")) %>%
  mutate(date = mdy_hm(time),
         Year = year(date),
         Month = as.numeric(month(date)))%>%
  select(-c(time,date))

PAR<-read_csv(here("Data","MCR_PAR_MODIS.csv")) %>%
  rename(Year = year, Month = month) %>%
  select(!day)%>%
  mutate(Month = as.numeric(Month))

clouds <- clouds %>%
  left_join(PAR)


## read in gump weather ####
weather<-read_csv(here("Data","Gump_MetData_Daily_average_20240222.csv"))

### bring in raw PAR data

## Raw oxygen data -- this file is huge
PP_raw<-read_csv(here("Data","PP_raw_timeseries.csv"))

PP_raw <- PP_raw %>%
  mutate(time_local = mdy_hm(time_local),
         date = as.Date(time_local),
         Year = year(date))

## Calculated integrated light by date and then take the average for each sampling period and site

IntLight<-PP_raw %>%
  filter(PAR>0) %>% # Only grab data with light
  mutate(PAR = PAR/1000000) %>% # conver to mols 3600*12/1000000
  group_by(Site, Year, Deployment, date)%>%
  summarise(TotalPAR = mean(PAR, na.rm = TRUE))%>% # get the daily total PAR
  mutate(TotalPAR = TotalPAR*60*60*12)%>%
  group_by(Site, Year, Deployment) %>% # get average by deployment
  summarise(TotalPAR_mean = mean(TotalPAR, na.rm = TRUE),
            TotalPAR_SE = sd(TotalPAR, na.rm = TRUE)/n(),
            meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, TotalPAR_mean, TotalPAR_SE, Month) %>%
  droplevels()%>%
  mutate(monthname = ifelse(Month == 1, "January","June"))

### plot intergrated light over time
IntLight %>%
  ggplot(aes(x = Year, y = TotalPAR_mean, color = monthname))+
  geom_point()+
  geom_errorbar(aes(x = Year, ymin = TotalPAR_mean - TotalPAR_SE,
                    ymax = TotalPAR_mean+TotalPAR_SE), width = 0.1)+
  facet_wrap(~Site)

Fig_light<-IntLight %>%
  ggplot(aes(x = monthname, y =TotalPAR_mean ))+
  geom_boxplot()+
  geom_jitter(width = .2, aes(color = monthname))+
  scale_color_manual(values = Cols_seasons)+
  theme_bw()+
  labs(x = "",
       y = expression(paste("PAR (mol photons", " m"^-2, " d"^-1,")")))+
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank()
        )

ggsave(here("Output","LightSeason.png"), height = 6, width = 4)
# write the csv
write_csv(IntLight ,here("Data","LightData.csv"))

### get average temperature for each deployment 
Temp_deploy<-PP_raw %>%
  mutate(mean_Temp_site = (UP_Temp+DN_Temp)/2) %>% # average up and down
  group_by(Site, Year, Deployment, date)%>%
  summarise(Temperature_mean = mean(mean_Temp_site, na.rm = TRUE),
            Temperature_max = max(mean_Temp_site, na.rm = TRUE)) %>%
  group_by(Site, Year, Deployment) %>% # get average by deployment
  summarise(Temp_mean = mean(Temperature_mean, na.rm = TRUE),
            Temp_SE = sd(Temperature_mean, na.rm = TRUE)/n(),
            meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, Temp_mean, Temp_SE, Month) %>%
  droplevels()%>%
  mutate(monthname = ifelse(Month == 1, "January","June"))

write_csv(Temp_deploy, here("Data","Temp_deployment.csv"))

#### Temperature boxplot

Fig_temp<-Temp_deploy %>%
  ggplot(aes(x = monthname, y =Temp_mean ))+
  geom_boxplot()+
  geom_jitter(width = .2, aes(color = monthname))+
  scale_color_manual(values = Cols_seasons)+
  theme_bw()+
  labs(x = "",
       y = expression(paste("Temperature (", ~degree, "C)")))+
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank()
  )
ggsave(here("Output","TempSeason.png"), height = 6, width  = 4)

### Calculate the average raw speed per deployment ####
RawSpeed<-PP_raw %>%
   group_by(Site, Year, Deployment, date)%>%
  summarise(Speed_mean = mean(Raw_Speed_UP, na.rm = TRUE),
            Depth_m = mean(Depth_UP, na.rm = TRUE),
            Solar_m = mean(mean_solar_rad_kwpm2, na.rm = TRUE)) %>%
  group_by(Site, Year, Deployment) %>% # get average by deployment
  summarise(Flow_mean = mean(Speed_mean, na.rm = TRUE),
            Flow_SE = sd(Speed_mean, na.rm = TRUE)/n(),
            Depth_mean = mean(Depth_m, na.rm = TRUE),
            Depth_SE = sd(Depth_m, na.rm = TRUE)/sqrt(n()),
            Solar_mean = mean(Solar_m, na.rm = TRUE),
            Solar_SE = sd(Solar_m, na.rm = TRUE)/sqrt(n()),
            meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate)) %>%
  left_join(clouds)%>%
  mutate(Month = case_when(Month==1~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, Flow_mean, Flow_SE, Month, Depth_mean, Depth_SE, Solar_mean, Solar_SE, mean_clouds,mean_MO_par) %>%
  droplevels()%>%
  mutate(monthname = ifelse(Month == 1, "January","June"))

### Speed plots ####
Fig_speed<-RawSpeed %>%
  ggplot(aes(x = monthname, y =Flow_mean ))+
  geom_boxplot()+
  geom_jitter(width = .2, aes(color = monthname))+
  scale_color_manual(values = Cols_seasons)+
  theme_bw()+
  labs(x = "",
       y = expression(paste("Flow Rate (","m"^2," s"^-1,")")))+
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
  )
ggsave(here("Output","FlowSeason.png"), height = 6, width  = 4)

Fig_light/Fig_temp/Fig_speed

ggsave(here("Output","Physics.png"), width = 5, height = 10)

## Bring together the local physics data ####
Physics_deploy <-IntLight %>%
  left_join(Temp_deploy)%>%
  left_join(RawSpeed) %>%
  select(Site, Year, Month = monthname,TotalPAR_mean, TotalPAR_SE, Temp_mean:mean_MO_par)

write_csv(Physics_deploy,here("Data","Physics_deploy.csv"))       

pl1<-ggplot(Physics_deploy, aes(y = TotalPAR_mean, x = Solar_mean))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")+
  geom_errorbarh(aes(xmin = Solar_mean-Solar_SE,xmax = Solar_mean+Solar_SE ))+
  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))+
  theme_bw()+
  theme(legend.position = "none")

pl2<-ggplot(Physics_deploy, aes(y = TotalPAR_mean, x = Depth_mean))+
  geom_point(aes(color =Month))+
  geom_smooth(method = "lm")+
  geom_errorbarh(aes(xmin = Depth_mean-Depth_SE,xmax = Depth_mean+Depth_SE))+
  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))+
  theme_bw()+
  theme(legend.position = "none")

pl3<-ggplot(Physics_deploy, aes(x = Year, y = Depth_mean))+
  geom_point(aes(color =Month))+
  geom_smooth(method = "lm")+
  labs(y = "Depth (m)")+
  geom_errorbar(aes(ymin = Depth_mean-Depth_SE,ymax = Depth_mean+Depth_SE), width = 0)+
  theme_bw()

pl4<-ggplot(Physics_deploy, aes(y = TotalPAR_mean, x = mean_MO_par))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")+
  #eom_errorbarh(aes(xmin = Solar_mean-Solar_SE,xmax = Solar_mean+Solar_SE ))+
  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))+
  labs(x = "Mean Monthly PAR from MODIS",
       y = "Mean PAR from Reef")+
  theme_bw()+
  theme(legend.position = "none")


pl1|pl2|pl3+plot_layout(guides = "collect")
ggsave(here("Output","Light-depth.png"), width = 10, height = 8)

weather %>%
  mutate(year = year(time_local),
         month = month(time_local),
         year_month = round_date(time_local, "month"))%>%
       #  year_month = paste(year,"-",month))%>% 
  filter(mean_solar_rad_kwpm2>0)%>%
  group_by(year_month)%>%
  summarise(mean_solar = mean(mean_solar_rad_kwpm2, na.rm = TRUE))%>%
  ggplot(aes(x = year_month, y = mean_solar))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Time",
       y = expression(paste("Mean Monthly Solar Radiation (kw m"^-2,")")))+
  theme_bw()

petedata<-read_csv(here("Data","In_Situ_Light_20240216.csv"))

petedata %>%
  mutate(date = ymd(`DateTime_GMT-07:00`),
         yearmonth = round_date(date, "month"))%>%
  group_by(yearmonth)%>%
  summarise(mean_light = mean(MAX_surface_PAR, na.rm = TRUE))%>%
  ggplot(aes(x = yearmonth, y = mean_light))+
  geom_point()

pH<-read_csv(here("Data","MCR_pH.csv"))

pH_mean<-pH %>%
  mutate(date = mdy_hm(timestamp_gmt),
         monthyear = round_date(date, "month"),
         Month = month(date, abbr = FALSE, label = TRUE),
         Year = year(date))%>%
  filter(ph_totalscale>7.5,
         ph_totalscale<9)%>%
  group_by(Year, Month)%>%
  summarise(pHmean = mean(ph_totalscale, na.rm = TRUE))

pH_mean%>%
  ggplot(aes(x = month, y = pHmean))+
  geom_point()

Physics_deploy<-Physics_deploy %>%
  left_join(pH_mean)
write_csv(Physics_deploy,here("Data","Physics_deploy.csv"))       
