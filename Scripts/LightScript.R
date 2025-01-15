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

# bring in monthly SST
monthly_sst<-read_csv(here("Data","SST_year_month.csv"))
yearly_sst<-read_csv(here("Data","SST_year.csv"))

clouds <- clouds %>%
  left_join(PAR) %>%
  left_join(monthly_sst)

clouds %>% 
  ggplot(aes(x = mean_SST, y = mean_clouds))+
  geom_point()+
  labs(y = "Mean Monthy Cloud Cover",
       x = "Mean Monthly SST"~degree~"C)")+
  theme_bw()
ggsave(here("Output","temp_clouds.png"), height = 6, width = 6)

## read in gump weather ####
weather<-read_csv(here("Data","Gump_MetData_Daily_average_20240222.csv"))

### bring in raw PAR data

## Raw oxygen data -- this file is huge
PP_raw<-read_csv(here("Data","PP_raw_timeseries_updated.csv"))

PP_raw <- PP_raw %>%
  mutate(time_local = mdy_hm(time_local),
         date = as.Date(time_local),
         Year = year(date)) %>%
  filter(!is.na(UP_Oxy) | !is.na(DN_Oxy)) %>% # drop the data where there was no oxygen because then there was no NPP data for those times
  mutate(Day_Night = ifelse(PAR > 10,"Day", "Night")) # the way they coded the data to process made some of the night values NA when it should be 0

## Calculated integrated light by date and then take the average for each sampling period and site

IntLight<-PP_raw %>%
  filter(PAR>0) %>% # Only grab data with light
  mutate(PAR = PAR/1000000) %>% # conver to mols 3600*12/1000000
  group_by(Site, Year, Deployment, date)%>%
  summarise(TotalPAR = mean(PAR, na.rm = TRUE))%>% # get the daily total PAR
  mutate(TotalPAR = TotalPAR*60*60*12)%>%
  group_by(Site, Year, Deployment) %>% # get average by deployment
  summarise(TotalPAR_mean = mean(TotalPAR, na.rm = TRUE),
            TotalPAR_SE = sd(TotalPAR, na.rm = TRUE)/sqrt(n()),
            meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 2~1,
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
            #Temperature_max = max(mean_Temp_site, na.rm = TRUE)
            ) %>%
  group_by(Site, Year, Deployment) %>% # get average by deployment
  summarise(#Temp_max = mean(Temperature_max, na.rm = TRUE),
            #Temp_max_SE = sd(Temperature_max, na.rm = TRUE)/sqrt(n),
            Temp_mean = mean(Temperature_mean, na.rm = TRUE),
            Temp_SE = sd(Temperature_mean, na.rm = TRUE)/sqrt(n()),
            meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 2~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, Temp_mean, Temp_SE, Month) %>%
  droplevels()%>%
  mutate(monthname = ifelse(Month == 1, "January","June"))

## calcualte day vs nighttime temperature
Temp_deploy_daynight <- PP_raw %>%
  mutate(mean_Temp_site = (UP_Temp+DN_Temp)/2) %>% # average up and down
  group_by(Site, Year, Deployment, date, Day_Night)%>%
  summarise(Temperature_mean = mean(mean_Temp_site, na.rm = TRUE),
            #Temperature_max = max(mean_Temp_site, na.rm = TRUE)
  ) %>%
  drop_na(Day_Night) %>%
  group_by(Site, Year, Deployment, Day_Night) %>% # get average by deployment
  summarise(#Temp_max = mean(Temperature_max, na.rm = TRUE),
    #Temp_max_SE = sd(Temperature_max, na.rm = TRUE)/sqrt(n),
    Temp_mean = mean(Temperature_mean, na.rm = TRUE),
    Temp_SE = sd(Temperature_mean, na.rm = TRUE)/sqrt(n()),
    meandate = mean(date, na.rm = TRUE)) %>% # get mean date to be able to easily extract month
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 2~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  ungroup()%>%
  select(Site, Year, Temp_mean, Temp_SE, Month, Day_Night) %>%
  droplevels()%>%
  mutate(monthname = ifelse(Month == 1, "January","June")) %>%
  pivot_wider(names_from = Day_Night,
              values_from = Temp_mean:Temp_SE)


Temp_deploy<-Temp_deploy %>%
  left_join(Temp_deploy_daynight)

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
                           Month == 2~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, Flow_mean, Flow_SE, Month, Depth_mean, Depth_SE, Solar_mean, Solar_SE, mean_clouds,mean_MO_par,mean_SST, max_SST ) %>%
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
  select(Site, Year, Month = monthname,TotalPAR_mean, 
         TotalPAR_SE, Temp_mean:max_SST)

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

ggplot(Physics_deploy, aes(x = Year, y = mean_MO_par))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")+
  #eom_errorbarh(aes(xmin = Solar_mean-Solar_SE,xmax = Solar_mean+Solar_SE ))+
#  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))+
  labs(x = "Year",
       y = "Mean PAR MODIS")+
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
  filter(!year %in% c(2021,2022))%>%
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
  ggplot(aes(x = Month, y = pHmean))+
  geom_point()

Physics_deploy<-Physics_deploy %>%
  left_join(pH_mean)
write_csv(Physics_deploy,here("Data","Physics_deploy.csv"))       

p1<-clouds %>%
  group_by(Year)%>%
  summarise(mean_cloud = mean(mean_clouds, na.rm = TRUE),
            mean_PAR = mean(mean_MO_par, na.rm = TRUE))%>%
  ggplot(aes(x = Year, y = mean_PAR))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Mean PAR from MODIS")+
  theme_bw()

ggsave(here("Output","MODIS_PAR.png"))

#p1a<-clouds %>%
 # group_by(Year, Month)%>%
#  filter(Month %in% c(1,6))%>%
#  mutate(Month = factor(Month))%>%
 # summarise(mean_cloud = mean(mean_clouds, na.rm = TRUE),
  #          mean_PAR = mean(mean_MO_par, na.rm = TRUE))%>%
  #ggplot(aes(x = Year, y = mean_PAR, color = Month))+
  #geom_point()+
  #geom_smooth(method = "lm")+
  #labs(y = "Mean PAR from MODIS")+
  #theme_bw()


p2<-Physics_deploy %>%
  ggplot((aes(x = Year, y = TotalPAR_mean, color = Month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Total daily PAR subsurface")+
  theme_bw()+
  theme(legend.position  = "none")

p3<-Physics_deploy %>%
  ggplot((aes(x = Year, y = Solar_mean, color = Month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Mean Solar Radiation Gump")+
  theme_bw()

p4<-clouds %>%
  mutate(Season = case_when(Month %in% c(12,1,2)~"Summer",
                           Month %in% c(3,4,5)~"Fall",
                           Month %in% c(6,7,8)~"Winter",
                           Month %in% c(9,10,11)~"Spring"))%>%
  #group_by(Year)%>%
  #summarise(mean_cloud = mean(mean_clouds, na.rm = TRUE),
   #         mean_PAR = mean(mean_MO_par, na.rm = TRUE),
    #        mean_sst = mean(mean_SST))%>%
  ggplot(aes(y = mean_MO_par, x = mean_clouds, color = factor(Month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  #labs(y = "mean cloud")+
  theme_bw()+
  facet_wrap(~Season, scales = "free")


p1|p2|p3 + plot_layout(guides = "collect")
ggsave(here("Output","PARall.png"), width = 10, height = 5)


Physics_deploy %>% 
  ggplot(aes(x = mean_MO_par, y = mean_clouds, color = Month))+
  geom_point()+facet_wrap(~Month, scales = "free")

Physics_deploy %>% 
  ggplot(aes(y = TotalPAR_mean, x = mean_clouds, color = Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scales = "free")



Physics_deploy %>%
  select(Temp_mean, TotalPAR_mean, Flow_mean, Year, Month, Site)%>%
  pivot_longer(Temp_mean:Flow_mean) %>%
  ggplot(aes(x = Year, y = value, color = Site))+
  geom_point()+
  facet_wrap(Month~name, scales = "free_y")


### correlate Pete's data with Gump.. Nest Modis/Gump/In Situ from Bob and see if these are relatedc
# Look at Tahiti data for solar radiation
# Look at Aerosols
# Look at UPF data
# aerosol optical depth
# ARGO floats also have light
# Look at HIMB 
# Send GPS points for 20 High volcanic islands to Stephane to extract MODIS data
# Look at Heron Island Data and Lizard
# Look into PAR and crop yields changing
# Look at IPCC working group on solar radiation