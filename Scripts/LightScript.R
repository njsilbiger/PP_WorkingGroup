#### Calculate integrated light #####


## load libraries #####
library(googlesheets4)
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)

## Season colors
Cols_seasons<-c("#F27F0c","#429ebd")

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
  mutate(Month = month(meandate),
         Month = case_when(Month==1~1,
                           Month == 3 ~1, # one covid march sample
                           Month == 6~6,
                           Month ==5 ~6)) %>% # all the end of May are called "June" in other datasets
  select(Site, Year, Flow_mean, Flow_SE, Month, Depth_mean, Depth_SE, Solar_mean, Solar_SE) %>%
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
  select(Site, Year, Month = monthname,TotalPAR_mean, TotalPAR_SE, Temp_mean:Solar_SE)

write_csv(Physics_deploy,here("Data","Physics_deploy.csv"))       

ggplot(Physics_deploy, aes(y = TotalPAR_mean, x = Solar_mean, color = Month))+
  geom_point()+
  geom_errorbarh(aes(xmin = Solar_mean-Solar_SE,xmax = Solar_mean+Solar_SE ))+
  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))

ggplot(Physics_deploy, aes(y = TotalPAR_mean, x = Depth_mean, color = Month))+
  geom_point()+
  geom_errorbarh(aes(xmin = Depth_mean-Depth_SE,xmax = Depth_mean+Depth_SE))+
  geom_errorbar(aes(ymin = TotalPAR_mean-TotalPAR_SE,ymax = TotalPAR_mean+TotalPAR_SE ))
