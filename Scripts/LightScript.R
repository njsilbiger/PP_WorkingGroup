#### Calculate integrated light #####


## load libraries #####
library(googlesheets4)
library(tidyverse)
library(here)
library(lubridate)

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

IntLight %>%
  ggplot(aes(x = monthname, y =TotalPAR_mean ))+
  geom_boxplot()+
  geom_jitter(width = .2, aes(color = monthname))+
  scale_color_manual(values = Cols_seasons)+
  theme_bw()+
  labs(x = "",
       y = expression(paste("PAR (mol photons", " m"^-2, " d"^-1,")")))+
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
        )

ggsave(here("Output","LightSeason.png"), height = 6, width = 4)
# write the csv
write_csv(IntLight ,here("Data","LightData.csv"))
