### Temperature over time versus community composition ###

### load libraries ####
library(here)
library(tidyverse)
library(googlesheets4)
library(lubridate)

## Temperature LTER 1
#Temp_LTER1<-read_sheet("https://docs.google.com/spreadsheets/d/1YJRyNAeo03xLoThBGX2-BtHpWqUpYXIarmYTsIhw5aI/edit?usp=sharing")
Temp_LTER1<-read_csv(here("Data","MCR_LTER02_BTM_Backreef_Forereef_20250522.csv")) %>%
  filter(reef_type_code == "Backreef") 

Mean_daily_Temp<-Temp_LTER1 %>%
  mutate(date = as.Date(time_local)) %>%
  group_by(date)%>%
  summarise(daily_temp =mean(temperature_c, na.rm = TRUE))

Temp_LTER1 <- Temp_LTER1 %>%
  mutate(Year = year(mdy_hm(time_local)),
         Month = month(mdy_hm(time_local)),
         date = as.Date(time_local)) # extract date

# Calculate summary stats by month
MeanMonth<-Temp_LTER1 %>%
  group_by(Year, Month, date) %>% # group by date
  summarise(dailyMax = max(temperature_c, na.rm = TRUE), # daily max
            dailyMean = mean(temperature_c, na.rm = TRUE), # daily mean
            dailyMin = min(temperature_c, na.rm = TRUE), # daily mean
            dtr = max(temperature_c, na.rm = TRUE) - min(temperature_c, na.rm = TRUE)) %>% # daily range) %>%
  ungroup() %>%
    group_by(Year, Month) %>% # group by Year and month
    summarise(MDMax = mean(dailyMax, na.rm = TRUE), # monthly daily max
              MDMean =mean(dailyMean, na.rm = TRUE),
              MDmin = mean(dailyMin, na.rm = TRUE),
              MDTR = mean(dtr, na.rm = TRUE))

MeanMonth %>%
  mutate(Year_month = ym(paste(Year,"-",Month))) %>%
  pivot_longer(MDMax:MDTR)%>%
  ggplot(aes(x = Year_month, y = value)) +
  geom_point()+
  geom_line()+
  facet_wrap(~name, scale = "free_y")

## Look at just june and january
MeanMonth<-MeanMonth %>%
  filter(Month %in% c(6,1))%>%
  mutate(monthname = case_when(Month == 1 ~ "January",
                               Month == 6 ~ "June"),
         lastyear = Year - 1) 

MeanMonth %>%
  pivot_longer(MDMax:MDTR)%>%
  ggplot(aes(x = Year, y = value, color = monthname)) +
  geom_point()+
  geom_smooth(method = "lm")+
  #geom_line()+
  facet_wrap(~name, scale = "free_y")

### Read in the benthic data ####
# Benthic Cover data for LTER 1
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')%>%
  mutate_at(vars(coral:sub_squares), .funs = as.numeric) %>%
  filter(Site == "LTER 1")

# get the average by time

Benthic_summary<-BenthicCover %>%
  group_by(Year) %>%
  summarise_at(vars(coral:ctb), .funs = function(x){mean(x,na.rm = TRUE)}) %>% # calculate the mean by transect
  mutate(logratio = log(coral/macroalgae)) # calculate log 
### Join benthic data with monthly temperature data

MeanMonth %>%
  select(Year = lastyear, Month:monthname) %>% # make year last year for an offset
  left_join(Benthic_summary) %>%
  drop_na(monthname)%>%
  droplevels()%>%
  ggplot(aes(x = MDTR, y = coral))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Prior Year DTR",
       y = "% coral cover")+
  facet_wrap(~monthname)


## Model January dtr versus coral cover
temp_benth<-Benthic_summary %>% 
  left_join(MeanMonth) %>%
  filter(monthname == "January")%>%
  drop_na(monthname)%>%
  droplevels()

modDTR<-lm(coral~MDTR, data = temp_benth)
anova(modDTR)

### DTR leads to decline in coral cover
temp_benth %>%
  ggplot(aes(x = MDTR, y = coral))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Mean Summer DTR",
       y = "% coral cover")

Benthic_summary %>% 
  left_join(MeanMonth) %>%
 # filter(monthname == "January")%>%
  drop_na(monthname)%>%
  droplevels()%>%
  ggplot(aes(x = Year, y = MDTR))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Average daily temperure range")+
  facet_wrap(~monthname)

## Bring in Primary productivity
## Primary production and calcification data 
PP<-read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing") %>%
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs = as.numeric)%>%
  mutate(NEC_NEP = daily_NEC/daily_NPP, ## calculate NEP/NEC ratio
         NEC_R = night_NEC/daily_R)


AllData<-PP %>% 
  filter(Site == "LTER 1") %>%
  rename(monthname = Month)%>%
  left_join(Benthic_summary) %>% 
  left_join(MeanMonth)

AllData %>%
  ggplot(aes(x = MDMax, y = daily_GPP, color = monthname))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "mean monthly max",
       color = "")

## Model 
modall_June<-lm(daily_GPP~coral+MDMax, data = AllData %>%
             filter(monthname == "June"))
anova(modall_June)
summary(modall_June)
