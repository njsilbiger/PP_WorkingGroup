# Clean the primary productivity data and calculate GP and R by day and Season
# By Nyssa Silbiger
# Updated on 2/2/2025


##### load libraries #############
library(tidyverse)
library(here)


### Read in the data ######

filedir<-here("Data","QC_PP") # file path for the QC PP files
files<-dir(path = filedir, pattern = ".csv", full.names = TRUE)

# bring in all the PP Data
All_PP_data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>%
  mutate(DateTime = mdy_hm(DateTime))


# From 2007-2014 the PP units are in g O2/m2/h. 
#From June 2014 on, the units are mmol O2/m2/h. So those early rates will need to be converted
# 32 g/mol of O2

All_PP_data<-All_PP_data %>%
  mutate(UP_Oxy = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (UP_Oxy/32)*1000, UP_Oxy),
         DN_Oxy = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (DN_Oxy/32)*1000, DN_Oxy),
         PP = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (PP/32)*1000, PP))%>%
  mutate(Date = as_date(DateTime),
         Year = year(DateTime))

# calculate hourly GP and R data 
Daily_R <-All_PP_data %>%
  filter(PAR==0)%>% # pull out all the night data
  group_by(Year, Season, Date) %>% # get the average nighttime respiration by day to add to NEP to calcualte GP
  summarise(R_average = mean(PP, na.rm = TRUE))

# Get NP and calculate GP  
All_PP_data<-All_PP_data %>%
  left_join(Daily_R) %>%
  mutate(GP = PP - R_average) %>%
  mutate(GP = ifelse(PAR == 0, NA, GP)) # remove GP from any of the night data

Seasonal_Averages <-All_PP_data %>%
  mutate(NP = ifelse(PAR == 0, NA, PP),# remove nighttime respiration for average NP
         R = ifelse(PAR == 0, PP, NA) # only include night data for R
         ) %>% 
  group_by(Year, Season) %>%
  summarise(NP_mean = mean(NP, na.rm = TRUE),
            GP_mean = mean(GP, na.rm = TRUE),
            R_mean = mean(R, na.rm = TRUE))
  
  
Seasonal_Averages %>%
  ggplot(aes(x = Year, y = GP_mean,color = Season ))+
  geom_point()

Seasonal_Averages %>%
  ggplot(aes(x = Year, y = R_mean,color = Season ))+
  geom_point()

Seasonal_Averages %>%
  ggplot(aes(x = Year, y = NP_mean,color = Season ))+
  geom_point()
