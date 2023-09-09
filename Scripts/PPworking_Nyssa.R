#### Working group for primary production ####
### Nyssa Silbiger ####
### 9/8/2023 #####


## load libraries #####
library(googlesheets4)
library(tidyverse)

## Read in the different data sheets ####
#Read google sheets data into R.

## Primary production and calcification data 
PP<-read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing") %>%
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs = as.numeric)%>%
  mutate(NEC_NEP = daily_NEC/daily_NPP, ## calculate NEP/NEC ratio
         NEC_R = night_NEC/daily_R)

# Read in Benthic data 
# Benthic Cover data
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')%>%
  mutate_at(vars(coral:sub_squares), .funs = as.numeric)

### look at relationship between NEP and NEC over time ###

PP %>% 
  filter(Site == "LTER 1") %>%
  ggplot()+
  geom_point(aes(x = Year, y = daily_NEC), color = "grey")+
  geom_point(aes(x = Year, y = night_NEC), color = "black")+
  facet_wrap(~Month)

## NEC/NEC for day
PP %>% 
  filter(Site == "LTER 1",
         Year != 2015) %>%
  ggplot(aes(x = Year, y = NEC_NEP, color = Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

# NEP/NEC for night  
PP %>% 
  filter(Site == "LTER 1",
         Year != 2015) %>%
  ggplot(aes(x = Year, y = NEC_R, color = Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

## NEP/NEC ratio decreases over time in the summer, but not winter

### Look at the ratio of coral to algae vs NEP and NEC
Benthic_summary<-BenthicCover %>%
  group_by(Year, Site) %>%
  summarise_at(vars(coral:ctb), .funs = function(x){mean(x,na.rm = TRUE)}) %>% # calculate the mean by transect
  mutate(logratio = log(coral/macroalgae)) # calculate log ratio


Benthic_summary %>%
  ggplot(aes(x = Year, y = logratio))+
  geom_point()+
  facet_wrap(~Site)

Benthic_summary %>%
full_join(PP)  %>%
  filter(Site == "LTER 1")%>%
  drop_na(Month)%>%
  filter(Year != 2015) %>%
  ggplot(aes(x = logratio, y = NEC_NEP))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "log ratio of coral to algae")+
 lims(x = c(-1.5,1.5))+
  facet_wrap(~Month)
