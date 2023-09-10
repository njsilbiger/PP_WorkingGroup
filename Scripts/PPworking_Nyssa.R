#### Working group for primary production ####
### Nyssa Silbiger ####
### 9/8/2023 #####


## load libraries #####
library(googlesheets4)
library(tidyverse)
library(rphylopic)
library(patchwork)
library(here)

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

PP %>% 
  filter(Site == "LTER 1") %>%
  ggplot(aes(x = Year, y = daily_NEC-night_NEC))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scales = "free")

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
#  filter(Year != 2015) %>%
  ggplot(aes(x = logratio, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "log ratio of coral to algae")+
# lims(x = c(-1.5,1.5))+
  facet_wrap(~Month)

#### PP versus percent cover #####
LTER1<-Benthic_summary %>%
  full_join(PP)  %>%
  filter(Site == "LTER 1") %>%
  drop_na(Month) %>%
  droplevels()

# Add a coral

uuid <- get_uuid(name = "Acropora donei", n = 1)
# Get the image for that uuid
img <- get_phylopic(uuid = uuid)

# algae
uuid2 <- get_uuid(name = "Fucus vesiculosus", n = 1)
img2 <- get_phylopic(uuid = uuid2)



LTER1 %>%
  rename(Coral=coral,
         Macroalgae = macroalgae)%>%
  pivot_longer(cols = c(Coral:ctb), names_to = "Benthos") %>% 
  filter(Benthos %in% c("Coral","Macroalgae")) %>%
    ggplot(aes(x = value, y = daily_GPP))+
  geom_point(color = "grey30")+
  geom_smooth(method = "lm", color = "black")+
  add_phylopic(x = 40, y = 2000, img = img, color = "coral", ysize = 4) +
#  geom_phylopic(aes(x = 40, y = 2000), img = img,
 #               color = "purple", size = 0.25)+
  labs(x = "% Benthic Cover",
       y = expression(paste("Daily GPP (mmol O", " m"^-2, " d"^-1,")")))+
  theme_bw()+
  facet_wrap(~Benthos*Month, ncol = 2)+
  theme(strip.background = element_blank())



###  
CoralMod<-lm(daily_GPP~coral*Month, data = LTER1)
anova(CoralMod)

MacroMod<-lm(daily_GPP~macroalgae*Month, data = LTER1)
anova(MacroMod)


## Just coral
CoralGPP<-LTER1 %>%
  rename(Coral=coral,
         Macroalgae = macroalgae)%>%
  ggplot(aes(x = Coral, y = daily_GPP))+
  geom_point(color = "grey30")+
  geom_smooth(method = "lm", color = "black")+
  geom_phylopic(aes(x = 35, y = 2300), img = img,
                color = "#F0B49D", size = 400)+
    #  geom_phylopic(aes(x = 40, y = 2000), img = img,
  #               color = "purple", size = 0.25)+
  lims(x = c(0,40),
       y = c(0,2500))+
  labs(x = "% Coral Cover",
       #x = "",
       y = expression(paste("Daily GPP (mmol O"^2, " m"^-2, " d"^-1,")")))+
  theme_bw()+
  facet_wrap(~Month, ncol = 2)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

## Macroalgae
MacroGPP<-LTER1 %>%
  rename(Coral=coral,
         Macroalgae = macroalgae)%>%
  ggplot(aes(x = Macroalgae, y = daily_GPP))+
  geom_point(color = "grey30")+
  geom_smooth(method = "lm", color = "black")+
  geom_phylopic(aes(x = 35, y = 2300), img = img2,
                color = "#696112", size = 400)+
  #  geom_phylopic(aes(x = 40, y = 2000), img = img,
  #               color = "purple", size = 0.25)+
  labs(x = "% Macroalgae Cover",
       y = expression(paste("Daily GPP (mmol O"^2, " m"^-2, " d"^-1,")")))+
  theme_bw()+
  lims(x = c(0,40),
       y = c(0,2500))+
  facet_wrap(~Month, ncol = 2)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

CoralGPP/MacroGPP
ggsave(here("Output","GPP_coralMacro.png"), width = 10, height = 10)
