## Timeseries for GEP, R, NEC, and benthic cover of coral and macroalgae
## Created on: 2023-09-08
## Created by: Hannah Merges
## Last updated on: 2023-09-10

#### load libraries ########
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
library(scales)



## read in the data 
PP<-read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing")

PP_edit <- PP %>% 
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs=as.numeric)  


### PREPARE PP AND NEC DATA


PP_long <- PP_edit %>% 
  filter(Site=="LTER 1")

PP_long$daily_R <- PP_long$daily_R * -1

PP_long <- PP_long %>% 
  rename("daily_GEP"="daily_GPP") %>% 
  pivot_longer(cols =c("daily_GEP","daily_R", "daily_NEC", "night_NEC"),
               #, "NPP_SE", "GPP_SE", "R_SE"),
               names_to = "parameter",
               values_to = "mean_parameter") %>%
  select(Year, Month, parameter, mean_parameter)
View(PP_long)

PP_long_se <- PP_edit %>% 
  filter(Site=="LTER 1") %>% 
  select(Year, Month, GPP_SE, R_SE, daily_NEC_SE, night_NEC_SE) %>%
  rename("daily_GEP"="GPP_SE", 
         "daily_R" = "R_SE",
         "daily_NEC" = "daily_NEC_SE",
         "night_NEC" = "night_NEC_SE") %>%
  pivot_longer(cols =c(daily_GEP: night_NEC),
               names_to = "parameter",
               values_to = "se_parameter") %>%
  select(Year, Month, parameter, se_parameter)

PP_long <- PP_long %>%
  left_join(PP_long_se)

vertical.lines <- c(2010, 2016, 2019) # coral reef disturbance events


### PREPARE BENTHIC DATA: CORAL AND MACROALGAE TIMESERIES
# Benthic Cover data
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')

BC_edit <- BenthicCover %>% 
  mutate_at(vars(coral:ctb), .funs=as.numeric) 

mean_BC <- BC_edit %>% 
  filter(Site == "LTER 1") %>% 
  group_by(Year) %>% 
  summarise_at(.vars = vars(coral:macroalgae), .funs = mean, na.rm = TRUE) %>% 
  rename(Coral = coral,
         Macroalgae = macroalgae) %>% 
  pivot_longer(cols = Coral:Macroalgae, names_to = "benthic", values_to = "mean")

se_BC <- BC_edit %>% 
  filter(Site == "LTER 1") %>% 
  group_by(Year) %>%
  summarise_at(.vars = vars(coral:macroalgae), .funs = plotrix::std.error, na.rm = TRUE) %>% 
  rename(Coral = coral,
         Macroalgae = macroalgae) %>% 
  pivot_longer(cols = Coral:Macroalgae, names_to = "benthic", values_to = "SE")

# dataframe with mean and standard error values of percent cover of coral and macroalgae per year
sum_BC <- full_join(mean_BC, se_BC)




################################################################
########### making a publication worthy time series ########
################################################################



# PP

renamePP <- c(daily_GEP = "Daily Mean GEP", January ="Austral Summer", June="Austral Winter", daily_R="Daily Mean R")

PP_long_plot <- PP_long %>% 
  filter(parameter == "daily_GEP" | parameter == "daily_R") %>% 
  mutate(parameter = factor(parameter, levels = c("daily_GEP", "daily_R"))) %>% 
  drop_na(mean_parameter)%>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~parameter*Month, ncol = 2, scales = "fixed", labeller=as_labeller(rename)) + 
  geom_smooth(method = "lm", color="coral3") + 
  geom_point(size=1) +
  geom_vline(xintercept=vertical.lines, color="coral1", linetype="dashed") +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(size=10, face="bold")) +
  theme(axis.title.x = element_blank()) +
  #labs(title = "Time Series Analysis of Productivty Parameters by Season")+ 
  ylab("PP Metabolic Rates") +
  scale_x_continuous(breaks= pretty_breaks(n=3)) +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"))

PP_long_plot


# NEC 

renameNEC <- c(January ="Austral Summer", June="Austral Winter",
               daily_NEC="Daily Mean NEC", night_NEC="Nightly Mean NEC")

NEC_long_plot <- PP_long %>% 
  filter(parameter == "daily_NEC" | parameter == "night_NEC") %>% 
  mutate(parameter = factor(parameter, levels = c("daily_NEC", "night_NEC"))) %>% 
  drop_na(mean_parameter)%>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~parameter*Month, ncol = 2, scales = "fixed", labeller=as_labeller(rename)) + 
  geom_smooth(method = "lm", color="coral3") + 
  geom_point(size=1) +
  geom_vline(xintercept=vertical.lines, color="coral1", linetype="dashed") +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(size=10, face="bold")) +
  theme(axis.title.x = element_blank()) +
  #labs(title = "Time Series Analysis of Productivty Parameters by Season")+ 
  ylab("NEC Metabolic Rates") +
  scale_x_continuous(breaks = pretty_breaks(n=3)) +
  xlim(2007, 2023) +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"))

NEC_long_plot

PP_long_plot / NEC_long_plot



# BENTHIC COVER

BC_long_plot <- sum_BC %>% 
  mutate(parameter = factor(benthic, levels = c("Coral", "Macroalgae"))) %>% 
  drop_na(mean, SE)%>%
  ggplot(aes(x=Year, 
             y=mean)) + 
  facet_wrap(~benthic, ncol = 2, scales = "fixed") + 
  geom_smooth(method = "lm", color="coral3") + 
  geom_point(size=1) +
  geom_vline(xintercept=vertical.lines, color="coral1", linetype="dashed") +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(size=10, face="bold")) +
  #labs(title = "Time Series Analysis of Productivty Parameters by Season")+ 
  ylab("Percent Benthic Cover") +
  scale_x_continuous(breaks = pretty_breaks(n=3)) +
  xlim(2007, 2023) +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"))

BC_long_plot


pp_plot <- PP_long_plot / NEC_long_plot / BC_long_plot

ggsave(here("Output", "PP_NEC_benthic_plot.png"), pp_plot, device = "png", height = 15, width = 13)


