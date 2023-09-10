## Test Script to Visualize Data 
## Created on: 2023-09-08
## Created by: Hannah Merges
## Last updated on: 2023-09-08

#### load libraries ########
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
library(scales)


######## View dataframes to take a look at everything ########
##View(fish)
#View(PP)
#View(inverts)

## read in the data 
PP<-read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing")

PP_edit <- PP %>% 
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs=as.numeric)  
  

### start with PP df ##### 
#PP_edit <- PP %>% 
 # filter(Site=="LTER 1")

#View(PP_edit)

#plot1 <- PP_edit %>% 
 # ggplot(aes(x=Year, 
           # y=daily_NPP, 
            # color=Year)) +
  #geom_point()+
  #facet_wrap(~Month) +
  #labs(title="Test")

##################################
#### working with the biology ##### 
####################################

# what about lining up NEP ratios with disturbance levels? 
# look at change in benthic cover over time and line that up with NEP 
# temp data taken every 20 minutes since 2005 
# seasonal changes, when physical parameters lull, biology peaks 
# ancova - to see same pattern between summer and winter 

## GP, NP, R (y) and percent cover (x) 
## fish and macroalgae (x) --> fish patterns decline or incline over time 
  ## look at Russ and Sally's papers to see if they already have a figure 
  ## look at herbivores to see patterns - driving macroalgae patterns 


# GEP, NEP, R separate out seasons for each year of each disturbance 
# lines for each year, faceted at each season or separated into 2 different plots? 
# mean with errors over time 
# make sure dates are true years 
# geom line as opposed to regression 
# geom v line at different years where things 
# 2020 - post cots/hurricane 
# 2016 - light bleaching 
# 2019 - bleaching 
# patchwork to put plots together 
# pivot data so it is long, facet by variable


## pivoting to longer format to include all the parameters in one column with their values in another


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


## now trying to plot each one independently and patch together  
# plot_NPP <- PP_long %>% 
#   filter(parameter=="daily_NEP") %>%
#   ggplot(aes(x=Year, 
#              y=mean_parameter)) + 
#   facet_wrap(~Month) + 
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
#   theme_bw() + 
#   labs(title = "NPP_mean") + 
#   theme(plot.title = element_text(hjust = 0.5))


plot_GEP <- PP_long %>% 
  filter(parameter=="daily_GEP") %>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~Month) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
  theme_bw() + 
  labs(title = "GEP_mean") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_R <- PP_long %>% 
  filter(parameter=="daily_R") %>%
  drop_na(mean_parameter)%>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~Month) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = Year, ymin = mean_parameter-se_parameter, ymax = mean_parameter+se_parameter),
                width = 0.1)+
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
  theme_bw() + 
  labs(title = "R_mean") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_NECd <- PP_long %>% 
  filter(parameter=="daily_NEC") %>%
  drop_na(mean_parameter)%>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~Month) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = Year, ymin = mean_parameter-se_parameter, ymax = mean_parameter+se_parameter),
                width = 0.1)+
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
  theme_bw() + 
  labs(title = "NEC_daily_mean") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_NECn <- PP_long %>% 
  filter(parameter=="night_NEC") %>%
  drop_na(mean_parameter)%>%
  ggplot(aes(x=Year, 
             y=mean_parameter)) + 
  facet_wrap(~Month) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = Year, ymin = mean_parameter-se_parameter, ymax = mean_parameter+se_parameter),
                width = 0.1)+
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
  theme_bw() + 
  labs(title = "NEC_nightly_mean") + 
  theme(plot.title = element_text(hjust = 0.5))

#### THIS IS THE ACTUAL PLOT #### 
  
  PP_long %>% 
    #filter(parameter=="daily_R") %>%
    drop_na(mean_parameter)%>%
    ggplot(aes(x=Year, 
               y=mean_parameter)) + 
    facet_wrap(~parameter*Month, ncol = 2, scales = "free_y") + 
    geom_line() +
    geom_point() +
   geom_errorbar(aes(x = Year, ymin = mean_parameter-se_parameter, ymax = mean_parameter+se_parameter),
                 width = 0.1)+
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
    theme_bw() + 
   # labs(title = "R_mean") + 
    theme(plot.title = element_text(hjust = 0.5))
  
dailymeans <- plot_R / plot_GEP / plot_NECd / plot_NECn ##divided by sign = stacks on top of each others 
# + sign adds side by side 


### PREPARE BENTHIC DATA: CORAL AND MACROALGAE TIMESERIES
# Benthic Cover data
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')

BC_edit <- BenthicCover %>% 
  mutate_at(vars(coral:ctb), .funs=as.numeric) 

sum_BC <- BC_edit %>% 
  filter(Site == "LTER 1") %>% 
  group_by(Year) %>% # not by patch
  summarise_at(.vars = vars(coral:ctb), .funs = c(mean, plotrix::std.error), na.rm = TRUE)

mean_BC <- sum_BC %>% 
  select(Year:macroalgae_fn1) %>% 
  rename(Coral = coral_fn1,
         Macroalgae = macroalgae_fn1) %>% 
  pivot_longer(cols = Coral:Macroalgae, names_to = "benthic", values_to = "mean")

se_BC <- sum_BC %>% 
  select(Year, coral_fn2, macroalgae_fn2) %>% 
  rename(Coral = coral_fn2,
         Macroalgae = macroalgae_fn2) %>% 
  pivot_longer(cols = Coral:Macroalgae, names_to = "benthic", values_to = "SE")

# dataframe with mean and standard error values of percent cover of coral and macroalgae per year
sum_BC <- full_join(mean_BC, se_BC)



################################################################
########### making a publication worthy time series ########
################################################################

#use a regression 
# should I look at this with a p-value? 

## make R positive 
#PP_long <- PP_long %>% 
 # drop_na(mean_parameter) %>% 
 # abs(mean_parameter)



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


PP_long_plot / NEC_long_plot
BC_long_plot


(PP_long_plot / NEC_long_plot) + BC_long_plot




############################################
##### Switching to benthic cover ###########
#############################################

### would be interesting to see the difference between coral rubble and live coral cover 

## edit from being a list 
BC_edit <- BenthicCover %>% 
  mutate_at(vars(coral:ctb), .funs=as.numeric)  

## also bring in NEC data 


## pivot to get total percentages
BC <- BC_edit %>% 
  filter(Site== "LTER 1") %>% 
  select(Year, coral, sand, ctb) %>% 
  pivot_longer(cols=c(coral, sand, ctb), 
               names_to="substrate", 
               values_to="cover")

BC_log <- BC %>% 
  group_by(substrate) %>% 
  summarise(log_cover=log((cover/(1-cover))))
  


BCplot <- BC %>% 
  ggplot(aes(x=Year, 
             y=cover, 
             color=substrate)) + 
  geom_point() + 
  facet_wrap(~substrate)
  
   



PP_long <- PP_long %>% 
  pivot_longer(cols =c("daily_NPP","daily_GPP","daily_R", "PR"),
               #, "NPP_SE", "GPP_SE", "R_SE"),
               names_to = "parameter",
               values_to = "mean_parameter")






  
  
  
  
  