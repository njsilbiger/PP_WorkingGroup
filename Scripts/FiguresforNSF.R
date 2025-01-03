### plots for NSF

library(tidyverse)
library(here)
library(RColorBrewer)
library("viridis")  
library(wesanderson)


# nutrients
nutrients <- read_csv(here("Data","MCR_LTER_Macroalgal_CHN_2005_to_2022_20230713.csv"))

# waves
for4<-read_csv(here("Data","FOR04_PhysoData_Monthly_average.csv"))%>%
  mutate(location = "East")

for5<-read_csv(here("Data","FOR05_PhysoData_Monthly_average.csv"))%>%
  mutate(location = "West")

for1<-read_csv(here("Data","FOR01_PhysoData_Monthly_average.csv")) %>%
  mutate(location = "North")

AllData <- bind_rows(for1, for4, for5) %>%
  mutate(wave_power_adcp =ifelse(wave_power_adcp<0,NA,wave_power_adcp),
         current_speed_bottom_mps = ifelse(current_speed_bottom_mps<0,NA, current_speed_bottom_mps)) %>% # remove the missing data
  mutate(Month = month(time_local)) %>%
  mutate(Season = case_when(Month %in% c(12,1,2)~"Summer",
                            Month %in% c(3,4,5)~"Fall",
                            Month %in% c(6,7,8)~"Winter",
                            Month %in% c(9,10,11)~"Spring"))

AllData %>% 
  filter(Season %in% c("Summer","Winter")) %>%
  group_by(location, Season)%>%
  #mutate(wave_power_adcp = log(wave_power_adcp))%>%
  summarise(wave_mean = mean(wave_power_adcp, na.rm = TRUE),
            wave_se = sd(wave_power_adcp, na.rm = TRUE)/sqrt(n()),
            current_mean = mean(current_speed_bottom_mps, na.rm = TRUE),
            current_se = sd(current_speed_bottom_mps, na.rm = TRUE)/sqrt(n()))%>%
  mutate(location = factor(location, levels = c("West","North","East")))%>%
  ggplot(aes(x = location, y = wave_mean, fill = Season))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin = wave_mean - wave_se, ymax = wave_mean+wave_se), 
                width = 0.1, position = position_dodge(0.9))+
  labs(x = "Side of Island",
       y = "Mean Wave Power (W/m)")+
  scale_fill_brewer(palette = "Set2",direction = -1)+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

ggsave(filename = here("Output","Waves.pdf"), 
       width = 5, height = 4)  


# Nutrients plot
nutrients %>%
  filter(Year > 2020)%>%
  filter(Genus == "Turbinaria",
         Site %in% c("LTER 1", "LTER 4","LTER 6"))%>%
  group_by(Site, Habitat)%>%
  summarise(N_mean = mean(N, na.rm = TRUE),
            N_se = sd(N, na.rm = TRUE)) %>%
  mutate(location = case_when(Site == "LTER 1"~"North",
                              Site == "LTER 4"~"East",
                              Site == "LTER 6"~"West"))%>%
  mutate(location = factor(location, levels = c("West","North","East")))%>%
  ggplot(aes(x = location, y = N_mean, color = Habitat))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = N_mean-N_se, ymax = N_mean+N_se), width = 0.1)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# Gradient color
pal <- wes_palette("Zissou1", 10, type = "continuous")

nutrients %>%
 # filter(Year > 2020)%>%
  filter(Genus == "Turbinaria",
         Site %in% c("LTER 1", "LTER 4","LTER 6"))%>%
  mutate(location = case_when(Site == "LTER 1"~"North",
                              Site == "LTER 4"~"East",
                              Site == "LTER 6"~"West"))%>%
  group_by(location, Site, Habitat)%>%
  summarise(N_mean = mean(N, na.rm = TRUE))%>%
  mutate(location = factor(location, levels = c("West","North","East")),
         Habitat = ifelse(Habitat == "Reef Crest", "Fore Reef",Habitat),
         Habitat = factor(Habitat, levels = c("Fringe","Back Reef","Fore Reef")))%>%
  ggplot(aes(x = location, y = Habitat, fill = N_mean))+
  geom_tile()+
  labs(x = "Side of Island",
       fill = "Mean %N")+
  scale_fill_gradientn(colours = pal) +
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

ggsave(filename = here("Output","Nutrient_tile.pdf"), 
       width = 5, height = 4)  
