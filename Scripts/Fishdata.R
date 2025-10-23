library(tidyverse)
library(here)

fish<-read_csv(here("Data","MCR_LTER_Annual_Fish_Survey_20250324.csv"))


fish_clean<-fish %>%
  filter(Site == "LTER_1",
         Habitat == "Backreef") %>%
  group_by(Year, Transect)%>%
  summarise(total_biomass = sum(Biomass, na.rm  =TRUE))%>%
  filter(total_biomass<25000) %>% # 3 super high outliers
  group_by(Year)%>%
  summarise(mean_biomass = mean(total_biomass, na.rm  =TRUE),
            se_biomass = sd(total_biomass, na.rm = TRUE)/sqrt(n())) 

fish_clean %>%  
ggplot(aes(x = Year, y = mean_biomass))+
  geom_point()+
  geom_errorbar(aes(x = Year, 
                    ymin = mean_biomass - se_biomass, 
                    ymax = mean_biomass + se_biomass), width = 0.1)+
  geom_smooth(method = "lm")+
  labs(y = "Mean total fish biomass (g)",
       x = "Year")+
  theme_bw()


write_csv(fish_clean, here("Data","fish_clean.csv"))
