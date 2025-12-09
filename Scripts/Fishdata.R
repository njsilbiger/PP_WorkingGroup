library(tidyverse)
library(here)

fish<-read_csv(here("Data","MCR_LTER_Annual_Fish_Survey_20250324.csv"))


fish_clean<-fish %>%
  filter(Site == "LTER_1",
         Habitat == "Backreef") %>%
  filter(Biomass < 8000) %>% # there are 3 big sharks in the entire dataset that are biassing the biomass data.  Dropping them
  mutate(Biomass = ifelse(Biomass<0, NA, Biomass)) %>% # negative values are used for missing data
  #mutate(fish_g_m2 = (Biomass/50*Swath)) %>%
  group_by(Year)%>%
  summarise(total_fish_g_m2 = sum(Biomass, na.rm  =TRUE)/1200) # 300 m2 by 4 transects is 1200 m2 per year
 # filter(total_biomass<25000) %>% # 3 super high outliers
#  group_by(Year)%>%
#  summarise(mean_biomass = mean(total_biomass, na.rm  =TRUE),
#            se_biomass = sd(total_biomass, na.rm = TRUE)/sqrt(n())) 

fish_clean %>%  
ggplot(aes(x = Year, y = total_fish_g_m2))+
  geom_point()+
 # geom_errorbar(aes(x = Year, 
#                    ymin = mean_biomass - se_biomass, 
#                    ymax = mean_biomass + se_biomass), width = 0.1)+
  geom_smooth(method = "lm")+
  labs(y = "Mean total fish biomass (g/ m2)",
       x = "Year")+
  theme_bw()

## group by trophic levels
fish_summary<-fish %>%
  filter(Site == "LTER_1",
         Habitat == "Backreef") %>%
  filter(Biomass < 8000) %>% # there are 3 big sharks in the entire dataset that are biassing the biomass data.  Dropping them
  mutate(Biomass = ifelse(Biomass<0, NA, Biomass)) %>% # negative values are used for missing data
  #mutate(fish_g_m2 = (Biomass/50*Swath)) %>%
  mutate(trophic_new = case_when(Fine_Trophic %in% c("Brusher", "Browser","Excavator","Scraper")~"Benefits from dead coral",
                                 Fine_Trophic  == "Corallivore"~"Corallivore",
                                 .default = "Other"
                                )
  )%>%
 # mutate(fish_kg_m2 = (Biomass/50*Swath)/1000)%>%
  group_by(Year, trophic_new) %>%
  summarise(fish_g_m2 = sum(Biomass, na.rm = TRUE)/1200)

#            fish_kg_m2_total = sum(fish_kg_m2, na.rm = TRUE)) # 300 m2 per transect x 4 transects per site 

fish_summary %>%
  #drop_na(trophic_new)%>%
  ggplot(aes(x = Year, y = fish_g_m2))+
  geom_point()+
  geom_smooth(method = "lm")+
  # add_fishape(#family = "Labridae",
  #   option = "Chaetodon_plebeius",
  #   xmin = 2010, xmax = 2014, ymin = 0.15, ymax = 0.16,
  #   #fill = fish(option = "Chaetodon_plebeius"),
  #   alpha = 0.8) +
  facet_wrap(~trophic_new, scales = "free")

write_csv(fish_clean, here("Data","fish_clean.csv"))

write_csv(fish_summary, here("Data","fish_summary.csv"))
