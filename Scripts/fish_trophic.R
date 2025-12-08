library(here)
library(tidyverse)
library(fishualize)

fish<-read_csv(here("Data","MCR_LTER_Annual_Fish_Survey_20250324.csv"))

fish<-fish %>%
  filter(Site == "LTER_1",
         Habitat == "Backreef")

fish_summary <- fish %>%
  mutate(trophic_new = case_when(Fine_Trophic %in% c("Brusher", "Browser","Excavator","Scraper")~"Benefits from dead coral",
                                 Fine_Trophic  == "Corallivore"~"Corallivore")
         )%>%
  mutate(fish_kg_m2 = (Biomass/50*Swath)/1000)%>%
  group_by(Year, trophic_new) %>%
  summarise(total_biomass = sum(Biomass, na.rm = TRUE),
            fish_kg_m2_total = sum(fish_kg_m2, na.rm = TRUE)) # 300 m2 per transect x 4 transects per site 

fish_summary %>%
  drop_na(trophic_new)%>%
  ggplot(aes(x = Year, y = fish_kg_m2_total))+
  geom_point()+
  geom_smooth(method = "lm")+
  add_fishape(#family = "Labridae",
              option = "Chaetodon_plebeius",
              xmin = 2010, xmax = 2014, ymin = 0.15, ymax = 0.16,
              #fill = fish(option = "Chaetodon_plebeius"),
              alpha = 0.8) +
  facet_wrap(~trophic_new, scales = "free")
