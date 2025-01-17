### Data cleaning and joining for the primary productivity analysis ###
### Bring in physical data, metabolism data, and cover data together ####
## Make some background figures #####

## By Nyssa Silbiger ###
## Updated 2025-01-16

## load libraries #####
library(tidyverse)
library(rphylopic)
library(patchwork)
library(here)
library(performance)
library(effectsize)
library(interactions)
library(lme4)
library(lmerTest)
library(brms)
library(tidybayes)
library(broom)
library(broom.mixed)
library(projpred)
library(ggsci)
library(posterior)
library(bayesplot)
library(rTPC)
library(nls.multstart)
library(boot)
library(car)

## Read in the different data sheets ####

## Primary production and calcification data 
PP<-read_csv(here("Data","PP_Data.csv"))%>%
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs = as.numeric)%>%
  mutate(NEC_NEP = daily_NEC/daily_NPP, ## calculate NEP/NEC ratio
         NEC_R = night_NEC/daily_R)

## Read in the deployment physics Data (light, temperature, and flow rate at time of collection) 
Physics_deploy <-read_csv(here("Data","Physics_deploy.csv"))

## Bring in the yearly SST data 
yearly_sst<-read_csv(here("Data","SST_year.csv"))

# Bob's transect percent cover data
BenthicCover_Algae<-read_csv(here("Data","Backreef_Algae.csv")) %>%
  filter(Habitat == "Backreef")

Benthic_summary_Algae<-BenthicCover_Algae %>%
  rename(name = Taxonomy_Substrate_Functional_Group)%>%
  mutate(name = ifelse(name %in%c("Amansia rhodantha",         
                                  "Turbinaria ornata" ,        
                                  "Dictyota sp.",              
                                  "Halimeda sp.",              
                                  "Galaxaura sp.",             
                                  "Liagora ceranoides",        
                                  "Cyanophyta",                
                                  "Halimeda minima",           
                                  "Amphiroa fragilissima",     
                                  "Caulerpa serrulata",        
                                  "Corallimorpharia",          
                                  "Dictyota friabilis",        
                                  "Galaxaura rugosa",          
                                  "Cladophoropsis membranacea",
                                  "Galaxaura filamentosa",     
                                  "Halimeda discoidea",        
                                  "Peyssonnelia inamoena",     
                                  "Caulerpa racemosa",         
                                  "Valonia ventricosa",        
                                  "Actinotrichia fragilis",    
                                  "Dictyota bartayresiana",    
                                  "Microdictyon umbilicatum",  
                                  "Halimeda distorta",         
                                  "Halimeda incrassata",       
                                  "Halimeda macroloba",        
                                  "Dictyota implexa",          
                                  "Gelidiella acerosa",        
                                  "Dictyosphaeria cavernosa",  
                                  "Valonia aegagropila",       
                                  "Microdictyon okamurae",     
                                  "Halimeda opuntia",          
                                  "Dichotomaria obtusata",     
                                  "Chlorodesmis fastigiata",   
                                  "Phyllodictyon anastomosans",
                                  "Phormidium sp.",            
                                  "Cladophoropsis luxurians",  
                                  "Sargassum pacificum",       
                                  "Chnoospora implexa",        
                                  "Halimeda taenicola",        
                                  "Boodlea kaeneana",          
                                  "Padina boryana",            
                                  "Coelothrix irregularis",    
                                  "Gelidiella sp.",            
                                  "Hydroclathrus clathratus",  
                                  "Dictyota divaricata",       
                                  "Hypnea spinella",           
                                  "Dichotomaria marginata",    
                                  "Sporolithon sp.",           
                                  "Chaetomorpha antennina",    
                                  "Asparagopsis taxiformis"
  ), "Fleshy Macroalgae",name))%>%
  group_by(Year, Site, name)%>%
  summarise(mean_cover = mean(Percent_Cover, na.rm = TRUE))

LTER1_cover<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"),
         Site == "LTER 1")%>%
  ggplot(aes(x = Year, y = mean_cover, color = name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen"))+
  geom_text(data = tibble(Year = c(2007, 2017, 2014.5), 
                          name = c("Coral","Crustose Corallines","Fleshy Macroalgae"), 
                          mean_cover = c(33,20,0)),
            aes(x = Year, y = mean_cover, label = name))+
  labs(x = "",
       y = "Cover (%)",
       color = "",
       title = "LTER 1 only")+
  scale_y_continuous(limits = c(0,40))+
  # scale_x_continuous(limits = c(2008, 2025))+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        #     legend.position = c(0.71, 0.9),
        legend.position = "none",
        #    legend.background = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

All_cover<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, name)%>%
  summarise(mean_all = mean(mean_cover, na.rm = TRUE),
            se_all = sd(mean_cover, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Year, y = mean_all, color = name))+
  geom_ribbon(aes(ymin = mean_all-se_all, ymax = mean_all+se_all, fill = name), alpha = 0.3, linetype = 0)+
  geom_point(size = 2)+
  #geom_errorbar(aes(ymin = mean_all-se_all, ymax = mean_all+se_all), width = 0.1)+
  geom_line(size = 1, show.legend = FALSE)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen"))+
  scale_fill_manual(values = c("#CC7161","lightpink","darkgreen"))+
  labs(x = "",
       y = "",
       color = "",
       fill = "",
       title = "Mean across all 6 LTER sites")+
  scale_y_continuous(limits = c(0,40))+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank())

LTER1_cover|All_cover+ theme(legend.position = "none")+ plot_layout(guides = "collect")

ggsave(here("Output","BenthicCoverAll.pdf"), width = 8, height = 4)


### Make a plot of all living cover (basically everything - sand + coral RUbble and algal turf)
LTER1_coverliving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"),
         Site == "LTER 1")%>%
  group_by(Year)%>%
  summarise(mean_alive = sum(mean_cover)) %>%
  ggplot(aes(x = Year, y = mean_alive))+
  geom_line(size = 1, alpha = 0.2, lty = 2)+
  geom_point(size = 2)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(limits = c(0,40))+
  labs(x = "",
       y = "Cover of living macro-producers (%)",
       color = "",
       # title = "LTER 1 only"
  )+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank())

# All sites
All_coverliving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Site, Year)%>%
  summarise(mean_alive = sum(mean_cover)) %>%
  ungroup()%>%
  group_by(Year)%>%
  summarise(mean_all = mean(mean_alive, na.rm = TRUE),
            se_all = sd(mean_alive, na.rm = TRUE)/sqrt(n()))%>%
  ungroup()%>%
  ggplot(aes(x = Year, y = mean_all))+
  geom_line(size = 1)+
  #  geom_line(size = 1, alpha = 0.2, lty = 2)+
  geom_point(size = 2)+
  #  geom_smooth(method = "lm", color = "black")+
  
  geom_ribbon(aes(x = Year, ymin = mean_all - se_all, ymax = mean_all + se_all),
              fill = "grey", alpha = 0.3)+
  scale_y_continuous(limits = c(0,40))+
  labs(x = "",
       y = "",
       #y = "Percent Cover of living macro-producers (%)",
       color = "",
       # title = "Mean across all 6 LTER sites"
  )+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank())

(LTER1_cover|All_cover+ theme(legend.position = "none")+ plot_layout(guides = "collect"))/(LTER1_coverliving+All_coverliving)
ggsave(here("Output","AllCoverData.pdf"), width = 8, height = 8)

## Calculate the total percent of calcifiers
Total_Calc<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines"))%>%
  group_by(Year, Site)%>%
  reframe(total_Calc = mean_cover[name == "Coral"]+
            mean_cover[name == "Crustose Corallines"])

TotalLiving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, Site)%>%
  summarise(mean_alive = sum(mean_cover))

# plot relationship between sst and percent cover of macroproducers
TotalLiving %>%
  filter(Site == "LTER 1")%>%
  left_join(yearly_sst %>% 
              select(-Year)%>%
              rename(Year = Year_Benthic)%>%
              mutate(Year = Year+1))%>%
  ggplot(aes(y = mean_alive, x = mean_SST))+
  geom_point()+
  geom_smooth(method = "lm")

# Bring together PP and deployment physics data
PP <- PP %>%
  left_join(Physics_deploy)%>%
  left_join(TotalLiving %>%
              filter(Site %in% c("LTER 1","LTER 2"))) %>% # bring in total live data
  left_join(Total_Calc %>%
              filter(Site %in% c("LTER 1","LTER 2"))) # bring in total calc data

# Make a long version of the PP dataset
PP_long<-PP %>% 
  filter(Site == "LTER 1") %>%
  select(Month, Year, NEC = daily_NEC, SE = daily_NEC_SE, NEP = daily_GPP, SE_NEP = GPP_SE)%>%
  mutate(Day_Night = "Day") %>%
  bind_rows(PP %>% 
              filter(Site == "LTER 1") %>%
              select(Month, Year, NEC = night_NEC, SE = night_NEC_SE,NEP = daily_R, SE_NEP = R_SE)%>%
              mutate(Day_Night = "Night")) %>%
  left_join(Total_Calc%>%
              filter(Site =="LTER 1"))  %>%# b
  left_join(TotalLiving%>%
              filter(Site =="LTER 1"))  # b

#### All the LTER1 data #####
LTER1<-PP %>%
  filter(Site == "LTER 1") %>%
  drop_na(Month) %>%
  droplevels() %>%
  left_join(Physics_deploy) %>%
  mutate(Season = ifelse(Month == "January", "Summer", "Winter"))

## plot of yearly SST over time
yearly_sst %>%
  mutate(Year = as.integer(Year))%>%
  ggplot(aes(x = Year, y = mean_SST, color = mean_SST))+
  #  geom_line(data = moddata, aes(x = Year, y = .fitted), color = "black")+
  geom_smooth(method = "lm", color = "black")+
  geom_point(size = 3)+
  labs( 
    x = 'Year', 
    y = 'Mean Temperature ('~degree~"C)") +
  scale_color_gradient(low = "#0091ff", high = "#f0650e", guide = NULL)+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank()
  )

ggsave(here("Output","TempYear.pdf"), width = 5, height = 6)

## remove what I dont need ###
rm(Total_Calc, TotalLiving, BenthicCover_Algae, Benthic_summary_Algae)
