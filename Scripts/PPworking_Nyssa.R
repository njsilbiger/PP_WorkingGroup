#### Working group for primary production ####
### Nyssa Silbiger ####
### 9/8/2023 #####

### updated 5/31/24


## load libraries #####
library(googlesheets4)
library(tidyverse)
library(rphylopic)
library(patchwork)
library(here)
library(performance)
library(effectsize)
library(interactions)

## Read in the different data sheets ####
#Read google sheets data into R.

## Primary production and calcification data 
PP<-read_csv(here("Data","PP_Data.csv"))%>%
  # read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing") %>%
  mutate_at(vars(daily_NPP:UPDN_No, daily_NEC:night_NEC_SE), .funs = as.numeric)%>%
  mutate(NEC_NEP = daily_NEC/daily_NPP, ## calculate NEP/NEC ratio
         NEC_R = night_NEC/daily_R)

# Read in Benthic data 
# Benthic Cover data
BenthicCover <- read_csv(here("Data","Backreef_Data.csv"))%>%
  # read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')%>%
  mutate_at(vars(coral:sub_squares), .funs = as.numeric)

# Bob's transect data
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
  geom_point()+
  geom_line()+
  labs(x = "",
       y = "Percent Cover",
       color = "",
       title = "LTER 1 only")+
  theme_bw()

All_cover<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, name)%>%
  summarise(mean_all = mean(mean_cover, na.rm = TRUE),
            se_all = sd(mean_cover, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Year, y = mean_all, color = name))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_all-se_all, ymax = mean_all+se_all), width = 0.1)+
  geom_line()+
  labs(x = "",
       y = "Percent Cover",
       color = "",
       title = "Mean across all 6 LTER sites")+
  theme_bw()
  
LTER1_cover+ theme(legend.position = "none")|All_cover+ plot_layout(guides = "collect")

## Read in the deployment physics Data (light, temperature, and flow rate at time of collection) 

Physics_deploy <-read_csv(here("Data","Physics_deploy.csv"))

# Bring together PP and deployment physics data
PP <- PP %>%
  left_join(Physics_deploy)

### look at relationship between NEP and NEC over time ###

PP_long<-PP %>% 
  filter(Site == "LTER 1") %>%
  drop_na(daily_NEC)%>%
  select(Month, Year, NEC = daily_NEC, SE = daily_NEC_SE, NEP = daily_GPP, SE_NEP = GPP_SE)%>%
  mutate(Day_Night = "Day") %>%
  bind_rows(PP %>% 
              filter(Site == "LTER 1") %>%
              drop_na(daily_NEC)%>%
              select(Month, Year, NEC = night_NEC, SE = night_NEC_SE,NEP = daily_R, SE_NEP = R_SE)%>%
              mutate(Day_Night = "Night")) 

NEC_plot<-PP_long %>%  
ggplot(aes(x = Year, y = NEC, color = Day_Night))+
  geom_point()+
  geom_errorbar(aes(ymin = NEC-SE, ymax = NEC+SE), width = 0.1)+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

NEP_plot<-PP_long %>%  
  ggplot(aes(x = Year, y = NEP, color = Day_Night))+
  geom_point()+
  geom_errorbar(aes(ymin = NEP-SE_NEP, ymax = NEP+SE_NEP), width = 0.1)+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

NEC_plot/NEP_plot

PP %>% 
  filter(Site == "LTER 1") %>%
  drop_na(daily_NEC)%>%
  ggplot(aes(x = Year, y = daily_NEC-night_NEC))+
  geom_point()+
  labs(y = "Net Calcification")+
  geom_smooth(method = "lm")+
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
#  filter(Site == "LTER 1") %>%
  select(!logratio)%>%
  pivot_longer(coral:ctb) %>%
  filter(name != "ctb")%>%
  ggplot(aes(x = Year, y = value, color = name))+
  geom_point()+
  geom_line()+
  labs(y = "Percent Cover")+
  facet_wrap(~Site)
  

Benthic_summary %>%
full_join(PP)  %>%
  filter(Site == "LTER 1")%>%
  drop_na(Month)%>%
#  filter(Year != 2015) %>%
  ggplot(aes(x = logratio, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm", formula = "y~poly(x,2)")+
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

# GPP vs NEC
ggplot(LTER1, aes(x = daily_GPP, y = daily_NEC))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(LTER1, aes(x = coral, y = daily_GPP))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

NC<-ggplot(LTER1 %>% drop_na(daily_NEC), aes(x = Year, y = daily_NEC,color = Month))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

ND<-ggplot(LTER1 %>% drop_na(daily_NEC), aes(x = Year, y = night_NEC,color = Month))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

NECMod<-lm(daily_NEC~Year*Month, data = LTER1)
anova(NECMod)


# GPP changes over time regardless of month
GP<-ggplot(LTER1, aes(x = Year, y = daily_GPP))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

R<-ggplot(LTER1, aes(x = Year, y = daily_R))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

GPPMod<-lm(daily_GPP~Year*Month, data = LTER1)
anova(GPPMod)

RMod<-lm(daily_R~Year*Month, data = LTER1)
anova(RMod)

(NC/ND)&theme_bw()|(GP/R)&theme_bw()

#Both GPP and R shrink regardless of month


### GPP ~ coral cover, NEC ~ GPP, Show coral cover decline across all 6 sites to show consistency in trend
## NEC and dissolution are dependent on season
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


### Plot GPP~ Temp, Light, Flow etc
LTER1 %>%
  ggplot(aes(x = TotalPAR_mean, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm")

LTER1 %>%
  ggplot(aes(x = TotalPAR_mean, y = daily_NEC))+
  geom_point()+
  geom_smooth(method = "lm")

LTER1 %>%
  ggplot(aes(x = Flow_mean, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm")

LTER1 %>%
  ggplot(aes(x = Temp_mean, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm")

#### Model 

## Calculate residuals to remove effect of light on GPP and NEC
modLight<-lm(daily_GPP~TotalPAR_mean, data = LTER1, na.action = na.exclude)
anova(modLight)
GPP_light<-residuals(modLight)

modLight_NEC<-lm(daily_NEC~TotalPAR_mean, data = LTER1, na.action = na.exclude)
anova(modLight_NEC)
NEC_light<-residuals(modLight_NEC)

## Add the residuals to the dataframe
LTER1<-LTER1 %>%
  ungroup() %>%
  mutate(GPP_light = GPP_light,
         NEC_light = NEC_light)
# standardize the predictor variables
LTER<-LTER1 %>%
  ungroup()%>%
  mutate_at(vars(TotalPAR_mean, Temp_mean, Flow_mean, coral, daily_GPP, daily_NEC, daily_R, night_NEC,GPP_light,NEC_light, logratio), .funs = scale)%>%
  mutate_at(vars(TotalPAR_mean, Temp_mean, Flow_mean, coral, daily_GPP, daily_NEC, daily_R, night_NEC,GPP_light,NEC_light, logratio), .funs = as.numeric)

# raw model
#mod_GPP<-lm(daily_GPP~coral*(Flow_mean+Temp_mean+TotalPAR_mean), data = LTER1)
mod_GPP<-lm(daily_GPP~(coral*Temp_mean)+TotalPAR_mean, data = LTER1)

#mod_NEC<-lm(daily_NEC~coral*(Flow_mean+Temp_mean+TotalPAR_mean), data = LTER1)
mod_NEC<-lm(daily_NEC~(coral*Temp_mean)+TotalPAR_mean, data = LTER1)

mod_GPPNEC<-lm(NEC_NEP~coral*(Flow_mean+Temp_mean+TotalPAR_mean), data = LTER1)
mod_R<-lm(-daily_R~coral*Month, data = LTER1)
mod_NED<-lm(night_NEC~coral*Temp_mean, data = LTER1)

# check assumptions
check_model(mod_GPP)
check_model(mod_NEC)
check_model(mod_GPPNEC)
check_model(mod_R)

anova(mod_GPP)
summary(mod_GPP)

anova(mod_NEC)
summary(mod_NEC)

anova(mod_GPPNEC)
summary(mod_GPPNEC)

anova(mod_R)
summary(mod_R)

anova(mod_NED)
summary(mod_NED)

## residual model accounting for light
mod_GPP_R<-lm(GPP_light~coral*(Flow_mean+Temp_mean), data = LTER1)
mod_NEC_R<-lm(NEC_light~coral*(Flow_mean+Temp_mean), data = LTER1)

# check assumptions
check_model(mod_GPP_R)
check_model(mod_NEC_R)

#anova
anova(mod_GPP_R)
anova(mod_NEC_R)

#summary
summary(mod_GPP_R)
summary(mod_NEC_R)


p1_GPP<-interact_plot(mod_GPP, pred = coral, modx = Temp_mean, interval = TRUE,modx.values = c(27,29))

interact_plot(mod_R, pred = coral, modx = Month, interval = TRUE, plot.points = TRUE)

# plot residuals
p2_GPP<-LTER1 %>%
  ggplot(aes(y = daily_GPP, x = TotalPAR_mean ))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()
  
p2_GPP+p1_GPP&lims(y = c(0,3000))

LTER1 %>%
  filter(Year != 2015)%>%
  ggplot(aes(x = logratio, y = NEC_NEP))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)
