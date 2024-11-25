#### Working group for primary production ####
### Nyssa Silbiger ####
### 9/8/2023 #####

### updated 11/25/24


## load libraries #####
library(googlesheets4)
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
#  geom_point()+
  geom_line(size = 1)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen"))+
  labs(x = "",
       y = "Percent Cover",
       color = "",
       title = "LTER 1 only")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

All_cover<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, name)%>%
  summarise(mean_all = mean(mean_cover, na.rm = TRUE),
            se_all = sd(mean_cover, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Year, y = mean_all, color = name))+
  geom_ribbon(aes(ymin = mean_all-se_all, ymax = mean_all+se_all, fill = name), alpha = 0.5, linetype = 0)+
#  geom_point()+
  #geom_errorbar(aes(ymin = mean_all-se_all, ymax = mean_all+se_all), width = 0.1)+
  geom_line(size = 1, show.legend = FALSE)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen"))+
  scale_fill_manual(values = c("#CC7161","lightpink","darkgreen"))+
  labs(x = "",
       y = "",
       color = "",
       fill = "",
       title = "Mean across all 6 LTER sites")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
  
LTER1_cover+ theme(legend.position = "none")|All_cover+ plot_layout(guides = "collect")

ggsave(here("Output","BenthicCoverAll.pdf"), width = 8, height = 4)


### Make a plot of all living cover (basically everything - sand + coral RUbble and algal turf)
LTER1_coverliving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"),
         Site == "LTER 1")%>%
  group_by(Year)%>%
  summarise(mean_alive = sum(mean_cover)) %>%
  ggplot(aes(x = Year, y = mean_alive))+
  #  geom_point()+
  geom_line(size = 1)+
  labs(x = "",
       y = "Percent Cover of living coral and macroalgae",
       color = "",
       title = "LTER 1 only")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


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

## Read in the deployment physics Data (light, temperature, and flow rate at time of collection) 

Physics_deploy <-read_csv(here("Data","Physics_deploy.csv"))

# Bring together PP and deployment physics data
PP <- PP %>%
  left_join(Physics_deploy)%>%
  left_join(TotalLiving %>%
              filter(Site %in% c("LTER 1","LTER 2"))) %>% # bring in total live data
  left_join(Total_Calc %>%
              filter(Site %in% c("LTER 1","LTER 2"))) # bring in total calc data

### look at relationship between NEP and NEC over time ###

PP_long<-PP %>% 
  filter(Site == "LTER 1") %>%
  #drop_na(daily_NEC)%>%
  select(Month, Year, NEC = daily_NEC, SE = daily_NEC_SE, NEP = daily_GPP, SE_NEP = GPP_SE)%>%
  mutate(Day_Night = "Day") %>%
  bind_rows(PP %>% 
              filter(Site == "LTER 1") %>%
            #  drop_na(daily_NEC)%>%
              select(Month, Year, NEC = night_NEC, SE = night_NEC_SE,NEP = daily_R, SE_NEP = R_SE)%>%
              mutate(Day_Night = "Night")) %>%
  left_join(Total_Calc%>%
              filter(Site =="LTER 1"))  %>%# b
  left_join(TotalLiving%>%
              filter(Site =="LTER 1"))  # b

NEC_plot<-PP_long %>%  
ggplot(aes(x = Year, y = NEC, color = Day_Night))+
  geom_point()+
  geom_errorbar(aes(ymin = NEC-SE, ymax = NEC+SE), width = 0.1)+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

NEP_alive_GP<-PP_long %>%  
  filter(Day_Night == "Day")%>%
  ggplot(aes(x = mean_alive, y = NEP))+
  geom_point(
    #aes(color = Month)
  )+#  geom_errorbar(aes(ymin = NEP-SE_NEP, ymax = NEP+SE_NEP), width = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover of Macro Producers",
       y = "Gross Ecosystem Production")+
 # facet_wrap(~Day_Night, scales = "free",ncol = 1)+
  theme_bw()&
  theme(axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 14))
 
NEP_alive_R<-PP_long %>%  
  filter(Day_Night == "Night")%>%
  ggplot(aes(x = mean_alive, y = -NEP))+
  geom_point(
    #aes(color = Month)
    )+
  #  geom_errorbar(aes(ymin = NEP-SE_NEP, ymax = NEP+SE_NEP), width = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover of Macro Producers",
       y = "Respiration")+
  # facet_wrap(~Day_Night, scales = "free",ncol = 1)+
  theme_bw()&
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

NEP_alive_GP/NEP_alive_R+plot_layout(guides = "collect")

ggsave(filename = "Output/CoverNEP.png", width = 6, height = 8)

alive_mod_GP<-lmer(NEP~mean_alive + (1|Month), data = PP_long %>%  
                     filter(Day_Night == "Day"))
anova(alive_mod_GP)

alive_mod_R<-lmer(NEP~mean_alive + (1|Month), data = PP_long %>%  
                     filter(Day_Night == "Night"))
anova(alive_mod_R)

### strong negative decline between temperature and cover, but only in june
PP %>%
  filter(Month == "June")%>%
ggplot( aes(x = Temp_mean, y = mean_alive))+
  geom_point() +
  geom_smooth(method = "lm", color = "black")+
  labs(x = "Mean Winter Temperature",
       y = "% Cover of Macro Producers")+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)
  )
ggsave(filename = "Output/TempAlive.png", width = 6, height = 6)


PP %>%
  mutate(Month = ifelse(Month == "June","Winter","Summer"))%>%
  #filter(Month == "June")%>%
  ggplot( aes(y = Temp_mean, x = Year))+
  geom_point() +
  geom_smooth(method = "lm", color = "black", data = PP %>%
                mutate(Month = ifelse(Month == "June","Winter","Summer"))%>%
                filter(Month == "Winter"))+
  labs(y = "Mean Monthly Temperature",
       x = "Year")+
  theme_bw()+
  facet_wrap(~Month, ncol = 1, scales = "free_y")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)
        )

ggsave(filename = "Output/TempYear.png", width = 6, height = 8)

winter_temp_mod<-lm(Temp_mean~Year , data = PP %>%  
                     filter(Month == "June"))
anova(winter_temp_mod)
summary(winter_temp_mod)
 #0.048 +/-0.017 degrees increase per year (~1C increase every 20 years)


## NEC and total calc
PP%>%
  ggplot(aes(x = total_Calc, y = daily_NEC))+
  geom_point(
    #aes(color = Month)
  )+
  #  geom_errorbar(aes(ymin = NEP-SE_NEP, ymax = NEP+SE_NEP), width = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover of Calcifiers",
       y = "NEC")+
   facet_wrap(~Month, scales = "free",ncol = 1)+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

NEP_plot<-PP_long %>%  
  ggplot(aes(x = mean_alive, y = NEP, color = Day_Night))+
  geom_point()+
  geom_errorbar(aes(ymin = NEP-SE_NEP, ymax = NEP+SE_NEP), width = 0.1)+
  geom_smooth(method = "lm")
  facet_wrap(~Month)

NEC_plot/NEP_plot

PP %>% 
  filter(Site == "LTER 1") %>%
 # drop_na(daily_NEC)%>%
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

#### plots with Bob's benthic data
PP_long %>%
  ggplot(aes(x = total_Calc, y = NEC, color = Day_Night))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)


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
  

# make a figure of LTER 2 and 6 for 2022
Benthic_summary %>%
  filter(Site %in% c("LTER 2","LTER 6"),
         Year == 2022) %>%
  mutate(Site = ifelse(Site == "LTER 2","North \nShore", "West \nShore"))%>%
  select(!logratio)%>%
  pivot_longer(coral:ctb) %>%
  filter(name != "ctb") %>%
  mutate(name = ifelse(name == "coral","Coral","Macroalgae"))%>%
  mutate(Site = factor(Site, levels = c("West \nShore","North \nShore")))%>%
  ggplot(aes(x = Site, y = value, fill = name))+
  geom_col()+
  labs(x = "",
       y = "% Cover",
       fill = "")+
  scale_fill_manual(values = c("coral","darkgreen"))+
  theme_minimal()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "top")

ggsave(here("Output","PercentCover_NSF.png"),
       width = 3.5, height = 5.3)

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

GPP_R<-LTER1 %>% 
  select(Year, Month, daily_GPP, daily_R)%>%
  pivot_longer(cols = c(daily_GPP, daily_R)) %>%
  ggplot(aes(x = Year, y = value, color = name))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_point(aes(shape = Month))+
  geom_smooth(method = "lm")+
  annotate("text", x = 2010.5, y = 2400, label = "Gross Photosynthesis")+
  annotate("text", x = 2010.5, y = -400, label = "Respiration")+
  scale_color_manual(values = c("#E79685","#815661"))+
  labs(y = expression(paste("Community production (mmol O"[2], " m"^-2, " d"^-1,")")),
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank())


NPP<-LTER1 %>% 
  select(Year, Month, daily_NPP, daily_R)%>%
  ggplot(aes(x = Year, y = daily_NPP))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_point(aes(shape = Month), color = "grey5")+
  geom_smooth(method = "lm", color = "black")+
  labs(y = expression(paste("Net ecosystem production (mmol O"[2], " m"^-2, " d"^-1,")")),)+
  theme_bw()

GPP_R/NPP+plot_layout(guides = "collect")

LTER1 %>% 
  select(Year, Month, daily_NEC, night_NEC)%>%
  pivot_longer(cols = c(daily_NEC, night_NEC)) %>%
  drop_na()%>%
  ggplot(aes(x = Year, y = value, color = name))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_smooth(method = "lm")+
  geom_point(aes(shape = Month))+
  scale_color_manual(values = c("#E79685","#815661"))+
  labs(y = expression(paste("Net ecosystem calcification (mmol CaCO"[3], " m"^-2, " d"^-1,")")),)+
  guides(color = "none")+
  theme_bw()

# GPPMod<-lmer(daily_GPP~Year + (1|Month), data = LTER1)
# anova(GPPMod)
# summary(GPPMod)


### CALCULATE THE X INTERCEPT FOR ALL THE BELOW MODELS TO PREDICT WHAT YEAR IT WILL REACH 0.
set.seed(23)
GPPMod<-brm(daily_GPP~Year + (1|Month), data = LTER1, 
            control = list(adapt_delta = 0.92), iter = 3000)
summary(GPPMod)
GPP_coeff<-summary(GPPMod)$fixed # pullout the fixed effects

slop_GPP<-round(GPP_coeff$Estimate[2],2) #slope
SE_GPP<-round(GPP_coeff$Est.Error[2],2) #error

GPP_0_Year <- round(-GPP_coeff$Estimate[1]/GPP_coeff$Estimate[2])

# GP model
p_gp<-plot(conditional_effects(GPPMod, "Year",re_formula = NULL), ask = FALSE, points = TRUE)$Year

data_gp<-conditional_effects(GPPMod, "Year",re_formula = NULL)$Year

GPP_pred<-ggplot()+
  geom_line(data = data_gp, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_gp, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = daily_GPP, color = Month), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  labs(x = "Year",
       y =bquote(atop("Gross Photosynthesis",
                      "(mmol" ~ O[2]~m^-2~d^-1~")")))+
  annotate("text", x = 2020, y = 2000, label = paste("GP = 0 in Year",GPP_0_Year))+
  theme_bw()+
  theme(legend.position = "none")

# GPP_pred<-p_gp+
#   geom_smooth(color = "black")+
#   labs(y =bquote(atop("Gross Photosynthesis",
#                       "(mmol" ~ O[2]~m^-2~d^-1~")")))+
#  # labs(y = expression(paste("Gross Photoshynthesis (mmol O"[2], " m"^-2, " d"^-1,")")))+
#   annotate("text", x = 2020, y = 2000, label = paste("GP = 0 in Year",GPP_0_Year))+
#   theme_bw()

draws_GPP<-GPPMod %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# calculate proportion < 0
draws_GPP %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 1 that NEP is declining over time
plotdata_GPP<-as_tibble(density(draws_GPP$b_Year)) %>%
  mutate(variable = ifelse(x<0, "On","Off"))

# Figure showing prosterior for R being negative
GPP_dens<-ggplot(plotdata_GPP, aes(x, y)) + 
   geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_GPP, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_GPP, variable == 'On'), fill = 'light blue') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.015, label = "P(1) \n GP is declining over time")+
  annotate("text", x = slop_GPP, y = 0.03, label = paste(slop_GPP,"\u00B1",SE_GPP))+
  labs(x = "Change in GP per year",
       y = "Density")+
  theme_bw()

GPPplot<-GPP_pred+GPP_dens&theme(axis.text = element_text(size = 16),
                              axis.title = element_text(size = 18))

RMod<-brm(-daily_R~Year + (1|Month), data = LTER1, 
          control = list(adapt_delta = 0.92), iter = 3000)
#anova(RMod)
R_coeff<-summary(RMod)$fixed # pullout the fixed effects
slop_R<-round(R_coeff$Estimate[2],2) #slope
SE_R<-round(R_coeff$Est.Error[2],2) #error

R_0_Year <- round(-R_coeff$Estimate[1]/R_coeff$Estimate[2]) # year at 0

p_r<-plot(conditional_effects(RMod, "Year",re_formula = NULL), ask = FALSE, points = TRUE)$Year

data_r<-conditional_effects(RMod, "Year",re_formula = NULL)$Year

R_pred<-ggplot()+
   geom_line(data = data_r, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_r, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = -daily_R, color = Month), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  labs(x = "Year",
       y =bquote(atop("Respiration",
                      "(mmol" ~ O[2]~m^-2~d^-1~")")))+
  annotate("text", x = 2020, y = 1300, label = paste("R = 0 in Year",R_0_Year))+
  theme_bw()+
  theme(legend.position = "none")

# plot the prediction from the bayesian model
#R_pred<-p_r+
 # geom_smooth(color = "black")+
  #labs(y =bquote(atop("Respiration",
   #                   "(mmol" ~ O[2]~m^-2~d^-1~")")))+
 # labs(y = expression(paste("Respiration (mmol O"[2], " m"^-2, " d"^-1,")")))+
  #annotate("text", x = 2020, y = 1300, label = paste("R = 0 in Year",R_0_Year))+
  #theme_bw()

draws_R<-RMod %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# calculate proportion < 0

draws_R %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 0.998 that NEP is declining over time
plotdata_R<-as_tibble(density(draws_R$b_Year)) %>%
  mutate(variable = ifelse(x<0, "On","Off"))


# Figure showing prosterior for R being negative
R_dens<-ggplot(plotdata_R, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  # geom_vline(xintercept = slop_NPP, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_R, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_R, variable == 'On'), fill = 'light blue') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.025, label = "P(0.998) \n R is declining over time")+
  annotate("text", x = slop_R, y = 0.04, label = paste(slop_R,"\u00B1",SE_R))+
  labs(x = "Change in R per year",
       y = "Density")+
  theme_bw()

Respplot<-R_pred+R_dens&theme(axis.text = element_text(size = 16),
                        axis.title = element_text(size = 18))

# NPMod<-lmer(daily_NPP~Year + (1|Month), data = LTER1)
# anova(NPMod)
# summary(NPMod)

NPMod<-brm(daily_NPP~Year + (1|Month), data = LTER1, 
           control = list(adapt_delta = 0.92), iter = 3000)
plot(NPMod)
summary(NPMod)
fit<-predict(NPMod)
NPP_coeff<-summary(NPMod)$fixed # pullout the fixed effects

slop_NPP<-round(NPP_coeff$Estimate[2],2) #slope
SE_NPP<-round(NPP_coeff$Est.Error[2],2) #error

NPP_0_Year <- round(-NPP_coeff$Estimate[1]/NPP_coeff$Estimate[2])

p1<-plot(conditional_effects(NPMod, re_formula = NULL), ask = FALSE, points = TRUE)$Year

# plot the prediction from the bayesian model
NPP_pred<-p1+
  geom_smooth(color = "black")+
   labs(y =bquote(atop("Net ecosystem production",
                       "(mmol" ~ O[2]~m^-2~d^-1~")")))+
    annotate("text", x = 2020, y = 750, label = paste("NEP = 0 in Year",NPP_0_Year))+
  
   theme_bw()

get_variables(NPMod)

draws<-NPMod %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# calculate proprotion < 0

draws %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 0.934 that NEP is declining over time

plotdata<-as_tibble(density(draws$b_Year)) %>%
  mutate(variable = ifelse(x<0, "On","Off"))

# Figure showing prosterior for NPP being negative
NPP_dens<-ggplot(plotdata, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  # geom_vline(xintercept = slop_NPP, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata, variable == 'On'), fill = 'light blue') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.035, label = "P(0.934) \n NEP is declining over time")+
  annotate("text", x = slop_NPP, y = 0.06, label = paste(slop_NPP,"\u00B1",SE_NPP))+
  labs(x = "Change in NEP per year",
       y = "Density")+
  theme_bw()

NPP_plot<-NPP_pred+NPP_dens&theme(axis.text = element_text(size = 16),
                        axis.title = element_text(size = 18))


### tBeta### think about this as a probablility that it is negative

#### Calcification model
NECMod<-brm(daily_NEC~Year*Month, data =LTER1 , 
            control = list(adapt_delta = 0.92, max_treedepth = 20), iter = 3000)

summary(NECMod)
NEC_coeff<-summary(NECMod)$fixed # pullout the fixed effects

slop_NEC<-round(NEC_coeff$Estimate[2],2) #slope
SE_NEC<-round(NEC_coeff$Est.Error[2],2) #error

NEC_0_Year <- round(-NEC_coeff$Estimate[1]/NEC_coeff$Estimate[2])

# NEC model
data_NEC<-conditional_effects(NECMod, "Year:Month",re_formula = NULL)$Year

NEC_pred<-ggplot()+
  geom_line(data = data_NEC, aes(x = effect1__, y = estimate__, color = effect2__), size = 1.2)+
  geom_ribbon(data = data_NEC, aes(x = effect1__, ymin = lower__, ymax = upper__, fill = effect2__),
               alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = daily_NEC, color = Month), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  scale_fill_manual(values = c("#ffbe4f","#0ea7b5"))+
  labs(color = "",
       fill = "",
       x = "Year",
       y =bquote(atop("Net Ecosystem Calcification",
                      "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
  annotate("text", x = 2020, y = 500, label = paste("NEC = 0 in Year",NEC_0_Year))+
  theme_bw()+
  theme(legend.position = c(.15,.8))

# p_NEC<-plot(conditional_effects(NECMod, "Year:Month",re_formula = NULL), ask = FALSE, points = TRUE)$Year+
#   geom_hline(yintercept = 0, linetype = 2)+
#   theme_bw()

# NEC_pred<-p_NEC+
#   geom_smooth(color = "black")+
#   labs(color = "",
#        fill = "",
#     y =bquote(atop("Net ecosystem calcification (day)",
#                       "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
#   annotate("text", x = 2020, y = 500, label = paste("NEC = 0 in Year",NEC_0_Year))+
#   theme_bw()+
#   theme(legend.position = c(.8,.8))

draws_NEC<-NECMod %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# calculate proportion < 0
draws_NEC %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 1 that NEP is declining over time
plotdata_NEC<-as_tibble(density(draws_NEC$b_Year)) %>%
  mutate(variable = ifelse(x<0, "On","Off"))

# Figure showing prosterior for R being negative
NEC_dens<-ggplot(plotdata_NEC, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_NEC, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_NEC, variable == 'On'), fill = 'light blue') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.015, label = "P(0.99) \n NEC is declining over time")+
  annotate("text", x = slop_NEC, y = 0.06, label = paste(slop_NEC,"\u00B1",SE_NEC))+
  labs(x = "Change in NEC per year",
       y = "Density")+
  theme_bw()

NECplot<-NEC_pred+NEC_dens&theme(axis.text = element_text(size = 16),
                                 axis.title = element_text(size = 18))

GPPplot/Respplot/NECplot
ggsave(filename = "Output/BayesRegression2.png", width = 14, height = 14)


(NC/ND)&theme_bw()|(GP/R)&theme_bw()

ggplot(LTER1, aes(x = Year, y = PR,color = Month))+
  geom_point()+
  geom_smooth(method = "lm")
#Both GPP and R shrink regardless of month

# Average all by year
LTER1 %>%
  select(!Month)%>%
  group_by(Year)%>%
  summarise_if(.predicate = is.numeric,.funs = function(x){mean(x, na.rm = TRUE)})%>%
  ggplot(aes(x = Year, y = daily_GPP))+
  geom_point()
  


####
 # MAKE A PLOT COMPARING ESTIMATES OF CHANGE OVER TIME HERE TO OTHER DATASETS AROUND THE WORLD

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
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")

LTER1 %>%
  ggplot(aes(x = Flow_mean, y = daily_GPP))+
  geom_point()+
  geom_smooth(method = "lm")

LTER1 %>%
  ggplot(aes(x = Temp_mean, y = daily_GPP))+
  geom_point(aes(color = Month))+
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
