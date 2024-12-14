#### Working group for primary production ####
### Bayesian analysis on GP, R, NC, and enviro drivers ###
### Nyssa Silbiger ####
### 9/8/2023 #####

### updated 11/25/24


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

# Read in Benthic data 
# Benthic Cover data
BenthicCover <- read_csv(here("Data","Backreef_Data.csv"))%>%
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
       y = "Percent Cover (%)",
       color = "",
       title = "LTER 1 only")+
  scale_y_continuous(limits = c(0,40))+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(0.71, 0.9),
        legend.background = element_blank(),
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
  geom_line(size = 1)+
  scale_y_continuous(limits = c(0,40))+
  labs(x = "",
       y = "Percent Cover of living macro-producers (%)",
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

### Look at the ratio of coral to algae vs NEP and NEC
Benthic_summary<-BenthicCover %>%
  group_by(Year, Site) %>%
  summarise_at(vars(coral:ctb), .funs = function(x){mean(x,na.rm = TRUE)}) %>% # calculate the mean by transect
  mutate(logratio = log(coral/macroalgae)) # calculate log ratio

#### PP versus percent cover #####
LTER1<-Benthic_summary %>%
  full_join(PP)  %>%
  filter(Site == "LTER 1") %>%
  drop_na(Month) %>%
  droplevels() %>%
  left_join(Physics_deploy)

###### Bayesian Analysis for metabolism #######
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

# extract the predictions with RE included
data_gp<-conditional_effects(GPPMod, "Year",re_formula = NULL)$Year

# Plot the predicted data
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

# extract the posterior draws
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
   geom_vline(xintercept = 0, lty = 2, alpha = 0.7, linewidth = 1.3)+
  geom_area(data = filter(plotdata_GPP, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_GPP, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.015, label = "P(1) \n GP is declining over time")+
  annotate("text", x = slop_GPP, y = 0.03, label = paste(slop_GPP,"\u00B1",SE_GPP))+
  labs(x = "Change in GP per year",
       y = "Density")+
  theme_bw()

GPPplot<-GPP_pred+GPP_dens&theme(axis.text = element_text(size = 16),
                              axis.title = element_text(size = 18),
                              panel.grid.minor = element_blank())

# Respiration model
RMod<-brm(-daily_R~Year + (1|Month), data = LTER1, 
          control = list(adapt_delta = 0.92), iter = 3000)
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
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7, linewidth = 1.3)+
  # geom_vline(xintercept = slop_NPP, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_R, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_R, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.025, label = "P(0.998) \n R is declining over time")+
  annotate("text", x = slop_R, y = 0.04, label = paste(slop_R,"\u00B1",SE_R))+
  labs(x = "Change in R per year",
       y = "Density")+
  theme_bw()

Respplot<-R_pred+R_dens&theme(axis.text = element_text(size = 16),
                        axis.title = element_text(size = 18),
                        panel.grid.minor = element_blank())

# Net prodiction model ####
NPMod<-brm(daily_NPP~Year + (1|Month), data = LTER1, 
           control = list(adapt_delta = 0.92), iter = 3000)
#plot(NPMod)
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
  geom_area(data = filter(plotdata, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.035, label = "P(0.934) \n NEP is declining over time")+
  annotate("text", x = slop_NPP, y = 0.06, label = paste(slop_NPP,"\u00B1",SE_NPP))+
  labs(x = "Change in NEP per year",
       y = "Density")+
  theme_bw()

NPP_plot<-NPP_pred+NPP_dens&theme(axis.text = element_text(size = 16),
                        axis.title = element_text(size = 18))

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
  geom_hline(yintercept = 0, lty = 2)+
  geom_line(data = data_NEC %>% filter(effect2__ == "January"), aes(x = effect1__, y = estimate__, color = effect2__), size = 1.2, show.legend = FALSE)+
  geom_ribbon(data = data_NEC%>% filter(effect2__ == "January"), aes(x = effect1__, ymin = lower__, ymax = upper__, fill = effect2__),
               alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = daily_NEC, color = Month), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  scale_fill_manual(values = c("#ffbe4f","#0ea7b5"), guide = FALSE)+
  labs(color = "",
       fill = "",
       x = "Year",
       y =bquote(atop("Net Ecosystem Calcification",
                      "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
  annotate("text", x = 2020, y = 500, label = paste("NEC = 0 in Year",NEC_0_Year))+
  theme_bw()+
  theme(legend.position = c(.15,.8))

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
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7,
             linewidth = 1.3)+
  geom_area(data = filter(plotdata_NEC, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_NEC, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.015, label = "P(0.99) \n NEC is declining over time")+
  annotate("text", x = slop_NEC, y = 0.06, label = paste(slop_NEC,"\u00B1",SE_NEC))+
  labs(x = "Change in NEC per year",
       y = "Density")+
  theme_bw()


## If we just looked at total with no interaction how would the slope change?
NECMod_total<-brm(daily_NEC~Year, data =LTER1 , 
                  control = list(adapt_delta = 0.92, max_treedepth = 20), iter = 3000)
summary(NECMod_total)
NEC_coeff_total<-summary(NECMod_total)$fixed # pullout the fixed effects

slop_NEC_total<-round(NEC_coeff_total$Estimate[2],2) #slope
SE_NEC_total<-round(NEC_coeff_total$Est.Error[2],2) #error

draws_NEC_total<-NECMod_total %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# calculate proportion < 0
draws_NEC_total %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 1 that NEP is declining over time
plotdata_NEC_total<-as_tibble(density(draws_NEC_total$b_Year)) %>%
  mutate(variable = ifelse(x<0, "On","Off"))

# Figure showing prosterior for R being negative
NEC_dens_total<-ggplot(plotdata_NEC_total, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_NEC_total, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_NEC_total, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  annotate("text", x = 27.5, y = 0.015, label = "P(0.90) \n NEC is declining over time")+
  annotate("text", x = slop_NEC_total, y = 0.06, label = paste(slop_NEC_total,"\u00B1",SE_NEC))+
  labs(x = "Change in NEC per year",
       y = "Density")+
  theme_bw()



NECplot<-NEC_pred+NEC_dens&theme(axis.text = element_text(size = 16),
                                 axis.title = element_text(size = 18),
                                 panel.grid.minor = element_blank())

GPPplot/Respplot/NECplot
ggsave(filename = "Output/BayesRegression2.pdf", width = 14, height = 14)

#### Bayesian analysis for environmental data #######
LTER1 <-LTER1 %>% # add in scaled data
  ungroup()%>%
 # drop_na(Temp_mean)%>%
  mutate(Temp_scale = as.numeric(scale(Temp_mean, scale = TRUE, center = TRUE)),
         Light_scale = as.numeric(scale(TotalPAR_mean, scale = TRUE, center = TRUE)),
         Flow_scale = as.numeric(scale(Flow_mean, scale = TRUE, center = TRUE)),
         alive_scale = as.numeric(scale(mean_alive, scale = TRUE, center = TRUE)),
         calc_scale = as.numeric(scale(total_Calc, scale = TRUE, center = TRUE))
         
  )

# split the seasons
WinterData<-LTER1 %>%
  filter(Month == "June")

SummerData<-LTER1 %>%
  filter(Month == "January")

# total macroproducers -  only done one time of year
set.seed(23)
AliveMod<-brm(alive_scale~Year , data = WinterData, 
                    control = list(adapt_delta = 0.92), iter = 3000)

#extract the parameters
AliveMod_Coefs <- summary(AliveMod)$fixed %>%
  mutate(Month = "Winter",
         Parameter = "Macroproducers",
         coef = rownames(.)) %>%
  as_tibble()

# calcifiers
CalcMod<-brm(calc_scale~Year , data = WinterData, 
              control = list(adapt_delta = 0.92), iter = 3000)

#extract the parameters
CalcMod_Coefs <- summary(CalcMod)$fixed %>%
  mutate(Month = "Winter",
         Parameter = "Calcifiers",
         coef = rownames(.)) %>%
  as_tibble()


# Temperature

TempMod_winter<-brm(Temp_scale~Year , data = WinterData, 
            control = list(adapt_delta = 0.92), iter = 3000)

#extract the parameters
Temp_winter_Coefs <- summary(TempMod_winter)$fixed %>%
  mutate(Month = "Winter",
         Parameter = "Temperature",
         coef = rownames(.)) %>%
  as_tibble()

TempMod_summer<-brm(Temp_scale~Year , data = SummerData, 
                    control = list(adapt_delta = 0.92), iter = 3000)

#extract the parameters
Temp_summer_Coefs <- summary(TempMod_summer)$fixed %>%
  mutate(Month = "Summer",
         Parameter = "Temperature",
         coef = rownames(.)) %>%
  as_tibble()

## Light
LightMod_winter<-brm(Light_scale~Year , data = WinterData, 
                    control = list(adapt_delta = 0.92), iter = 3000)

#extract the parameters
Light_winter_Coefs <- summary(LightMod_winter)$fixed %>%
  mutate(Month = "Winter",
         Parameter = "Light",
         coef = rownames(.)) %>%
  as_tibble()

LightMod_summer<-brm(Light_scale~Year , data = SummerData, 
                     control = list(adapt_delta = 0.92), iter = 3000)

Light_summer_Coefs <- summary(LightMod_summer)$fixed %>%
  mutate(Month = "Summer",
         Parameter = "Light",
         coef = rownames(.)) %>%
  as_tibble()
# Flow
FlowMod_winter<-brm(Flow_scale~Year , data = WinterData, 
                     control = list(adapt_delta = 0.94), iter = 5000)

#extract the parameters
Flow_winter_Coefs <- summary(FlowMod_winter)$fixed %>%
  mutate(Month = "Winter",
         Parameter = "Flow",
         coef = rownames(.)) %>%
  as_tibble()

FlowMod_summer<-brm(Flow_scale~Year , data = SummerData, 
                     control = list(adapt_delta = 0.92), iter = 3000)

Flow_summer_Coefs <- summary(FlowMod_summer)$fixed %>%
  mutate(Month = "Summer",
         Parameter = "Flow",
         coef = rownames(.)) %>%
  as_tibble()
  

## Bring together the coeffes
EnviroCoefs<-bind_rows(Temp_summer_Coefs,
          Temp_winter_Coefs,
          Light_summer_Coefs,
          Light_winter_Coefs,
          Flow_summer_Coefs,
          Flow_winter_Coefs,
          AliveMod_Coefs,
          CalcMod_Coefs)


EnviroCoefs %>%
  filter(coef == "Year")%>%
  mutate(Parameter = factor(Parameter, levels = c("Flow","Temperature","Light","Calcifiers","Macroproducers")))%>%
  ggplot(aes(x = Estimate, y = Parameter, color = Month))+
  geom_vline(xintercept = 0)+
  geom_point(size = 4, position = position_dodge(0.15))+
  geom_errorbarh(aes(xmin = Estimate - Est.Error, xmax = Estimate + Est.Error, y = Parameter), 
                 height = 0, position = position_dodge(0.15))+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  labs(x = "Standardized Effect Size",
       y = "",
       color = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16))
  
ggsave(filename = "Output/EnviroCoefs.pdf", width = 8, height = 5)

#### show the predictions, but with the raw data##############

TempMod_winter<-brm(Temp_mean~Year , data = WinterData, 
                    control = list(adapt_delta = 0.92), iter = 3000)

summary(TempMod_winter) #0.04 +/- 0.03 per year
data_Temp<-conditional_effects(TempMod_winter, "Year",re_formula = NULL)$Year

Temp_pred<-ggplot()+
  geom_line(data = data_Temp, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_Temp, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1 %>% filter(Month == "June"), 
             aes(x = Year, y = Temp_mean), size = 2, color = "#0ea7b5")+
  labs(x = "Year",
       y =bquote(atop("Temperature",
                      "("~degree~"C)")))+
  theme_bw()+
  theme(legend.position = "none")

# winter light

LightMod_winter<-brm(TotalPAR_mean~Year , data = WinterData, 
                    control = list(adapt_delta = 0.92), iter = 3000)
summary(LightMod_winter) # -1.06 +/- 0.29 mols per year

data_Light<-conditional_effects(LightMod_winter, "Year",re_formula = NULL)$Year

Light_pred<-ggplot()+
  geom_line(data = data_Light, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_Light, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1 %>% filter(Month == "June"), 
             aes(x = Year, y = TotalPAR_mean), size = 2, color = "#0ea7b5")+
  labs(x = "Year",
       y = expression(paste("Total PAR \n(mol photons", " m"^-2, " d"^-1,")")))+
  theme_bw()+
  theme(legend.position = "none")

# winter flow

FlowMod_winter<-brm(Flow_mean~Year , data = WinterData, 
                     control = list(adapt_delta = 0.92), iter = 3000)

data_Flow<-conditional_effects(FlowMod_winter, "Year",re_formula = NULL)$Year

Flow_pred<-ggplot()+
  geom_line(data = data_Flow, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_Flow, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1 %>% filter(Month == "June"), 
             aes(x = Year, y = Flow_mean), size = 2, color = "#0ea7b5")+
  labs(x = "Year",
       y = expression(paste("Flow Speed (","m"^2," s"^-1,")")))+
  theme_bw()+
  theme(legend.position = "none")

# macroproducers
AliveMod<-brm(mean_alive~Year , data = WinterData, 
              control = list(adapt_delta = 0.92), iter = 3000)

data_macro<-conditional_effects(AliveMod, "Year",re_formula = NULL)$Year

alive_pred<-ggplot()+
  geom_line(data = data_macro, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_macro, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1 %>% filter(Month == "June"), 
             aes(x = Year, y = mean_alive), size = 2, color = "#0ea7b5")+
  labs(x = "Year",
       y = "Macroproducer cover (%)")+
  theme_bw()+
  theme(legend.position = "none")

# Calcifiers
# macroproducers
CalcMod<-brm(total_Calc~Year , data = WinterData, 
              control = list(adapt_delta = 0.92), iter = 3000)

data_calc<-conditional_effects(CalcMod, "Year",re_formula = NULL)$Year

calc_pred<-ggplot()+
  geom_line(data = data_calc, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_calc, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1 %>% filter(Month == "June"), 
             aes(x = Year, y = total_Calc), size = 2, color = "#0ea7b5")+
  labs(x = "Year",
       y = "Calcifier cover (%)")+
  theme_bw()+
  theme(legend.position = "none")


Temp_pred/Light_pred/Flow_pred/alive_pred/calc_pred
ggsave(here("Output","WinterEnviro_pred.pdf"), width = 5, height = 12)

##### Look for the drivers of GPP#############
## macro producers + Temp + Light + Flow (scaled) + (1|month)

## All scaled in the models
# GPP
GPP_enviro_mod<-brm(daily_GPP~alive_scale+Temp_scale+Light_scale+Flow_scale +(1|Month) , data = LTER1, 
                        control = list(adapt_delta = 0.92), iter = 5000)

GPP_enviro_mod2<-brm(daily_GPP~alive_scale+Light_scale+Flow_scale +(1|Month) , data = LTER1, 
                    control = list(adapt_delta = 0.95), iter = 6000)

#plot(conditional_effects(GPP_enviro_mod, re_formula =NULL), points = TRUE)
# visualize the pairs to inspect for multicollinearity
GP_pairs<-pairs(GPP_enviro_mod)
ggsave(here("Output","GP_pairs.png"), plot = GP_pairs)

GPP_enviro_coef<-summary(GPP_enviro_mod)$fixed[2:5,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()

GPP_enviro_coef2<-summary(GPP_enviro_mod2)$fixed[2:4,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()


GPP_coef_plot<-GPP_enviro_mod %>%
  spread_draws(b_Light_scale, b_alive_scale, b_Temp_scale, b_Flow_scale) %>%
  rename(Light = b_Light_scale, Cover =b_alive_scale,
         Temperature = b_Temp_scale, Flow = b_Flow_scale)%>%
  pivot_longer(Light:Flow)%>%
  ggplot(aes(y = name, x = value,fill = after_stat(x < 0))) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Gross Photosynthesis",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))
  
GPP_coef_plot2<-GPP_enviro_mod2 %>%
  spread_draws(b_Light_scale, b_alive_scale, b_Flow_scale) %>%
  rename(Light = b_Light_scale, Cover =b_alive_scale,
         Flow = b_Flow_scale)%>%
  pivot_longer(Light:Flow)%>%
  ggplot(aes(y = name, x = value,fill = after_stat(x < 0))) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Gross Photosynthesis",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))

# Respiration
R_enviro_mod<-brm(-daily_R~alive_scale+Temp_scale+Flow_scale +(1|Month) , data = LTER1, 
                    control = list(adapt_delta = 0.92), iter = 5000)

#plot(conditional_effects(R_enviro_mod, re_formula =NULL), points = TRUE)
R_pairs<-pairs(R_enviro_mod)
ggsave(here("Output","R_pairs.png"), plot = R_pairs)

R_enviro_coef<-summary(R_enviro_mod)$fixed[2:5,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()

R_coef_plot<-R_enviro_mod %>%
  spread_draws( b_alive_scale, b_Temp_scale, b_Flow_scale) %>%
  rename(Cover =b_alive_scale,
         Temperature = b_Temp_scale, Flow = b_Flow_scale)%>%
  pivot_longer(Cover:Flow)%>%
  ggplot(aes(y = name, x = value,fill = after_stat(x < 0))) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Respiration",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))

# Calcification

C_enviro_mod<-brm(bf(daily_NEC~Month*(calc_scale+Temp_scale+Light_scale+Flow_scale),
                     decomp = "QR") , data = LTER1, 
                    control = list(adapt_delta = 0.96), iter = 5000)

#plot(conditional_effects(C_enviro_mod, re_formula =NULL), points = TRUE)
NEC_pairs<-pairs(C_enviro_mod)
ggsave(here("Output","C_pairs.png"), plot = NEC_pairs)

get_variables(C_enviro_mod)

C_enviro_coef_all<-summary(C_enviro_mod)$fixed[3:10,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()

# all the coefs with the interactions 
C_coef_plot_all<-C_enviro_mod %>%
  spread_draws(b_Light_scale, b_calc_scale, b_Temp_scale, b_Flow_scale,
               `b_MonthJune:Light_scale`, `b_MonthJune:calc_scale`, `b_MonthJune:Temp_scale`, `b_MonthJune:Flow_scale`) %>%
  rename(Light = b_Light_scale, Cover =b_calc_scale,
         Temperature = b_Temp_scale, Flow = b_Flow_scale,
         Light_June =`b_MonthJune:Light_scale`,
         Cover_June = `b_MonthJune:calc_scale`,
         Temp_June = `b_MonthJune:Temp_scale`,
         Flow_June = `b_MonthJune:Flow_scale`)%>%
  pivot_longer(Light:Flow_June)%>%
  ggplot(aes(y = name, x = value,fill = after_stat(x < 0))) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Net ecosystem calcification (daytime)",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))

C_enviro_coef<-summary(C_enviro_mod)$fixed[3:6,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()

C_coef_plot<-C_enviro_mod %>%
  spread_draws(b_Light_scale, b_calc_scale, b_Temp_scale, b_Flow_scale
                ) %>%
  rename(Light = b_Light_scale, Cover =b_calc_scale,
         Temperature = b_Temp_scale, Flow = b_Flow_scale)%>%
  pivot_longer(Light:Flow)%>%
  ggplot(aes(y = name, x = value,fill = after_stat(x < 0))) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Net ecosystem calcification (daytime)",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))

GPP_coef_plot|R_coef_plot|C_coef_plot
ggsave(here("Output","CoefPlot_enviro_metab.pdf"), width = 14, height = 5)

# MAKE A PLOT COMPARING ESTIMATES OF CHANGE OVER TIME HERE TO OTHER DATASETS AROUND THE WORLD

BothSites<-Benthic_summary_Algae %>%
  pivot_wider(names_from = name, values_from = mean_cover)%>%
  mutate(mean_alive = sum(Coral, `Crustose Corallines`, `Fleshy Macroalgae`))%>%
  full_join(PP)  %>%
  #filter(Site == "LTER 1") %>%
  drop_na(Month) %>%
  droplevels() %>%
  left_join(Physics_deploy)

BothSites %>%
  ggplot(aes(x = mean_alive, y= daily_GPP))+
  geom_point(aes(color = Site))+
  geom_smooth(method = "lm")
  

testboth<-lmer(daily_GPP ~ mean_alive+TotalPAR_mean+Temp_mean+Flow_mean +(1|Site/Month), data = BothSites)
anova(testboth)
summary(testboth)

testboth_R<-lmer(-daily_R ~ mean_alive+Temp_mean+Flow_mean +(1|Site/Month), data = BothSites)
anova(testboth_R)
summary(testboth_R)

testboth_NEC<-lmer(daily_NEC ~ mean_alive+Temp_mean+Flow_mean +(1|Site/Month), data = BothSites)
anova(testboth_NEC)
summary(testboth_NEC)

LTER1 %>%
  ggplot(aes(x = Temp_mean, y= daily_NPP))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")


### Look at total fish biomass data from backreef over time

