# Clean the primary productivity data and calculate GP and R by day and Season
# By Nyssa Silbiger
# Updated on 2/2/2025


##### load libraries #############
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(lme4)
library(lmerTest)
library(scales)
library(patchwork)
library(ggtext)
library(ggridges)
library(viridis)

### Read in the data ######

filedir<-here("Data","QC_PP") # file path for the QC PP files
files<-dir(path = filedir, pattern = ".csv", full.names = TRUE)

# bring in all the PP Data
All_PP_data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>%
  mutate(DateTime = mdy_hm(DateTime))

## Daily NEC 
NEC<-read_csv(here("Data","NEC_daily.csv"))

# From 2007-2014 the PP units are in g O2/m2/h. 
#From June 2014 on, the units are mmol O2/m2/h. So those early rates will need to be converted
# 32 g/mol of O2

All_PP_data<-All_PP_data %>%
  mutate(UP_Oxy = ifelse(DateTime< ymd_hms("2014-04-01 00:00:00"), (UP_Oxy/32)*1000, UP_Oxy),
         DN_Oxy = ifelse(DateTime< ymd_hms("2014-04-01 00:00:00"), (DN_Oxy/32)*1000, DN_Oxy),
         PP = ifelse(DateTime< ymd_hms("2014-04-01 00:00:00"), (PP/32)*1000, PP))%>%
  mutate(Date = as_date(DateTime),
         DielDateTime = DateTime+hours(12), # all sampling started at noon such that midnight was the middle of a "day". Use this to extract the "daily" R to calculating GP
         DielDate = as_date(DielDateTime),
         Year = year(DateTime)) %>%
  filter(!Date %in% mdy("5/27/2011","5/28/2011","1/21/2014","05/25/2024" ))   # the respiration rate is incorrect these days from instrument failure
 
  #filter(!DielDate %in% mdy("1/22/2020", "1/18/2017","5/28/2009","3/2/2022")) # these are extreme outliers

# find all the incomplete datasets (less than 24 hours) and only keep the complete ones
complete_dates<-All_PP_data %>% 
  group_by(DielDate)%>% 
  count() %>%
  filter(n == 24)

All_PP_data<-complete_dates %>%
  left_join(All_PP_data)

# calculate hourly GP and R data 
Daily_R <-All_PP_data %>%
  #filter(PAR==0)%>% # pull out all the night data
  group_by(Year, Season, DielDate) %>% # get the average nighttime respiration by day to add to NEP to calcualte GP
  summarise(R_average = mean(PP[PAR==0], na.rm = TRUE))


#Calculate GP
All_PP_data<-All_PP_data %>%
  left_join(Daily_R) %>%
  mutate(GP = PP - R_average) %>%
  mutate(#GP = ifelse(PAR == 0, NA, GP),# remove GP from any of the night data 
         GP = ifelse(PAR == 0, NA, GP),
         GP = ifelse(GP<0, NA, GP),
         Temperature_mean = (UP_Temp+ DN_Temp)/2, # average temperature for the site
         Flow_mean = (UP_Velocity_mps+DN_Velocity_mps)/2) # average flow for the site


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

## Calculate the total percent of calcifiers
Total_Calc<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines"))%>%
  group_by(Year, Site)%>%
  reframe(total_Calc = mean_cover[name == "Coral"]+
            mean_cover[name == "Crustose Corallines"])

TotalLiving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, Site)%>%
  summarise(mean_alive = sum(mean_cover),
            mean_fleshy = sum(mean_cover[name == "Fleshy Macroalgae"]),
            mean_coral = sum(mean_cover[name == "Coral"]))


  
Seasonal_Averages <-All_PP_data %>%
  mutate(NP = PP,# remove nighttime respiration for average NP
         R = ifelse(PP<0, PP, NA) # only include night data for R
  ) %>% 
  group_by(Year, Season) %>%
  summarise(NP_mean = mean(NP, na.rm = TRUE),
            NP_SE = sd(NP, na.rm = TRUE)/sqrt(n()),
            NP_max = max(NP, na.rm = TRUE),
            GP_mean = mean(GP, na.rm = TRUE),
            GP_SE = sd(GP, na.rm = TRUE)/sqrt(n()),
            R_mean = mean(R, na.rm = TRUE),
            R_SE = sd(R, na.rm = TRUE)/sqrt(n()),
            Temperature_mean = mean(Temperature_mean, na.rm = TRUE),
            Flow_mean = mean(Flow_mean, na.rm = TRUE),
            PAR_mean = mean(PAR[PAR>0], na.rm = TRUE)
  ) %>%
  left_join(TotalLiving %>%
              filter(Site == "LTER 1")) %>%
  left_join(NEC %>% 
              group_by(Year,Season, Day_Night)%>% 
              summarise(mean_NEC = mean(NEC,na.rm = TRUE)) %>%
              pivot_wider(names_from = Day_Night, values_from = mean_NEC, names_prefix = "NEC_"))

daily_data<-All_PP_data %>%
  group_by(Year, Season, DielDate)%>%
  summarise(daily_GP = mean(GP,na.rm = TRUE),
            daily_P = mean(PP[PP>0], na.rm = TRUE),
            daily_R = mean(PP[PP<0], na.rm = TRUE),
            daily_temp = mean(Temperature_mean, na.rm = TRUE),
            daily_PAR = mean(PAR[PAR>0], na.rm = TRUE),
            daily_flow = mean(Flow_mean, na.rm = TRUE),
            daily_NP = mean(PP, na.rm = TRUE),
            daily_NP_max = max(PP, na.rm = TRUE)) %>%
  mutate(P_R = abs(daily_GP/daily_R))%>% # average P to average R
  left_join(TotalLiving %>%
              filter(Site == "LTER 1"))

#### yearly averages
Year_Averages <-All_PP_data %>%
  mutate(NP = PP,# remove nighttime respiration for average NP
         R = ifelse(PP<0, PP, NA) # only include night data for R
  ) %>% 
  group_by(Year) %>%
  summarise(NP_mean = mean(NP, na.rm = TRUE),
            NP_SE = sd(NP, na.rm = TRUE)/sqrt(n()),
            NP_max = max(NP, na.rm = TRUE),
            GP_mean = mean(GP, na.rm = TRUE),
            GP_SE = sd(GP, na.rm = TRUE)/sqrt(n()),
            R_mean = mean(R, na.rm = TRUE),
            R_SE = sd(R, na.rm = TRUE)/sqrt(n()),
            Temperature_mean = mean(Temperature_mean, na.rm = TRUE),
            Flow_mean = mean(Flow_mean, na.rm = TRUE),
            PAR_mean = mean(PAR[PAR>0], na.rm = TRUE)
  ) %>%
  left_join(TotalLiving %>%
              filter(Site == "LTER 1")) %>%
  left_join(NEC %>% 
              group_by(Year,Day_Night)%>% 
              summarise(NEC_mean = mean(NEC,na.rm = TRUE),
                        NEC_SE = sd(NEC, na.rm = TRUE)/sqrt(n())) %>%
              pivot_wider(names_from = Day_Night, values_from = c(NEC_mean, NEC_SE)))

daily_data<-All_PP_data %>%
  group_by(Year, Season, DielDate)%>%
  summarise(daily_GP = mean(GP,na.rm = TRUE),
            daily_P = mean(PP[PP>0], na.rm = TRUE),
            daily_R = mean(PP[PP<0], na.rm = TRUE),
            daily_temp = mean(Temperature_mean, na.rm = TRUE),
            daily_PAR = mean(PAR[PAR>0], na.rm = TRUE),
            daily_flow = mean(Flow_mean, na.rm = TRUE),
            daily_NP = mean(PP, na.rm = TRUE),
            daily_NP_max = max(PP, na.rm = TRUE)) %>%
  mutate(P_R = abs(daily_GP/daily_R))%>% # average P to average R
  left_join(TotalLiving %>%
              filter(Site == "LTER 1"))



## there are a few P/R values that don't make sense, which days are they and why (there are 5 days)
#daily_data %>%
#  filter(P_R>4)

daily_data %>%
  ggplot(aes(x = daily_temp, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")
  #geom_smooth(method = 'nls', formula = "y ~ a*x^b", start = list(a=1,b=1),se=FALSE)

daily_data %>%
  ggplot(aes(x =daily_flow, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.01)) # 2 decimal places
  
daily_data %>%
  ggplot(aes(x =daily_PAR, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.01)) # 2 decimal places

daily_data %>%
  ggplot(aes(x =mean_alive, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  #scale_x_continuous(trans = "log",
  #                   labels = label_number(accuracy = 0.01)) + # 2 decimal places
  facet_wrap(~Season)

#standardized 
std.data<-daily_data %>%
  mutate(temp_scale = scale(daily_temp),
         flow_scale = scale(log(daily_flow)),
         PAR_scale = scale(daily_PAR))


Seasonal_Averages %>%
  ggplot(aes(x = Year, y = GP_mean ))+
  geom_point()+
  geom_errorbar(aes(ymin = GP_mean - GP_SE, ymax = GP_mean+GP_SE))+
  geom_smooth(method = "lm")

daily_data %>%
  ggplot(aes(x = Year, y = daily_GP))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)

Seasonal_Averages %>%
  ggplot(aes(x = Year, y = R_mean ))+
  geom_errorbar(aes(ymin = R_mean - R_SE, ymax = R_mean+R_SE))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)

Seasonal_Averages %>%
  #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
  ggplot(aes(x = Year, y = NP_mean,color = Season ))+
  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_point()+
  geom_smooth(method = "lm", formula = "y~poly(x,2)")+
  facet_wrap(~Season)
  
Seasonal_Averages %>%
  #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
  ggplot(aes(x = Year, y = abs(GP_mean/R_mean),color = mean_alive ))+
  #geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_point()+
  geom_smooth(method = "lm", formula = "y~poly(x,2)")+
  facet_wrap(~Season, scales = "free")
###########################################



#  NP_model<-brm(
#    bf(NP_mean~Year + I(Year^2) , nu = 3), data = Seasonal_Averages, 
#                family = "student", control = list(max_treedepth = 14))


NP_all<-  Seasonal_Averages %>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = NP_mean*12))+
  #  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_hline(yintercept = 0, lty = 2)+
    geom_point()+
    geom_smooth(data = Seasonal_Averages %>% filter(Season == "Summer"), method = "lm", formula = "y~x + I(x^2)", color = "black")+
  geom_smooth(data = Seasonal_Averages %>% filter(Season == "Winter"), method = "lm", color = "black")+
  labs(x = "",
       y = expression(atop("Net ecosystem production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
    facet_wrap(~Season)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"))

NP_year<-Year_Averages %>%
  ggplot(aes(x = Year, y = NP_mean*12)) + # * 12 to get the rate in per day
  geom_hline(yintercept = 0, lty = 2)+
  geom_smooth(method = "gam",color = "black", alpha = 0.2)+
  geom_errorbar(aes(ymin = NP_mean*12 - NP_SE*12, ymax =NP_mean*12 + NP_SE*12, color = mean_coral ), 
                width = 0, linewidth = 2, alpha = 0.2)+
  geom_point(aes(color = mean_coral))+
 
#  geom_smooth(method = "nls",
#              formula = y ~ a * x / (b + x) +c,
#              method.args = list(start = list(a = 450, b = 2020,c = -350)),
#              se = FALSE,
#              color = "red",
#              linewidth = 1.2) +
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
  labs(x = "",
       y = expression(atop("Net ecosystem production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"))


GP_all<-  Seasonal_Averages %>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = GP_mean*12))+
    #  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
    geom_point()+
    geom_smooth(data = Seasonal_Averages %>% filter(Season == "Summer"), method = "lm", formula = "y~x + I(x^2)", color = "black")+
  geom_smooth(data = Seasonal_Averages %>% filter(Season == "Winter"), method = "lm", color = "black")+
  labs(x = "",
       y = expression(atop("Gross production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
    facet_wrap(~Season)+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_blank())

GP_year<- Year_Averages %>%
  ggplot(aes(x = Year, y = GP_mean*12))+
  geom_smooth(method = "lm", color = "black", alpha = 0.2)+
  geom_errorbar(aes(ymin = GP_mean*12 - GP_SE*12, ymax =GP_mean*12 + GP_SE*12, color = mean_coral ), 
                width = 0, linewidth = 2, alpha = 0.2)+
  geom_point(aes(color = mean_coral))+
  scale_color_viridis(option = "rocket", name = "% Coral Cover")+
  labs(x = "",
       y = expression(atop("Gross production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_blank())
  
R_all<-  Seasonal_Averages %>%
  #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
  ggplot(aes(x = Year, y = R_mean*12))+
  #  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_point()+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "",
       y = expression(atop("Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  facet_wrap(~Season)+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_text(size = 12))

R_year<- Year_Averages %>%
  ggplot(aes(x = Year, y = R_mean*12))+
  geom_smooth(method = "lm", color = "black", alpha = 0.2)+
  geom_errorbar(aes(ymin = R_mean*12 - R_SE*12, ymax =R_mean*12 + R_SE*12, color = mean_coral ), 
                width = 0, linewidth = 2, alpha = 0.2)+
  geom_point(aes(color = mean_coral))+
  scale_color_viridis(option = "rocket", name = "% Coral Cover")+
  labs(x = "",
       y = expression(atop("Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_text(size = 12))

NEC_Day_year<- Year_Averages %>%
  ggplot(aes(x = Year, y = NEC_mean_Day))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_smooth(method = "lm", color = "black", alpha = 0.2)+
  geom_errorbar(aes(ymin = NEC_mean_Day - NEC_SE_Day, ymax =NEC_mean_Day + NEC_SE_Day, color = mean_coral ), 
                width = 0, linewidth = 2, alpha = 0.2)+
  geom_point(aes(color = mean_coral))+
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
  labs(x = "",
       y = expression(atop("Day Calcification",paste("(mmol CaCO"[3]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_blank())

NEC_Night_year<- Year_Averages %>%
  ggplot(aes(x = Year, y = NEC_mean_Night))+
  geom_hline(yintercept = 0, linetype = 2)+
 # geom_smooth(method = "gam", color = "black", alpha = 0.2)+
  geom_errorbar(aes(ymin = NEC_mean_Night - NEC_SE_Night, ymax =NEC_mean_Night + NEC_SE_Night, color = mean_coral ), 
                width = 0, linewidth = 2, alpha = 0.2)+
  geom_point(aes(color = mean_coral))+
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
  labs(x = "",
       y = expression(atop("Night Calcification",paste("(mmol CaCO"[3]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.text.x = element_text(size = 12))

NP_all/GP_all/R_all&theme(panel.grid.minor = element_blank(),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 14))&lims(x = c(2008,2025))

ggsave(here("Output","SeasonalRates_time.png"), height = 8, width = 6)


((NP_year/GP_year/R_year)|(plot_spacer()/NEC_Day_year/NEC_Night_year))+plot_layout(guides = "collect")&theme(panel.grid.minor = element_blank(),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 14), legend.position = "bottom", legend.title.position = "top")&lims(x = c(2008,2025))

ggsave(here("Output","YearlyRates_time.png"), height = 10, width = 8)


Seasonal_Averages %>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = GP_mean,color = mean_alive ))+
    #  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
    geom_point()+
    geom_smooth(method = "lm", formula = "y~x + I(x^2)")+
    facet_wrap(~Season, scales = "free")
  
  Seasonal_Averages %>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = PAR_mean))+
    #geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
    geom_point()+
    geom_smooth(method = "lm")+
    facet_wrap(~Season, scales = "free")
  
  All_PP_data %>%
  ggplot(aes(x = PAR, y = PP))+
  geom_point(aes(color = as.factor(DielDate)))+
  facet_wrap(Year~Season, scale = "free")

All_PP_data %>%
  filter(PAR>0) %>% # Only grab data with light
  mutate(PAR = PAR/1000000) %>% # conver to mols 3600*12/1000000
  group_by(Year, Season, Date)%>%
  summarise(TotalPAR = mean(PAR, na.rm = TRUE))%>% # get the daily total PAR
  mutate(TotalPAR = TotalPAR*60*60*12)%>%
  group_by(Year, Season) %>% # get average by deployment
  summarise(TotalPAR_mean = mean(TotalPAR, na.rm = TRUE),
            TotalPAR_SE = sd(TotalPAR, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Year, y = TotalPAR_mean, color = Season))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)


All_PP_data %>%
  filter(PAR>0) %>% # Only grab data with light
  mutate(PAR = PAR/1000000) %>% # conver to mols 3600*12/1000000
  group_by(Year, Season, DielDate)%>%
  summarise(DailyMaxPAR = mean(PAR, na.rm = TRUE))%>% # get the daily total PAR
  mutate(DailyMaxPAR = DailyMaxPAR*60*60*12)%>%
  group_by(Year, Season) %>% # get average by deployment
  summarise(TotalPAR_mean = mean(DailyMaxPAR, na.rm = TRUE),
            TotalPAR_SE = sd(DailyMaxPAR, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Year, y = TotalPAR_mean, color = Season))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)
  

## calcualte % time less than 1000 (will replace with Ik)
All_PP_data %>%
  mutate(less_1000 = ifelse(PAR<1200,1,0))%>%
  group_by(Year, Season, DielDate)%>%
  summarise(less_counts = sum(less_1000),
            n = n()# hours in the timeseries, should be 24
  ) %>%
  mutate(percent_less = less_counts/n*100) %>%
  group_by(Year, Season) %>% 
  summarise(mean_percent = mean(percent_less, na.rm = TRUE))%>%
  ggplot(aes(x = Year, y = mean_percent, color = Season))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Season)

  
All_PP_data %>%
  filter(PAR>0)%>%
  ggplot(aes(x = PAR, y = Year, group = Year, height = after_stat(density)))+
  geom_density_ridges(alpha = 0.5, stat = "density")+
  scale_x_continuous(trans = "log")+
  facet_wrap(~Season, scale = "free_x")



All_PP_data %>%
  #filter(PAR > 0)%>%
  ggplot(aes(x = PAR, y = PP, group = DielDate, color = DielDate))+
  geom_point()+
 # geom_smooth(method = "lm")+
  facet_wrap(~DielDate)
  
All_PP_data %>%
    filter(PAR>0)%>%
    ggplot(aes(x = mean_solar_rad_kwpm2, y = PAR))+
    geom_point()
    #facet_wrap(~DielDate)
  
  All_PP_data %>%
    filter(PAR>0)%>%
    group_by(Year, Season)%>%
    summarise(mean_solar = mean(mean_solar_rad_kwpm2, na.rm = TRUE),
              mean_PAR = mean(PAR, na.rm = TRUE))%>%
    ggplot(aes(y = mean_solar, x = Year, color = Season))+
    geom_point()+
    geom_smooth(method = "lm")
   

  All_PP_data %>%
    filter(Year == 2021)%>%
    ggplot(aes(x = DateTime, y = PP))+
    geom_point(aes(color = PAR), size = 2)+
 #   scale_color_continuous(trans = "log")+
    geom_line()+
    geom_hline(yintercept = 0)+
    facet_wrap(Year~Season, scales = "free_x")+
    theme_bw()
    #geom_smooth()
  
  
  All_PP_data %>%
    filter(Year == 2021)%>%
    ggplot(aes(x = PAR, y = PP, group = Date))+
    geom_point(aes(color = PAR), size = 2)+
    #   scale_color_continuous(trans = "log")+
    #geom_smooth()+
    geom_hline(yintercept = 0)+
    facet_wrap(~DielDate)
  
  
  All_PP_data<-All_PP_data %>%
    left_join(TotalLiving %>%
                filter(Site == "LTER 1"))

  LTER1_Pnet <-All_PP_data %>%
    mutate(tempc = Temperature_mean,
           Flowmean = Flow_mean,
           cover = mean_alive,
           coral = mean_coral,
           flow_log = log(Flowmean), # log scal the flow data since it fits in a power function
          # daily_R = -daily_R
           )%>%
    mutate(PARcenter = as.numeric(scale(PAR, center = TRUE, scale = FALSE)),
           Covercenter = as.numeric(scale(mean_alive, center = TRUE, scale = FALSE)),
           Tempcenter = as.numeric(scale(Temperature_mean, center = TRUE, scale = FALSE)),
           Flowcenter = as.numeric(scale(Flow_mean, center = TRUE, scale = FALSE)))

  ### Run PI curves allowing Rd and Pmax to change by cover, temperature, and flow based on what we know from biology
  ## add them in stepwise and then use loo for a model selection to chose the best fit model
  
 # the base model -  a PI curve where Pmax and Rd is constant 
  fit1_base<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
        nl = TRUE, alpha~1,Pmax~1,
       Rd~1)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
      ), 
  control = list(adapt_delta = 0.95, max_treedepth = 20), 
     cores = 4, 
    chains = 3, seed = 12, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
 
  # fit a Rd to vary by cover -  hypothesis that your Rd will drop with a lower biomass of benthic orgs  
  fit1_a<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~1,
       Rd~cover)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
    ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 4, 
    chains = 3, seed = 12, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
  
  # vary pmax by flow - addition to cover and Rd, Pmax will also increase with log flow 
  fit1_b<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log,
       Rd~coral)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
    ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
     cores = 4, 
    chains = 3, seed = 12, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
 
# add in temperature to Rd, Rd should increase with temperature--- This is taking forever
  
  fit1_c<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log,
       Rd~cover+tempc)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
     ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 4, 
    chains = 3, seed = 13, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 

  
  # add in temperature to Rd, Rd should increase with temperature -  also taking forever
  ### best fit so far
  fit1_f<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log+tempc+cover,
       Rd~flow_log+tempc+coral)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
    ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 3, 
    chains = 3, seed = 14, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
  ## add in the average daily values of flow, temp, cover for the models
  
  fit1_f<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))+Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log+tempc+cover+(1|DielDate),
       Rd~flow_log+tempc+coral+(1|DielDate))+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
#    prior = c(
#      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
#    ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 3, 
    chains = 3, seed = 13, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
 

  LTER1_Pnet <-LTER1_Pnet %>%
    mutate(UPDN = as.factor(UPDN),
           Year = as.factor(Year))
  
  fit1_f<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))+Rd, 
       nl = TRUE, alpha~0+Year,Pmax~0+Year,
       Rd~0+Year)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
        prior = c(
          prior(normal(0.1,10), nlpar = "alpha", lb = 0),
          prior(normal(-100,10), nlpar = "Rd", ub = 0), 
          prior(normal(100,10), nlpar = "Pmax", lb = 0) 
        ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 3, 
    chains = 3, seed = 223, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
  
  names<-rownames(fixef(fit1_f))
  
  coefficients<-as_tibble(fixef(fit1_f)) %>%
    mutate(params = names)
  
  coefficients %>%
    separate(params, sep = "_",into = c("param_type","yearname"), remove = FALSE) %>%
    filter(param_type == "Pmax") %>%
    separate(yearname, sep = "r", into = c("y","year")) %>%
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(x = year, y = Estimate))+
    geom_point()

  Pmax_coefs<-coefficients %>%
    separate(params, sep = "_",into = c("param_type","yearname"), remove = FALSE) %>%
    filter(param_type == "Pmax") %>%
    separate(yearname, sep = "r", into = c("y","Year")) %>%
    mutate(Year = as.numeric(Year)) %>%
    left_join(Year_Averages) 

  Pmax_coefs %>%
    ggplot(aes(x = mean_alive, y = Estimate))+
    geom_point()+
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0)+
    coord_trans(x = "log")+
    geom_smooth(method = "lm")+
    labs(x = "coral cover",
         y = "Predicted Pmax")
    
Pmaxyear<-  Pmax_coefs %>%
    ggplot(aes(x = Year, y = Estimate))+
    geom_errorbar(aes( ymin = Q2.5, ymax = Q97.5, color = mean_coral), width = 0, linewidth = 2, alpha = 0.5)+
    geom_point()+
  #  coord_trans(x = "log")+
    geom_smooth(method = "lm", color = "black")+
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
      labs(x = "Year",
           y = expression(atop("Maximum Photosynthetic Capacity",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()

# how much does Pmax change by year
Pmax_model_year<-brm(Estimate|se(Est.Error)~Year, data = Pmax_coefs) # keeping the error propagated (obervation level standard error)
  
  Resp_coefs<-coefficients %>%
    separate(params, sep = "_",into = c("param_type","yearname"), remove = FALSE) %>%
    filter(param_type == "Rd") %>%
    separate(yearname, sep = "r", into = c("y","Year")) %>%
    mutate(Year = as.numeric(Year)) %>%
    left_join(Year_Averages) 
 
 Resp_coefs %>%
    ggplot(aes(x = R_mean, y = Estimate))+
    geom_point()+
    geom_errorbar(aes(x = R_mean, ymin = Q2.5, ymax = Q97.5), width = 0)+
   geom_errorbarh(aes(xmin = R_mean-R_SE, xmax = R_mean+R_SE), height = 0)+
    #coord_trans(x = "log")+
    geom_smooth(method = "lm")+
 #   geom_abline(a = 1, b = 0)+
    labs(x =  expression(atop("Measured Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))),
         y = expression(atop("Predicted Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
   theme_bw()
 ggsave(here("Output","MeasuredVsPredictedR.pdf"), width = 4, height = 4)
 
  Resp_coefs %>%
    ggplot(aes(x = mean_coral, y = -Estimate))+
    geom_point()+
    geom_errorbar(aes(x = mean_coral, ymin = -Q2.5, ymax = -Q97.5), width = 0)+
    coord_trans(x = "log")+
    geom_smooth(method = "lm")+
    labs(x = "coral cover",
         y = "Predicted R")
  
    Resp_coefs %>%
    ggplot(aes(x = Temperature_mean, y = Estimate))+
    geom_point()+
    geom_errorbar(aes( ymin = Q2.5, ymax = Q97.5), width = 0)+
   # coord_trans(x = "log")+
  #  geom_smooth(method = "lm")+
    labs(x = "coral cover",
         y = "Predicted R")
    
Respyear<-Resp_coefs %>%
      ggplot(aes(x = Year, y = -Estimate))+
      geom_errorbar(aes( ymin = -Q2.5, ymax = -Q97.5, color = mean_coral), width = 0, linewidth = 2, alpha = 0.5)+
      geom_point()+
      # coord_trans(x = "log")+
        geom_smooth(method = "lm", color = "black")+
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
        labs(x = "Year",
           y = expression(atop("Ecosystem Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()


# how much does Rd change by year
Rd_model_year<-brm(-Estimate|se(Est.Error)~Year, data = Resp_coefs) # keeping the error propagated (obervation level standard error)

alpha_coefs<-coefficients %>%
      separate(params, sep = "_",into = c("param_type","yearname"), remove = FALSE) %>%
      filter(param_type == "alpha") %>%
      separate(yearname, sep = "r", into = c("y","Year")) %>%
      mutate(Year = as.numeric(Year)) %>%
      left_join(Year_Averages)   
    
alphayear<-  alpha_coefs%>%
     # filter(!Year %in% c(2011,2021))%>%
      ggplot(aes(x = Year, y = Estimate))+
      geom_point()+
      geom_errorbar(aes( ymin = Q2.5, ymax = Q97.5), width = 0)+
      # coord_trans(x = "log")+
        geom_smooth(method = "lm")+
      labs(x = "Year",
           y = "Predicted alpha")+
  theme_bw()

## How much does NP change from Year to year using the average data and SE
NP_model_year<-brm(NP_mean|se(NP_SE)~poly(Year,2), 
                   data = Year_Averages,  family = gaussian()) # keeping the error propagated (observation level standard error)


## How much does Day calc change from Year to year using the average data and SE
NEC_model_year<-brm(NEC_mean_Day|se(NEC_SE_Day)~Year, data = Year_Averages) # keeping the error
draws_NEC_total<-NEC_model_year %>%
  spread_draws(b_Year) %>%
  as_tibble() 
NEC_density<-density(draws_NEC_total$b_Year)
plotdata_NEC_total<-as_tibble(bind_cols(x = NEC_density$x, y=NEC_density$y)) %>%
  mutate(param = "NEC Day")


NEC_model_year_Night<-brm(NEC_mean_Night|se(NEC_SE_Night)~Year, data = Year_Averages) # keeping the error 
draws_NEC_total_Night<-NEC_model_year_Night %>%
  spread_draws(b_Year) %>%
  as_tibble() 
NEC_density_Night<-density(draws_NEC_total_Night$b_Year)
plotdata_NEC_total_Night<-as_tibble(bind_cols(x = NEC_density_Night$x, 
                                              y=NEC_density_Night$y)) %>%
  mutate(param = "NEC Night")

draws_NP<-NP_model_year %>%
  spread_draws(b_polyYear21) %>%
  as_tibble()
NP_density<-density(draws_NP$b_polyYear21)
plotdata_NP<-as_tibble(bind_cols(x = NP_density$x, 
                                              y=NP_density$y)) %>%
  mutate(param = "NP")

draws_Rd<-Rd_model_year %>%
  spread_draws(b_Year) %>%
  as_tibble() 
Rd_density<-density(draws_Rd$b_Year)
plotdata_Rd<-as_tibble(bind_cols(x = Rd_density$x, 
                                              y=Rd_density$y)) %>%
  mutate(param = "Rd")

draws_Pmax<-Pmax_model_year %>%
  spread_draws(b_Year) %>%
  as_tibble() 
Pmax_density<-density(draws_Pmax$b_Year)
plotdata_Pmax<-as_tibble(bind_cols(x = Pmax_density$x, 
                                 y=Pmax_density$y)) %>%
  mutate(param = "Pmax")

plotdata<-bind_rows(plotdata_NEC_total, 
                    plotdata_NEC_total_Night,
                    plotdata_NP,
                    plotdata_Rd,
                    plotdata_Pmax)

change_plot<-ggplot(plotdata, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  geom_area(fill =  "skyblue", alpha = 0.5) +
  geom_line(linewidth = 1.3)+
  facet_wrap(~param, ncol = 1, scale = "free",strip.position = "left")+
  labs (x = "Rate of change per year",
        y = "Density")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 90))

# get the conditional effects to make a plots
ce<- conditional_effects(fit1_f, effects = "PAR")
ce<-as_tibble(ce$PAR) %>%
  left_join(Year_Averages %>% select(Year, mean_coral) %>% mutate(Year = as.factor(Year)))

PI_plot<-ggplot()+
  geom_hline(yintercept = 0, lty = 2)+
  geom_ribbon(data = ce, aes(x = PAR, ymin = lower__, ymax = upper__), alpha = 0.75, fill = "grey")+
  geom_line(data = ce, aes(x = PAR, y = estimate__), linewidth = 1)+
  geom_point(data = LTER1_Pnet, aes(x = PAR, y = PP, color = mean_coral),
             inherit.aes=FALSE, alpha=0.05)+
  scale_color_viridis(option = "rocket", name = "% Coral Cover", 
                      trans = "log", end =  0.75, direction  = 1,
                      breaks = c(1, 5, 10, 15, 20, 25, 30))+
  labs( x = expression(atop("Photosynthetic Active Radiation",paste("(",~mu~"mol photons", " m"^-2, " s"^-1,")"))),
        y = expression(atop("Net ecosystem production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

ggsave(here("Output","PIcurve.pdf"), width = 6, height = 6)

  
((PI_plot|NP_year&lims(x = c(2008,2025)))&theme(panel.grid.minor = element_blank(),
                                                legend.title = element_text(size = 14),
                                                legend.text = element_text(size = 12),
                                                axis.text.x = element_text(size = 12),
                                                axis.title.x = element_text(size = 14),
                                                axis.text.y = element_text(size = 12),
                                                axis.title.y = element_text(size = 14), 
                                                legend.position = "none"))/((Pmaxyear|NEC_Day_year)/(Respyear|NEC_Night_year)+plot_layout(guides = "collect")&theme(panel.grid.minor = element_blank(),
                                                                                        legend.title = element_text(size = 14),
                                                                                        legend.text = element_text(size = 12),
                                                                                        axis.text.x = element_text(size = 12),
                                                                                        axis.title.x = element_blank(),
                                                                                        axis.text.y = element_text(size = 12),
                                                                                        axis.title.y = element_text(size = 14), 
                                                                                        legend.position = "bottom", 
                                                                                        legend.title.position = "top")&lims(x = c(2008,2025))&guides(color = guide_colourbar(direction = "horizontal", barwidth = 15)))+plot_layout(heights = c(0.5,1,1))

ggsave(here("Output","Yearly_rates_modelpreds.pdf"), width = 10, height = 16, device = cairo_pdf)


# ((NP_year/Pmaxyear/Respyear)|(PI_plot/NEC_Day_year/NEC_Night_year))+plot_annotation(tag_levels = "a")+plot_layout(guides = "collect")&theme(panel.grid.minor = element_blank(),
#                                                                                                               legend.title = element_text(size = 14),
#                                                                                                               legend.text = element_text(size = 12),
#                                                                                                               axis.text.x = element_text(size = 12),
#                                                                                                               axis.title.x = element_blank(),
#                                                                                                               axis.text.y = element_text(size = 12),
#                                                                                                               axis.title.y = element_text(size = 14),
#                                                                                                               legend.position = "bottom", 
#                                                                                                               legend.title.position = "top",
#                                                                                                               plot.tag.position = c(0.95, .95))&lims(x = c(2008,2025))&guides(color = guide_colourbar(direction = "horizontal", barwidth = 15))





ggplot()+
geom_point(data = LTER1_Pnet, aes(x = PAR, y = PP),
           inherit.aes=FALSE, alpha=0.05)+
  facet_wrap(~Year)






############## Model the coefs ~ of temperature, cover, and flow ###########


Allcoefs<-Resp_coefs %>%
  select(Year, resp_effect = Estimate, resp2.5 = Q2.5, resp97.5 = Q97.5) %>%
  left_join(Pmax_coefs%>%
              select(Year, pmax_effect = Estimate, pmax2.5 = Q2.5, pmax97.5 = Q97.5)) %>%
  left_join(Year_Averages)


PredictionData<-Allcoefs %>%
  select(resp_effect, pmax_effect, Temperature_mean, Flow_mean, PAR_mean, mean_coral, NEC_mean_Day, NEC_mean_Night) %>%
  mutate(resp_effect = -resp_effect) %>% # make respiration positive 
  mutate(mean_coral = log(mean_coral))%>% # log transform the coral cover data
  mutate_at(vars(resp_effect:NEC_mean_Night), .funs = scale) %>%
  mutate_all(.funs = as.numeric)

mod_predictions_r<-brm(resp_effect~Temperature_mean+Flow_mean+ mean_coral, data =PredictionData )


## standardized coef plot for Respiration
Plot_rcoef<-fixef(mod_predictions_r) %>%
  as_tibble() %>%
  mutate(params = rownames(fixef(mod_predictions_r))) %>%
  filter(params != "Intercept") %>%
  mutate(nicenames = case_when(params == "Temperature_mean" ~"Temperature",
                               params == "mean_coral"~ "Coral Cover",
                               params == "Flow_mean"~ "Current Speed"))%>%
  mutate(sig = ifelse(params == "mean_coral",1, 0.75))%>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(aes(x = Estimate, y = nicenames, alpha = sig), size = 3)+
  geom_errorbarh(aes(xmin = Q2.5,xmax = Q97.5, y = nicenames, alpha = sig), height = 0)+
  scale_alpha(range = c(0.35,1))+
  labs(x = expression(atop("Standardized effect size","Respiration")),
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

## Make the coral plot on coral scale since it is the only significant one
mod_predictions_r<-brm(Estimate~Temperature_mean+Flow_mean+ log(mean_coral), data =Resp_coefs)

ce_r<- conditional_effects(mod_predictions_r)[[3]] 

resp_coral<-ce_r %>%
  ggplot()+
  geom_point(data = Resp_coefs, aes(x = mean_coral, y = -Estimate), inherit.aes = FALSE)+
  geom_ribbon(aes(x = mean_coral, ymin = -lower__, ymax = -upper__), alpha = 0.5, fill = "coral")+
  geom_line(aes(x = mean_coral, y = -estimate__))+
  labs(x = "Coral Cover (%)",
       y = expression(atop("Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
  

# photosynthesis
mod_predictions_p<-brm(pmax_effect~Temperature_mean+Flow_mean+ mean_coral, data =PredictionData )

## standardized coef plot for Pmax
Plot_pcoef<-fixef(mod_predictions_p) %>%
  as_tibble() %>%
  mutate(params = rownames(fixef(mod_predictions_p))) %>%
  filter(params != "Intercept") %>%
  mutate(nicenames = case_when(params == "Temperature_mean" ~"Temperature",
                               params == "mean_coral"~ "Coral Cover",
                               params == "Flow_mean"~ "Current Speed"))%>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(aes(x = Estimate, y = nicenames), size = 3)+
  geom_errorbarh(aes(xmin = Q2.5,xmax = Q97.5, y = nicenames), height = 0)+
  labs(x = expression(atop("Standardized effect size","Pmax")),
       y = "")+
  theme_bw()

        
# NEC
mod_predictions_NEC<-brm(NEC_mean_Day~Temperature_mean+Flow_mean+ mean_coral, data =PredictionData )

## standardized coef plot for Pmax
Plot_neccoef<-fixef(mod_predictions_NEC) %>%
  as_tibble() %>%
  mutate(params = rownames(fixef(mod_predictions_NEC))) %>%
  filter(params != "Intercept") %>%
  mutate(nicenames = case_when(params == "Temperature_mean" ~"Temperature",
                               params == "mean_coral"~ "Coral Cover",
                               params == "Flow_mean"~ "Current Speed"))%>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(aes(x = Estimate, y = nicenames), size = 3)+
  geom_errorbarh(aes(xmin = Q2.5,xmax = Q97.5, y = nicenames), height = 0)+
  labs(x = expression(atop("Standardized effect size","Daytime Net Ecosystem Calcification")),
       y = "")+
  theme_bw()


# NEC Night
mod_predictions_NEC_Night<-brm(NEC_mean_Night~Temperature_mean+Flow_mean+ mean_coral, data =PredictionData )

## standardized coef plot for NEC Night
Plot_necnightcoef<-fixef(mod_predictions_NEC_Night) %>%
  as_tibble() %>%
  mutate(params = rownames(fixef(mod_predictions_NEC_Night))) %>%
  filter(params != "Intercept") %>%
  mutate(nicenames = case_when(params == "Temperature_mean" ~"Temperature",
                               params == "mean_coral"~ "Coral Cover",
                               params == "Flow_mean"~ "Current Speed"))%>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(aes(x = Estimate, y = nicenames), size = 3)+
  geom_errorbarh(aes(xmin = Q2.5,xmax = Q97.5, y = nicenames), height = 0)+
  labs(x = expression(atop("Standardized effect size","Nighttime Net Ecosystem Calcification")),
       y = "")+
  theme_bw()

resp_coral |Plot_rcoef
ggsave(here("Output","Resp_coral.pdf"), width = 8, height = 4)


  ((Plot_rcoef|Plot_pcoef)/(Plot_neccoef|Plot_necnightcoef))

#################################################
# Do it for every season
LTER1_Pnet <-LTER1_Pnet %>%
  mutate(DielDatefactor = as.factor(DielDate))

fit1_g<-brm(
  bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))+Rd, 
     nl = TRUE, alpha~0+UPDN,Pmax~0+UPDN,
     Rd~0+UPDN)+ student(),
  data = LTER1_Pnet,
  set_rescor(FALSE),
  prior = c(
    prior(normal(0.1,10), nlpar = "alpha", lb = 0),
    prior(normal(-100,10), nlpar = "Rd", ub = 0),
    prior(normal(100,10), nlpar = "Pmax", lb = 0)
  ),
  control = list(adapt_delta = 0.95, max_treedepth = 20), 
  cores = 3, 
  chains = 3, seed = 223, iter = 8000, warmup = 2000
  #silent = TRUE
) 



names<-rownames(fixef(fit1_g))

coefficients_UPDN<-as_tibble(fixef(fit1_g)) %>%
  mutate(params = names)

# get the year and season for UPDN
meta_UPDN <-All_PP_data %>%
  ungroup() %>% 
  select(UPDN, Year, Season) %>% 
  distinct()

Pmax_coefs_season<-coefficients_UPDN %>%
  separate(params, sep = "_",into = c("param_type","UPDNname"), remove = FALSE) %>%
  filter(param_type == "Pmax") %>%
  separate(UPDNname, sep = "N", into = c("y","UPDN")) %>%
  mutate(UPDN = as.numeric(UPDN)) %>%
  left_join(meta_UPDN) %>%
  left_join(Seasonal_Averages)

Pmax_coefs_season %>%
  ggplot(aes(y = NEC_Day, x = Estimate))+
  geom_point()+
 # coord_trans(x = "log")+
  geom_smooth(method = "lm")
  #geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), width = 0)

Pmax_coefs_season %>%
  ggplot(aes(x = mean_coral , y = NEC_Day))+
  geom_point()+
  # coord_trans(x = "log")+
  geom_smooth(method = "lm")

R_coefs_season<-coefficients_UPDN %>%
  separate(params, sep = "_",into = c("param_type","UPDNname"), remove = FALSE) %>%
  filter(param_type == "Rd") %>%
  separate(UPDNname, sep = "N", into = c("y","UPDN")) %>%
  mutate(UPDN = as.numeric(UPDN)) %>%
  left_join(meta_UPDN) %>%
  left_join(Seasonal_Averages)

R_coefs_season %>%
  ggplot(aes(x = Flow_mean, y = Estimate))+
  geom_point()+
  #coord_trans(x = "log")+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0)

Alpha_coefs_season<-coefficients_UPDN %>%
  separate(params, sep = "_",into = c("param_type","UPDNname"), remove = FALSE) %>%
  filter(param_type == "alpha") %>%
  separate(UPDNname, sep = "N", into = c("y","UPDN")) %>%
  mutate(UPDN = as.numeric(UPDN)) %>%
  left_join(meta_UPDN) %>%
  left_join(Seasonal_Averages)

Alpha_coefs_season %>%
  filter(Estimate<2)%>%
  ggplot(aes(x = Year, y = Estimate))+
  geom_point()+
  #coord_trans(x = "log")+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0)


#############################################################
test<-ranef(fit1_f)
  rn<-as_tibble(test)
  rn<-rownames(rn$DielDate)
  test<-as_tibble(test[[1]])
  test<-rn %>% bind_cols(test) %>% rename("DielDate" = `...1`) 
  test1<-LTER1_Pnet %>% group_by(DielDate)%>% summarise(corals = mean(coral), temp = mean(tempc), flow = mean(Flow_mean), prod = mean(mean_alive), R = mean(R_average), Pmax = max(PP, na.rm = TRUE)) %>% 
    left_join(test %>% mutate(DielDate = ymd(DielDate)))
  
  
  conditions <- data.frame(DielDate = unique(LTER1_Pnet$DielDate))
  rownames(conditions) <- unique(LTER1_Pnet$DielDate) 
  me_year <- conditional_effects(fit1_f, conditions = conditions, re_formula = NULL, method = "predict") 
  plot(me_year, ncol = 5, points = TRUE)
  
# testing pmax chaning with cover and flow  
  fit1_e<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log+cover,
       Rd~cover)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
    prior = c(
      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
    ), 
    control = list(adapt_delta = 0.95, max_treedepth = 20), 
    cores = 4, 
    chains = 3, seed = 16, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
  # testing pmax chaning with cover and flow  
#  fit1_f<-brm(
#    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
#       nl = TRUE, alpha~1,Pmax~flow_log+tempc,
#       Rd~cover+flow_log)+ student(),
#    data = LTER1_Pnet,
#    set_rescor(FALSE),
#    prior = c(
#      prior(normal(0.1,10), nlpar = "alpha", lb = 0) 
#    ), 
#    control = list(adapt_delta = 0.95, max_treedepth = 20), 
#    cores = 4, 
#    chains = 3, seed = 17, iter = 8000, warmup = 2000
#    #silent = TRUE
#  ) 
  
  loo_compare(loo(fit1_d),
              loo(fit1_e),
              loo(fit1_f))
  
  loo_compare(loo(fit1_base), 
              loo(fit1_a), 
              loo(fit1_b),
              loo(fit1_c),
              loo(fit1_d),
              loo(fit1_e))  
  

  pp_check(fit1_d)
    
  # nlf(Pmax ~ a*Flowmean^b)
  
  fit1<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
        nl = TRUE, alpha~1,
        Pmax ~ Flowmean + Tempcenter,
       Rd~Covercenter+Tempcenter+Flowmean)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
     prior = c(
       prior(normal(1,100), nlpar = "alpha", lb = 0) 
     #  prior(normal(100,100), nlpar = "Pmax", lb = 0)
    #  prior(normal(1000, 500), nlpar = "Rd", lb = 0)
     ), 
    control = list(adapt_delta = 0.99, max_treedepth = 20), 
    #  init = my_inits,
    cores = 4, 
    chains = 3, seed = 11, iter = 8000, warmup = 2000
    #silent = TRUE
  )
  
  pp_check(fit1, resp = "PP", ndraws = 20)+
    scale_x_continuous(limits = c(-200,200))
 # pp_check(fit1, resp = "Pmax", ndraws = 20)
#  pp_check(fit1, resp = "Rd", ndraws = 20)
  
  ### get the predictions for plots
  Plotdata<-conditional_effects(fit1_f)

  
RawData<- ggplot()+
  geom_point(data = All_PP_data, aes(x = PAR, y = PP, color = mean_coral), alpha = 0.08)+
  labs(x = expression(paste("PAR ("~mu~"mol photons m"^-2, " s"^-1,")", sep = "")),
       y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")),
       color = "% Coral Cover")+
  scale_color_gradient(low = "lightgray", high = "coral")+
  theme_bw()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.7),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 1))
  
  # PAR vs PP
P_PAR<-  as_tibble(Plotdata$PAR) %>%
    ggplot(aes(x = PAR, y = estimate__))+
 #   geom_point(data = All_PP_data, aes(x = PAR, y = PP), alpha = 0.05)+
    geom_line()+
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue", alpha = 0.5)+
    labs(x = expression(paste("PAR ("~mu~"mol photons m"^-2, " s"^-1,")", sep = "")),
         y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
    theme_bw()
  
  # Temp vs PP
P_temp<-  as_tibble(Plotdata$tempc) %>%
    ggplot(aes(x = tempc, y = estimate__))+
  #  geom_point(data = All_PP_data, aes(x = Temperature_mean, y = PP), alpha = 0.05)+
    geom_line()+
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue", alpha = 0.5)+
    labs(x = "Temperature ("~degree~"C)",
         y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
    theme_bw()
  
  # Temp vs PP
P_flow<-  as_tibble(Plotdata$flow_log) %>%
    ggplot(aes(x = exp(flow_log), y = estimate__))+
#    geom_point(data = All_PP_data, aes(x = Flow_mean, y = PP), alpha = 0.05)+
    geom_line()+
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue", alpha = 0.5)+
    labs(x = expression(paste("Flow (m s"^-1, ")")),
         y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
    theme_bw()
  
  # Temp vs PP
P_cover<-  as_tibble(Plotdata$cover) %>%
    ggplot(aes(x = cover, y = estimate__))+
 #   geom_point(data = All_PP_data, aes(x = mean_alive, y = PP), alpha = 0.05)+
    geom_line()+
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue", alpha = 0.5)+
    labs(x = "% cover of producers",
         y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
    theme_bw()

# Temp vs PP
P_coral<-  as_tibble(Plotdata$coral) %>%
  ggplot(aes(x = coral, y = estimate__))+
  #   geom_point(data = All_PP_data, aes(x = mean_alive, y = PP), alpha = 0.05)+
  geom_line()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue", alpha = 0.5)+
  labs(x = "% cover of coral",
       y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
  theme_bw()
  

((RawData/P_PAR/ P_flow)|(P_temp/P_cover/P_coral))&theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))  
ggsave(here("Output","BayesModelFits.png"), width = 8, height = 10)
##########################################################
  ### Looking at the raw data with each level of the model

Temp_raw<-Seasonal_Averages %>% 
  ggplot(aes(x = Temperature_mean, y = NP_mean*12))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "Temperature ("~degree~"C)",
       y = expression(atop("Mean NP", paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()


Temp_raw_resp<-Seasonal_Averages %>% 
  ggplot(aes(x = Temperature_mean, y = R_mean*12))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "Temperature ("~degree~"C)",
       y = expression(atop("Mean R", paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()

cover_raw<-Seasonal_Averages %>% 
  ggplot(aes(x = mean_alive, y = R_mean*12))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover",
       y = expression(atop("Mean Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()

cover_raw_NP<-Seasonal_Averages %>% 
  ggplot(aes(x = mean_alive, y = NP_max*12))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover",
       y = expression(atop("Max NEP",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()

flow_raw<-Seasonal_Averages %>% 
  ggplot(aes(x = Flow_mean, y = NP_max))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", formula = "y~log(x)", color = "black")+
  labs(x = expression(paste("Flow (m s"^-1,")")),
       y = expression(atop("Max NEP",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()

cover_raw/flow_raw/Temp_raw
ggsave(here("Output","RawDatafits.png"), height = 8, width = 6)


##########
  # Testing how much a 1 SD change in light, temperature, producer cover, and flow (model is on the log scale)
  
  # First calculate the mean value for each
  MeanValues<-All_PP_data %>%
  ungroup()%>%
  select(mean_alive,mean_coral, Flow_mean, Temperature_mean, PAR) %>%
  mutate(PAR = ifelse(PAR==0,NA, PAR)) %>% # only get the data in the light for PAR
  pivot_longer(mean_alive:PAR) %>%
  group_by(name)%>%
  summarise(MeanParam = median.default(value, na.rm = TRUE), # at the median value
            SDParam = sd(value, na.rm = TRUE)) %>%
  mutate(Plus_1_SD = MeanParam+SDParam,
         Minus_1_SD = MeanParam-SDParam) %>%
  mutate(Plus_1_SD = ifelse(name == "Flow_mean", log(Plus_1_SD), Plus_1_SD), # log transform the flow data to fit in the model
         Minus_1_SD = ifelse(name == "Flow_mean", log(Minus_1_SD), Minus_1_SD),
         MeanParam = ifelse(name == "Flow_mean", log(MeanParam), MeanParam)
         )

  # expand the dataframe so that we have an instance with everything held at the mean and one value changing at a time
PlusSD<-MeanValues %>% 
  select(name, MeanParam, Plus_1_SD) %>% 
  pivot_wider(names_from = name, values_from = Plus_1_SD) %>%
  mutate(Flow_mean = ifelse(is.na(Flow_mean),MeanParam,Flow_mean),
         PAR = ifelse(is.na(PAR),MeanParam,PAR),
         Temperature_mean = ifelse(is.na(Temperature_mean),MeanParam,Temperature_mean),
         mean_coral = ifelse(is.na(mean_coral ),MeanParam,mean_coral ),
         mean_alive = ifelse(is.na(mean_alive),MeanParam,mean_alive),
         ParamChange = c("flow_log","PAR","tempc","cover","coral")
  ) %>%
  pivot_longer(cols = MeanParam:mean_coral) %>%
  pivot_wider(names_from = 1, values_from = "value") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (26.6%)",
                               name == "mean_coral"~"Cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (1253 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (29.1 °C)",
                               name == "MeanParam" ~"Mean All")
  )

# minus 1 SD for each parameter
MinusSD<-MeanValues %>% 
  select(name, MeanParam, Minus_1_SD) %>% 
  pivot_wider(names_from = name, values_from = Minus_1_SD) %>%
  mutate(Flow_mean = ifelse(is.na(Flow_mean),MeanParam,Flow_mean),
         PAR = ifelse(is.na(PAR),MeanParam,PAR),
         Temperature_mean = ifelse(is.na(Temperature_mean),MeanParam,Temperature_mean),
         mean_alive = ifelse(is.na(mean_alive),MeanParam,mean_alive),
         mean_coral = ifelse(is.na(mean_coral),MeanParam,mean_coral ),
         ParamChange = c("flow_log","PAR","tempc","cover","coral")
  )  %>%
  pivot_longer(cols = MeanParam:mean_coral) %>%
  pivot_wider(names_from = 1, values_from = "value") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (10.2%)",
                               name == "mean_coral"~"Cover <br> (0.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)",
                               name == "MeanParam" ~"Mean All")
         
  )



### put in the fits
# PlusSD fits
posteriorpred_CI_plus <- PlusSD %>%
  add_epred_draws(fit1_f) %>%
  median_qi() %>% 
  arrange(.row) %>%
  mutate(group = ifelse(name == "MeanParam","Mean","+1 SD from median"))

# MinusSD fits together with plus
posteriorpred_CI <- MinusSD %>%
  add_epred_draws(fit1_f) %>%
  median_qi()%>% 
  arrange(.row)%>%
  mutate(group = ifelse(name == "MeanParam","Mean","-1 SD from median")) %>%
  filter(group != "Mean") %>%
  bind_rows(posteriorpred_CI_plus) %>%
  mutate(group = factor(group, levels = c("-1 SD from median","Mean","+1 SD from median")))

# calculate the percent change from the mean with error
Pluspercent<-PlusSD %>%
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "+1 SD from median") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (27.6%)",
                               name == "mean_coral"~"Coral cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (1253 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (29.1 °C)")
                        )

Minuspercent<- MinusSD %>%
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "-1 SD from median")%>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (11.3%)",
                               name == "mean_coral"~"Coral cover <br> (0.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)")
  )

# Make a plot
NP_percent<-Minuspercent %>%
  bind_rows(Pluspercent)%>%
  mutate(order = fct_reorder(Nicenames,percentchange)) %>%  
  mutate(labelspace = ifelse(percentchange>0,12,-12))%>%
ggplot(aes(x = order, y = percentchange))+
  geom_col()+
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0.1)+
  geom_text(aes(y = percentchange+labelspace, 
                label = paste0(round(percentchange,1),"%")))+
  labs(x = "",
       y = "% change in NEP")+
  facet_wrap(~group, scale = "free_x")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_markdown())
  ggsave(here("Output","Sensitivity.png"), width = 9, height = 6)


# plot with the actual data
backgroundmean<-posteriorpred_CI %>% filter(name == "MeanParam")
  
posteriorpred_CI %>%
  filter(name !="MeanParam") %>% #remove the mean
  ggplot(aes(x = Nicenames, y = .epred))+
  geom_rect(aes(ymin = backgroundmean$.lower, 
                ymax = backgroundmean$.upper, 
                xmin = -Inf, xmax = Inf), alpha = 0.5, fill = "firebrick")+
  geom_hline(yintercept = backgroundmean$.epred)+
  geom_col()+
  geom_errorbar(aes(ymin = .lower,ymax = .upper), width = 0.1)+
  facet_grid(~group, scale = "free_x", space = "free")+
  labs(x = "",
       y = expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")),
       caption = expression(paste("Median Values: Flow 0.13 (m s"^-1,"), PAR 688 ("~mu~"mol photons s"^-1,"), Temperature 28.5 ("~degree~C,"), Cover 19.4 (%)")))+
  theme_bw()+
  theme(axis.text = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(here("Output","Sensitivityraw.png"), width = 9, height = 6)
#######################
# DO the same thing but for respiration (set light to 0)
Pluspercent_R<-PlusSD %>%
  mutate(PAR = 0) %>% # only calculate in the dark
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "+1 SD from median") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (27.6%)",
                               name == "PAR" ~ "PAR <br> (1253 &mu;mol photon s<sup>-1</sup>)",
                               name == "mean_coral"~"Coral cover <br> (11.3%)",
                               name == "Temperature_mean"~"Temperature <br> (29.1 °C)")
         
  )

Minuspercent_R<- MinusSD %>%
  mutate(PAR = 0) %>% # only calculate in the dark
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "-1 SD from median")%>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (11.3%)",
                               name == "mean_coral"~"Coral cover <br> (0.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)")
  )

# Make a plot
R_percent<-Minuspercent_R %>%
  bind_rows(Pluspercent_R)%>%
  mutate(order = fct_reorder(Nicenames,percentchange)) %>%  
  mutate(labelspace = ifelse(percentchange>0,12,-12))%>%
  ggplot(aes(x = order, y = percentchange))+
  geom_col()+
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0.1)+
  geom_text(aes(y = percentchange+labelspace, 
                label = paste0(round(percentchange,1),"%")))+
  labs(x = "",
       y = "% change in ER")+
  facet_wrap(~group, scale = "free_x")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_markdown())
ggsave(here("Output","Sensitivity_R.png"), width = 9, height = 6)

## plot actual respiration data############
# PlusSD fits
posteriorpred_CI_plusR <- PlusSD %>%
  mutate(PAR = 0)%>%
  add_epred_draws(fit1_f) %>%
  median_qi() %>% 
  arrange(.row) %>%
  mutate(group = ifelse(name == "MeanParam","Mean","+1 SD from median"))

# MinusSD fits together with plus
posteriorpred_CI_R <- MinusSD %>%
  mutate(PAR = 0)%>%
  add_epred_draws(fit1_f) %>%
  median_qi()%>% 
  arrange(.row)%>%
  mutate(group = ifelse(name == "MeanParam","Mean","-1 SD from median")) %>%
  filter(group != "Mean") %>%
  bind_rows(posteriorpred_CI_plusR) %>%
  mutate(group = factor(group, levels = c("-1 SD from median","Mean","+1 SD from median")))

backgroundmeanR<-posteriorpred_CI_R %>% filter(name == "MeanParam")

posteriorpred_CI_R %>%
  filter(name !="MeanParam") %>% #remove the mean
  ggplot(aes(x = Nicenames, y = .epred))+
  geom_rect(aes(ymin = backgroundmeanR$.lower, 
                ymax = backgroundmeanR$.upper, 
                xmin = -Inf, xmax = Inf), alpha = 0.5, fill = "firebrick")+
  geom_hline(yintercept = backgroundmeanR$.epred)+
  geom_col()+
  geom_errorbar(aes(ymin = .lower,ymax = .upper), width = 0.1)+
  facet_grid(~group, scale = "free_x", space = "free")+
  labs(x = "",
       y = expression(paste("Respiration (mmol O"[2]," m"^-2, " hr"^-1,")")),
       caption = expression(paste("Median Values: Flow 0.13 (m s"^-1,"), PAR 688 ("~mu~"mol photons s"^-1,"), Temperature 28.5 ("~degree~C,"), Cover 19.4 (%)")))+
  theme_bw()+
  theme(axis.text = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

##### Calculate GP###################
# First I take the draws for NP and then I join them with the draws for R and add them together

MinusPercent_GP<-MinusSD %>%
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  left_join(MinusSD %>%
              mutate(PAR = 0)%>% # the respiration draws
              add_epred_draws(fit1_f) %>%
              ungroup()%>%
              select(name, .epred_R = .epred, .draw)) %>%
  mutate(.epred_GP = .epred - .epred_R) %>%
  select(-c(.epred,.epred_R))%>%
  pivot_wider(names_from = name, values_from = .epred_GP) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "-1 SD from median")%>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_coral"~"Coral cover <br> (0.3%)",
                               name == "mean_alive"~"Cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)")
  )

PlusPercent_GP<-PlusSD %>%
  add_epred_draws(fit1_f) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  left_join(MinusSD %>%
              mutate(PAR = 0)%>% # the respiration draws
              add_epred_draws(fit1_f) %>%
              ungroup()%>%
              select(name, .epred_R = .epred, .draw)) %>%
  mutate(.epred_GP = .epred - .epred_R) %>%
  select(-c(.epred,.epred_R))%>%
  pivot_wider(names_from = name, values_from = .epred_GP) %>%
  pivot_longer(cols = Flow_mean:mean_coral) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "+1 SD from median") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (27.6%)",
                               name == "mean_coral"~"Coral cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (1253 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (29.1 °C)")
  )

# Make a plot
GP_percent<-MinusPercent_GP %>%
  bind_rows(PlusPercent_GP)%>%
  mutate(order = fct_reorder(Nicenames,percentchange)) %>%  
  mutate(labelspace = ifelse(percentchange>0,12,-12))%>%
  ggplot(aes(x = order, y = percentchange))+
  geom_col()+
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0.1)+
  geom_text(aes(y = percentchange+labelspace, 
                label = paste0(round(percentchange,1),"%")))+
  labs(x = "",
       y = "% change in GP")+
  facet_wrap(~group, scale = "free_x")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_markdown())
#####################
## plot observed versus predicted values

predicted<-predict(fit1_f, newdata = LTER1_Pnet %>% select(PP,flow_log, PAR, tempc, coral, cover)) %>%
  bind_cols(LTER1_Pnet %>% select(PP,flow_log, PAR, tempc, cover,coral, Year, Season))

# plot the relationship between cover and Respiration with the model fits
Resp_predictions<-LTER1_Pnet %>% 
  select(PP,flow_log, PAR, tempc, cover,coral, Year, Season) %>%
  add_epred_draws(fit1_f) %>%
  ungroup() %>%
  filter(.epred<0) %>%
  group_by(Year, Season, coral) %>%
  median_qi(.epred, PP, tempc, flow_log) 

Resp_predictions %>%
  arrange(coral)%>%
  ggplot(aes(x = coral, y = .epred))+
  #geom_point()+
  geom_point()+
  geom_smooth(method = "lm")

Resp_predictions %>%
  ggplot(aes(x = tempc, y = .epred))+
  #geom_point()+
  geom_point()+
  geom_smooth(method = "lm")

Resp_predictions %>%
  ggplot(aes(x = flow_log, y = PP))+
  #geom_point()+
  geom_point()+
  geom_smooth(method = "lm")


P_predictions<-LTER1_Pnet %>% 
  select(PP,flow_log, PAR, tempc, cover,coral, Year, Season) %>%
  add_epred_draws(fit1_f) %>%
  ungroup() %>%
  filter(.epred>0) %>%
  select(tempc,flow_log, .epred, PP, Season, Year) %>%
  group_by(Year, Season) %>%
  summarise(PP_max = max(PP, na.rm = TRUE),
            epred_max = max(.epred, na.rm = TRUE),
            mean_temp = mean(tempc, na.rm = TRUE),
            mean_flow = mean(flow_log, na.rm = TRUE))



P_predictions %>%
  ggplot(aes(x = exp(mean_flow), y = epred_max))+
  geom_point()+
  geom_smooth(method = "lm", formula="y~log(x)")+
  theme_bw()


# calculate a psuedo-R2 (currently 0.813)
predicted %>%
  mutate(residual = PP-Estimate) %>%
  reframe(SSR = sum(residual^2),
          SST = sum((PP-mean(PP))^2),
          R2 = 1-(SSR/SST))

# plot the observed versus predicted values of the model
OP_plot<-predicted %>%
  ggplot(aes(x = PP, y = Estimate))+
  geom_point(alpha = 0.5)+
  labs(x = expression(paste("Observed NEP (mmol O"[2]," m"^-2, " hr"^-1,")")),
       y = expression(paste("Predicted NEP (mmol O"[2]," m"^-2, " hr"^-1,")")))+
  geom_abline(slope = 1, linewidth = 1, color = "blue")+
  annotate("text", x = -25, y = 150, label = expression(paste("R"^2,"=0.81")))+
  xlim(-100, 200)+
  ylim(-100,200)+
  theme_classic()

# posterior predictive checks
PP_plot<-pp_check(fit1_f)+
  labs(x =  expression(paste("NEP (mmol O"[2]," m"^-2, " hr"^-1,")")),
       y = "Density")+
  theme_classic()

OP_plot+PP_plot
ggsave(here("Output","PosteriorChecks.png"), width = 8, height = 4)


##### relationship between NEP and O2

    # Note Gross water column carbon fixation is 2.6 mmol C m-2 d-1 - alderidge
    # Look at table 1 from Oxygen metabolism of a fringing reef in French Polynesia A. SOURNIA (1976)
    All_PP_data %>% group_by(Year, Season, mean_alive, DielDate)%>% 
      summarise(mean_up = mean(UP_Oxy, na.rm = TRUE), 
                mean_dn = mean(DN_Oxy, na.rm = TRUE),
                mean_o2 = mean((UP_Oxy+DN_Oxy)/2, na.rm = TRUE),
                se_o2 = sd((UP_Oxy+DN_Oxy)/2, na.rm = TRUE)/sqrt(n()),
                mean_np = mean(PP, na.rm = TRUE),
                se_np = sd(PP,na.rm = TRUE)/sqrt(n())) %>% 
      ggplot(aes(x = mean_np, y = mean_o2))+
      geom_point(aes(color = Year), alpha = 0.5)+
   #   geom_errorbar(aes(ymin = mean_o2-se_o2, ymax = mean_o2+se_o2))+
  #    geom_errorbar(aes(xmin = mean_np-se_np, xmax = mean_np+se_np))+
      geom_smooth(method = "lm", color = "black")+
      labs(x = expression(paste("Daily mean NEP (mmol O"[2]," m"^-2, " hr"^-1,")")),
           y = expression(paste("Daily mean O"[2]," ("~mu~"mol L"^-1,")")))+
      scale_color_viridis_c(option = "E")+
      theme_bw()
ggsave(here("Output","NEP_O2.png"), width = 5, height = 4)  


### plot respiration vs Pmax

Allcoefs %>% 
  ggplot(aes(x = -resp_effect, y = pmax_effect))+
  geom_point()+
  geom_smooth(method = "lm", color = "black")+
  coord_trans(x = "log", y = "log")+
  labs(x = expression(atop("Ecosystem Respiration",paste("(mmol O"[2]," m"^-2, " hr"^-1,")"))),
       y = expression(atop("Maximum Photosynthetic Capacity",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave(here("Output","ER_Pmax.pdf"), width = 5, height = 5)
