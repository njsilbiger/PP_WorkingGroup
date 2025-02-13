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

### Read in the data ######

filedir<-here("Data","QC_PP") # file path for the QC PP files
files<-dir(path = filedir, pattern = ".csv", full.names = TRUE)

# bring in all the PP Data
All_PP_data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>%
  mutate(DateTime = mdy_hm(DateTime))


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
  summarise(mean_alive = sum(mean_cover))


  
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
            PAR_mean = mean(PAR, na.rm = TRUE)
  ) %>%
  left_join(TotalLiving %>%
              filter(Site == "LTER 1"))

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

pr.mod<-lmer(data = std.data, 
           formula = P_R ~temp_scale+flow_scale+ PAR_scale+(1|mean_alive))

intercepts<-tibble(mean_alive = as.numeric(rownames(ranef(pr.mod)$mean_alive)),
       intercept = as.numeric(ranef(pr.mod)$mean_alive[,1])) 

# how does cover modulate GP_R
intercepts %>%
  ggplot(aes(x = mean_alive, y = intercept))+
  geom_point()

anova(pr.mod)

gpp.mod<-lmer(data = std.data, 
             formula = daily_GP ~temp_scale+flow_scale+ PAR_scale+(1|mean_alive))

summary(gpp.mod)

intercepts<-tibble(mean_alive = as.numeric(rownames(ranef(gpp.mod)$mean_alive)),
                   intercept = as.numeric(ranef(gpp.mod)$mean_alive[,1])) 

intercepts %>%
  ggplot(aes(x = mean_alive, y = intercept))+
  geom_point()

r.mod<-lmer(data = std.data, 
              formula = -daily_R ~temp_scale+flow_scale+(1|mean_alive))

summary(r.mod)

intercepts<-tibble(mean_alive = as.numeric(rownames(ranef(r.mod)$mean_alive)),
                   intercept = as.numeric(ranef(r.mod)$mean_alive[,1])) 

intercepts %>%
  ggplot(aes(x = mean_alive, y = intercept))+
  geom_point()


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



  NP_model<-brm(
    bf(NP_mean~Year + I(Year^2) , nu = 3), data = Seasonal_Averages, 
                family = "student", control = list(max_treedepth = 14))


NP_all<-  Seasonal_Averages %>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = NP_mean*12))+
  #  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_hline(yintercept = 0, lty = 2)+
    geom_point()+
    geom_smooth(method = "lm", formula = "y~x + I(x^2)", color = "black")+
  labs(x = "",
       y = expression(atop("Net ecosystem production",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
    facet_wrap(~Season)+
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

NP_all/GP_all/R_all&theme(panel.grid.minor = element_blank(),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 14))&lims(x = c(2008,2024))

ggsave(here("Output","SeasonalRates_time.png"), height = 8, width = 6)

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
  
 
# add in temperature to pmax, Pmax should increase with temperature--- This is taking forever
  
  fit1_c<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
       nl = TRUE, alpha~1,Pmax~flow_log+tempc,
       Rd~cover)+ student(),
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
  fit1_d<-brm(
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
  Plotdata<-conditional_effects(fit1_d)
  
  # PAR vs PP
P_PAR<-  as_tibble(Plotdata$PAR) %>%
    ggplot(aes(x = PAR, y = estimate__))+
    #geom_point(data = All_PP_data, aes(x = PAR, y = PP), alpha = 0.05)+
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
  

(P_PAR + P_flow)/(P_temp+P_cover)  
ggsave(here("Output","BayesModelFits.png"), width = 8, height = 8)
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

cover_raw<-Seasonal_Averages %>% 
  ggplot(aes(x = mean_alive, y = R_mean*12))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "% Cover",
       y = expression(atop("Mean Respiration",paste("(mmol O"[2]," m"^-2, " d"^-1,")"))))+
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
  select(mean_alive, Flow_mean, Temperature_mean, PAR) %>%
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
         mean_alive = ifelse(is.na(mean_alive),MeanParam,mean_alive),
         ParamChange = c("flow_log","PAR","tempc","cover")
  ) %>%
  pivot_longer(cols = MeanParam:mean_alive) %>%
  pivot_wider(names_from = 1, values_from = "value") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (27.6%)",
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
         ParamChange = c("flow_log","PAR","tempc","cover")
  )  %>%
  pivot_longer(cols = MeanParam:mean_alive) %>%
  pivot_wider(names_from = 1, values_from = "value") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)",
                               name == "MeanParam" ~"Mean All")
         
  )



### put in the fits
# PlusSD fits
posteriorpred_CI_plus <- PlusSD %>%
  add_epred_draws(fit1_d) %>%
  median_qi() %>% 
  arrange(.row) %>%
  mutate(group = ifelse(name == "MeanParam","Mean","+1 SD from median"))

# MinusSD fits together with plus
posteriorpred_CI <- MinusSD %>%
  add_epred_draws(fit1_d) %>%
  median_qi()%>% 
  arrange(.row)%>%
  mutate(group = ifelse(name == "MeanParam","Mean","-1 SD from median")) %>%
  filter(group != "Mean") %>%
  bind_rows(posteriorpred_CI_plus) %>%
  mutate(group = factor(group, levels = c("-1 SD from median","Mean","+1 SD from median")))

# calculate the percent change from the mean with error
Pluspercent<-PlusSD %>%
  add_epred_draws(fit1_d) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_alive) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "+1 SD from median") %>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.2 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (27.6%)",
                               name == "PAR" ~ "PAR <br> (1253 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (29.1 °C)")
            
            )


Minuspercent<- MinusSD %>%
  add_epred_draws(fit1_d) %>%
  ungroup()%>%
  select(name, .epred, .draw) %>%
  pivot_wider(names_from = name, values_from = .epred) %>%
  pivot_longer(cols = Flow_mean:mean_alive) %>%
  mutate(percentchange = 100*(value-MeanParam)/MeanParam) %>%
  group_by(name)%>%
  median_qi(percentchange) %>%
  mutate(group = "-1 SD from median")%>%
  mutate(Nicenames = case_when(name == "Flow_mean" ~ "Flow <br> (0.07 m s<sup>-1</sup>)",
                               name == "mean_alive"~"Cover <br> (11.3%)",
                               name == "PAR" ~ "PAR <br> (124 &mu;mol photon s<sup>-1</sup>)",
                               name == "Temperature_mean"~"Temperature <br> (27.9 °C)")
         
  )

# Make a plot
Minuspercent %>%
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

#####################
## plot observed versus predicted values

predicted<-predict(fit1_d, newdata = LTER1_Pnet %>% select(PP,flow_log, PAR, tempc, cover)) %>%
  bind_cols(LTER1_Pnet %>% select(PP,flow_log, PAR, tempc, cover, Year, Season))

# plot the relationship between cover and Respiration with the model fits
Resp_predictions<-LTER1_Pnet %>% 
  select(PP,flow_log, PAR, tempc, cover, Year, Season) %>%
  add_epred_draws(fit1_d) %>%
  ungroup() %>%
  filter(.epred<0) %>%
  group_by(Year, Season, cover) %>%
  median_qi(.epred, PP, tempc) 

Resp_predictions %>%
  arrange(cover)%>%
  ggplot(aes(x = cover, y = .epred))+
  #geom_point()+
  geom_point()+
  geom_smooth(method = "lm")

Resp_predictions %>%
  ggplot(aes(x = tempc, y = .epred))+
  #geom_point()+
  geom_point()+
  geom_smooth(method = "lm")

P_predictions<-LTER1_Pnet %>% 
  select(PP,flow_log, PAR, tempc, cover, Year, Season) %>%
  add_epred_draws(fit1_d) %>%
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


# calculate a psuedo-R2 (currently 0.784)
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
  annotate("text", x = -25, y = 150, label = expression(paste("R"^2,"=0.78")))+
  xlim(-100, 200)+
  ylim(-100,200)+
  theme_classic()

# posterior predictive checks
PP_plot<-pp_check(fit1_d)+
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
