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
  mutate(UP_Oxy = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (UP_Oxy/32)*1000, UP_Oxy),
         DN_Oxy = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (DN_Oxy/32)*1000, DN_Oxy),
         PP = ifelse(DateTime< ymd_hms("2014-03-01 00:00:00"), (PP/32)*1000, PP))%>%
  mutate(Date = as_date(DateTime),
         DielDateTime = DateTime+hours(12), # all sampling started at noon such that midnight was the middle of a "day". Use this to extract the "daily" R to calculating GP
         DielDate = as_date(DielDateTime),
         Year = year(DateTime)) %>%
  filter(!Date %in% mdy("5/27/2011","5/28/2011")) %>%  # the respiration rate is incorrect these days from instrument failure
  filter(Season != "Summer" | Year != "2007") %>%
  filter(!DielDate %in% mdy("1/22/2020", "1/18/2017","5/28/2009","3/2/2022")) # these are extreme outliers

# calculate hourly GP and R data 
Daily_R <-All_PP_data %>%
  filter(PAR==0)%>% # pull out all the night data
  group_by(Year, Season, DielDate) %>% # get the average nighttime respiration by day to add to NEP to calcualte GP
  summarise(R_average = mean(PP, na.rm = TRUE))

#Calculate GP
All_PP_data<-All_PP_data %>%
  left_join(Daily_R) %>%
  mutate(GP = PP - R_average) %>%
  mutate(GP = ifelse(PAR == 0, NA, GP),# remove GP from any of the night data 
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
         R = ifelse(PAR == 0, PP, NA) # only include night data for R
  ) %>% 
  group_by(Year, Season) %>%
  summarise(NP_mean = mean(NP, na.rm = TRUE),
            NP_SE = sd(NP, na.rm = TRUE)/sqrt(n()),
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
            daily_NP = mean(PP, na.rm = TRUE)) %>%
  mutate(P_R = abs(daily_P/daily_R))%>% # average P to average R
  left_join(TotalLiving %>%
              filter(Site == "LTER 1"))

daily_data %>%
  filter(P_R<4)%>%
  ggplot(aes(x = daily_temp, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")
  #geom_smooth(method = 'nls', formula = "y ~ a*x^b", start = list(a=1,b=1),se=FALSE)

daily_data %>%
  filter(P_R<4)%>% ## need to find the GP/R values that make no sense
  ggplot(aes(x =daily_flow, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.01)) # 2 decimal places
  
daily_data %>%
  filter(P_R<4)%>% ## need to find the GP/R values that make no sense
  ggplot(aes(x =daily_flow, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.01)) # 2 decimal places

daily_data %>%
  filter(P_R<4)%>% ## need to find the GP/R values that make no sense
  ggplot(aes(x =mean_alive, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  #scale_x_continuous(trans = "log",
  #                   labels = label_number(accuracy = 0.01)) + # 2 decimal places
  facet_wrap(~Season)


daily_data %>%
  filter(P_R<4)%>% ## need to find the GP/R values that make no sense
  ggplot(aes(x =daily_PAR, y = P_R))+
  geom_point(aes(color = daily_flow))+
  geom_smooth(method = "lm")+
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.01)) 

#facet_wrap(~Season)

#standardized 
std.data<-daily_data %>%
  filter(P_R<5) %>%
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
  geom_smooth(method = "lm")
  facet_wrap(~Season)

Seasonal_Averages %>%
  ggplot(aes(x = Year, y = R_mean ))+
  geom_errorbar(aes(ymin = R_mean - R_SE, ymax = R_mean+R_SE))+
  geom_point()+
  geom_smooth(method = "lm")

Seasonal_Averages %>%
  #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
  ggplot(aes(x = Year, y = NP_mean,color = Season ))+
  geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_point()+
  facet_wrap(~Season)
  


Seasonal_Averages %>%
  #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
  ggplot(aes(x = Year, y = abs(GP_mean/R_mean),color = mean_alive ))+
  #geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
  geom_point()+
  #geom_smooth()+
  facet_wrap(~Season, scales = "free")

  NP_model<-brm(
    bf(NP_mean~Year*Season, nu = 3), data = Seasonal_Averages, 
                family = "student", control = list(max_treedepth = 14))


  Seasonal_Averages %>%
    filter(!Year %in% c("2021", "2007"))%>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = NP_mean,color = Flow_mean ))+
    geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
    geom_point()+
    geom_smooth(method = "lm")+
    facet_wrap(~Season, scales = "free")
  
  
  Seasonal_Averages %>%
    filter(!Year %in% c("2021", "2007"))%>%
    #filter(Season != "Summer" | !Year %in% c(2009, 2010, 2015) )%>%
    ggplot(aes(x = Year, y = PAR_mean))+
    #geom_errorbar(aes(ymin = NP_mean - NP_SE, ymax = NP_mean+NP_SE))+
    geom_point()+
    geom_smooth(method = "lm")+
    facet_wrap(~Season, scales = "free")
  
  All_PP_data %>%
  ggplot(aes(x = PAR, y = PP))+
  geom_point()

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
  geom_point()

All_PP_data %>%
  filter(PAR > 0)%>%
  ggplot(aes(x = PAR, y = PP, group = Date, color = Date))+
  geom_point()+
 # geom_smooth(method = "lm")+
  facet_wrap(~Date)
  
  All_PP_data %>%
    filter(PAR>0)%>%
    ggplot(aes(x = mean_solar_rad_kwpm2, y = PAR))+
    geom_point()
  
  All_PP_data %>%
    filter(PAR>0)%>%
    group_by(Year, Season)%>%
    summarise(mean_solar = mean(mean_solar_rad_kwpm2, na.rm = TRUE),
              mean_PAR = mean(PAR, na.rm = TRUE))%>%
    ggplot(aes(y = mean_PAR, x = Year))+
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
    geom_smooth()+
    geom_hline(yintercept = 0)+
    facet_wrap(~Date)
  
  
  All_PP_data<-All_PP_data %>%
    left_join(TotalLiving %>%
                filter(Site == "LTER 1"))

  LTER1_Pnet <-All_PP_data %>%
    mutate(tempc = Temperature_mean,
           Flowmean = Flow_mean,
          # daily_R = -daily_R
           )%>%
    mutate(PARcenter = as.numeric(scale(PAR, center = TRUE, scale = FALSE)),
           Covercenter = as.numeric(scale(mean_alive, center = TRUE, scale = FALSE)),
           Tempcenter = as.numeric(scale(Temperature_mean, center = TRUE, scale = FALSE)),
           Flowcenter = as.numeric(scale(Flow_mean, center = TRUE, scale = FALSE)))
  
  
  fit1<-brm(
    bf(PP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
        nl = TRUE, alpha~1,Pmax~Tempcenter +Flowmean + Covercenter,
       Rd~Tempcenter+Covercenter+ Flowcenter)+ student(),
    data = LTER1_Pnet,
    set_rescor(FALSE),
   # prior = c(
   #   prior(normal(1,100), nlpar = "alpha", lb = 0), 
   #   prior(normal(400,100), nlpar = "Pmax", lb = 0),
    #  prior(normal(1000, 500), nlpar = "Rd", lb = 0)
   # ), 
  control = list(adapt_delta = 0.99, max_treedepth = 20), 
  #  init = my_inits,
    cores = 4, 
    chains = 3, seed = 11, iter = 8000, warmup = 2000
    #silent = TRUE
  ) 
  
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
  
  conditional_effects(fit1)
  
  h <- data.frame(PAR=rep(seq(0,2000, length.out = 50),5), 
                  Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                  Tempcenter = rep(-1,250),
                  Flowmean = rep(0.1,250)) %>%
    bind_rows(data.frame(PAR=rep(seq(0,2000, length.out = 50),5), 
                         Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                         Tempcenter = rep(0,250),
                         Flowmean = rep(0.2,250)))%>%
    bind_rows(data.frame(PAR=rep(seq(0,2000, length.out = 50),5), 
                         Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                         Tempcenter = rep(1,250),
                         Flowmean = rep(0.3,250)))%>%
    bind_rows(data.frame(PAR=rep(seq(0,2000, length.out = 50),5), 
                         Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                         Tempcenter = rep(1,250),
                         Flowmean = rep(0.4,250)))
  
  pred<-predict(fit1, newdata = h, summary = TRUE, allow_new_levels = TRUE) %>%
    bind_cols(h) %>%
    mutate(Covercenter = as.factor(Covercenter),
           Tempcenter = as.factor(Tempcenter),
           Flowmean = as.factor(Flowmean))
  
  ggplot(pred, aes(x= PAR, y  =Estimate))+
    scale_color_manual(values = c("blue","pink","red"))+
    geom_point(data = LTER1_Pnet, aes(x = PAR, 
                                      y = PP, 
                                      size = mean_alive,
                                     # fill = Flow_mean
                                      ), shape = 21, alpha = 0.1)+
    geom_line(aes(color = Tempcenter, lty = Covercenter, linewidth = Flowmean))+
    scale_linewidth_discrete(range = c(0.1,1))+
   # scale_alpha(range =c(0.5,1))+
    labs(y = "NEP")
  
  
  # 10% change in PP at ~ average PAR and in the dark
  Predict_10 <-tibble(PAR = c(730,0,730,0,730,0,730,0,730,0,730,0), 
                         Tempcenter = c(0,0,0,0,0,0,1.1,1.1,0,0,0,0), 
                         Flowmean = c(rep(mean(LTER1_Pnet$Flowmean, na.rm = TRUE),8),
                                      c(mean(LTER1_Pnet$Flowmean, na.rm = TRUE),mean(LTER1_Pnet$Flowmean, na.rm = TRUE),
                                        mean(LTER1_Pnet$Flowmean, na.rm = TRUE)*1.1, mean(LTER1_Pnet$Flowmean, na.rm = TRUE)*1.1)),
                         Covercenter = c(0,0,10,10,0,0,0,0,0,0,0,0),
                       model = c("Cover","Cover","Cover","Cover", "Temp","Temp","Temp","Temp", "Flow","Flow","Flow","Flow"),
                      change = c("Average","Average","Change","Change","Average","Average","Change","Change","Average","Average","Change","Change"))
  
 Percent_10<- fitted(fit1, newdata = Predict_10 , summary = TRUE, allow_new_levels = TRUE)%>%
    bind_cols(Predict_10) %>%
    mutate(P_R = ifelse(PAR == 0, "Respiration", "Net Production")) %>%
    group_by(P_R, model)%>%
    reframe(Percent_change = 100*(Estimate[change == "Average"]- Estimate[change == "Change"])/Estimate[change == "Average"])
    
  
 Percent_10 %>%
 ggplot(aes(x = model, y = Percent_change))+
   geom_col()+
   labs(y = "Percent Change in PP",
        x = "10% increase in each parameter")+
   facet_wrap(~P_R)+
   theme_bw()
  ggsave(here("Output","PercentChange.png"))  
 
 fitted(fit1, newdata = Predict_10 , summary = TRUE, allow_new_levels = TRUE)%>%
    bind_cols(Predict_10) %>%
    mutate(P_R = ifelse(PAR == 0, "Respiration", "Net Production"))%>%
  ggplot(aes(x = Covercenter, y = Estimate))+
    geom_point()+
    geom_errorbar(aes(ymin = Estimate-Est.Error, ymax= Estimate+Est.Error), width = 0.1)+
    facet_wrap(~P_R, scale = "free")
  
  
  mean(All_PP_data$PAR[All_PP_data$PAR>0])
  ## predictions 
  
  Newdata2<- tibble(PAR = mean(LTER1_Pnet$PAR, na.rm = TRUE)-seq(-1,1,length.out = 11)*mean(LTER1_Pnet$PAR, na.rm = TRUE),
                    Tempcenter = seq(-1,1,length.out = 11)*(max(LTER1_Pnet$Temperature_mean, na.rm = TRUE)-
                                                              min(LTER1_Pnet$Temperature_mean, na.rm = TRUE)))
  # expand out
  Newdata2<-Newdata2 %>%
    mutate(PAR_per = seq(75,-75,length.out = 11),
           Temp_per = seq(-75,75,length.out = 11)) %>%
    expand.grid() %>%
     mutate(Covercenter = 0,
           Flowmean = 0)
  
  post<-posterior_predict(fit1,newdata = Newdata2)%>%
    as_tibble()
  
  post2<-post %>%
    colMeans()
  
  New_update<-Newdata2 %>%
    mutate(Estimate = as.numeric(post2),
           percent_change = 100*(Estimate - 20)/20,
           par_pro = 100*(PAR/mean(LTER1_Pnet$PAR, na.rm = TRUE)-1 ),
           temp_pro = 100*(Tempcenter/(max(LTER1_Pnet$Temperature_mean, na.rm = TRUE)-
                                         min(LTER1_Pnet$Temperature_mean, na.rm = TRUE)))-1 )
  
  
  New_update%>%
    #  filter(par_pro > -50 & par_pro < 50)%>%
    #  filter(temp_pro > -50 & temp_pro < 50) %>%
    ggplot(aes(x = par_pro, 
               y = temp_pro, 
               fill = percent_change))+
    geom_tile()+
    scale_fill_gradient2(limits = c(-100,100), low = "red", high = "blue", midpoint = 0, mid = "white") +
    labs(x = "PAR (% change from mean)",
         y = "Temperature (% change from mean)",
         fill = "% change in PP")+
    theme_classic()
  
  
  ggplot(daily_data %>% filter (Year>2007), aes(x = Year, y = daily_NP))+ 
    geom_point()+
    geom_label(aes(label = DielDate))
    geom_smooth(method = "lm", formula = "y~poly(x,2)")
  