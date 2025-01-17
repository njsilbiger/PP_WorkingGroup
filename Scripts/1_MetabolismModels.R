## Bayesian analyses of change in metabolism over time and as a function of environment ###

## By Nyssa Silbiger ###
## Updated on 2025-01-16 ####


# sorce the cleaned dataframes
source(here("Scripts","0_DataCleaning.R" ))

###### Bayesian Analysis for metabolism #######
#set.seed(151)

# compare with and without the random intercept
GPPMod<-brm(daily_GPP~Year + (1|Month), data = LTER1, 
            control = list(adapt_delta = 0.92), 
            iter = 10000, cores = 2,seed = 31)

summary(GPPMod)
GPP_coeff<-summary(GPPMod)$fixed # pullout the fixed effects
slop_GPP<-round(GPP_coeff$Estimate[2],2) #slope
SE_GPP<-round(GPP_coeff$Est.Error[2],2) #error
GPP_0_Year <- round(-GPP_coeff$Estimate[1]/GPP_coeff$Estimate[2])
ranef(GPPMod) # random effects

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
  # annotate("text", x = 2020, y = 2000, label = paste("GP = 0 in Year",GPP_0_Year))+
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
  mutate(variable = ifelse(x > -82 & x < -28.31, "On","Off")) # -82.17    -28.31

# Figure showing prosterior for GPP
GPP_dens<-ggplot(plotdata_GPP, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7, linewidth = 1.3)+
  # geom_area(data = filter(plotdata_GPP, variable == 'Off'), fill = 'grey', alpha = 0.5) + 
  geom_area(data = filter(plotdata_GPP, variable == 'On'), fill =  "skyblue", alpha = 0.5) +
  xlim(-100,50)+
  geom_line(data = tibble(x = -55.29, y = c(0,0.0285)), aes(x=x, y=y), color = "skyblue", linewidth = 2)+
  geom_line(linewidth = 1.3) +
  # scale_fill_manual(values = c("gray80", "skyblue"))+
  #  annotate("text", x = 27.5, y = 0.015, label = "P(1) \n GP is declining over time")+
  #  annotate("text", x = slop_GPP, y = 0.03, label = paste(slop_GPP,"\u00B1",SE_GPP))+
  labs(x = "Change in GP per year",
       y = "Density")+
  theme_bw()

GPPplot<-GPP_pred+GPP_dens&theme(axis.text = element_text(size = 16),
                                 axis.title = element_text(size = 18),
                                 panel.grid.minor = element_blank())

# Respiration model
RMod<-brm(-daily_R~Year + (1|Month), data = LTER1, 
          control = list(adapt_delta = 0.93), cores = 2, 
          iter = 3000, seed = 112)
summary(RMod)

R_coeff<-summary(RMod)$fixed # pullout the fixed effects
slop_R<-round(R_coeff$Estimate[2],2) #slope
SE_R<-round(R_coeff$Est.Error[2],2) #error
ranef(RMod)

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
  #  annotate("text", x = 2020, y = 1300, label = paste("R = 0 in Year",R_0_Year))+
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
  mutate(variable = ifelse(x > -58 & x < -14, "On","Off"))

# probability of 1 that NEP is declining over time

# Figure showing prosterior for R being negative
R_dens<-ggplot(plotdata_R, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7, linewidth = 1.3)+
  # geom_vline(xintercept = slop_NPP, lty = 2, alpha = 0.7)+
  geom_area(data = filter(plotdata_R, variable == 'On'), fill =  "skyblue", alpha = 0.5) +
  #  geom_area(data = filter(plotdata_R, variable == 'On'), fill = 'grey') +
  geom_line(data = tibble(x = -36.4, y = c(0,0.0355)), aes(x=x, y=y), color = "skyblue", linewidth = 2)+ 
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  #  annotate("text", x = 27.5, y = 0.025, label = "P(0.998) \n R is declining over time")+
  #  annotate("text", x = slop_R, y = 0.04, label = paste(slop_R,"\u00B1",SE_R))+
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
data_NP<-conditional_effects(NPMod, "Year",re_formula = NULL)$Year

# plot the prediction from the bayesian model


NPP_pred<-ggplot()+
  geom_line(data = data_NP, aes(x = effect1__, y = estimate__), size = 1.2)+
  geom_ribbon(data = data_NP, aes(x = effect1__, ymin = lower__, ymax = upper__),
              fill = "#A1AEB1", alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = daily_NPP, color = Month), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  labs(x = "Year",
       y =bquote(atop("Net ecosystem production",
                      "(mmol" ~ O[2]~m^-2~d^-1~")")))+
  theme_bw()+
  theme(legend.position = "none")

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
  mutate(variable = ifelse(x > -26.16 & x < 2.95, "On","Off"))

# Figure showing prosterior for NPP being negative
NPP_dens<-ggplot(plotdata, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7, size = 1.5)+
  geom_area(data = filter(plotdata, variable == 'On'), fill =  "skyblue", alpha = 0.5) +
  #  geom_area(data = filter(plotdata_R, variable == 'On'), fill = 'grey') +
  geom_line(data = tibble(x = -11.65, y = c(0,0.052)), aes(x=x, y=y), color = "skyblue", linewidth = 2)+
  geom_line(linewidth = 1.3) +
  # geom_vline(xintercept = slop_NPP, lty = 2, alpha = 0.7)+
  #geom_area(data = filter(plotdata, variable == 'Off'), fill = 'grey') + 
  #geom_area(data = filter(plotdata, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  xlim(-100,50)+
  #annotate("text", x = 27.5, y = 0.035, label = "P(0.934) \n NEP is declining over time")+
  #annotate("text", x = slop_NPP, y = 0.06, label = paste(slop_NPP,"\u00B1",SE_NPP))+
  labs(x = "Change in NEP per year",
       y = "Density")+
  theme_bw()

NPP_plot<-NPP_pred+NPP_dens&theme(axis.text = element_text(size = 16),
                                  axis.title = element_text(size = 18))

#### Calcification model
NECMod<-brm(daily_NEC~Year*Season, data =LTER1 , 
            control = list(adapt_delta = 0.92, max_treedepth = 20), iter = 3000)

summary(NECMod)
NEC_coeff<-summary(NECMod)$fixed # pullout the fixed effects

slop_NEC<-round(NEC_coeff$Estimate[2],2) #slope
slop_NEC_June<-round(NEC_coeff$Estimate[4],2) #slope

SE_NEC<-round(NEC_coeff$Est.Error[2],2) #error
SE_NEC_June<-round(NEC_coeff$Est.Error[4],2) #error

NEC_0_Year <- round(-NEC_coeff$Estimate[1]/NEC_coeff$Estimate[2]) # estimate for January

# NEC model
data_NEC<-conditional_effects(NECMod, "Year:Season",re_formula = NULL)$Year

NEC_pred<-ggplot()+
  geom_hline(yintercept = 0, lty = 2)+
  geom_line(data = data_NEC, aes(x = effect1__, y = estimate__, color = effect2__), size = 1.2, show.legend = FALSE)+
  geom_ribbon(data = data_NEC, aes(x = effect1__, ymin = lower__, ymax = upper__, fill = effect2__),
              alpha = 0.3)+
  geom_point(data = LTER1, aes(x = Year, y = daily_NEC, color = Season), size = 2)+
  scale_color_manual(values = c("#ffbe4f","#0ea7b5"))+
  scale_fill_manual(values = c("#ffbe4f","#0ea7b5"), guide = "none")+
  labs(color = "",
       fill = "",
       x = "Year",
       y =bquote(atop("Net Ecosystem Calcification",
                      "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
  #  annotate("text", x = 2020, y = 500, label = paste("NEC = 0 in Year",NEC_0_Year))+
  theme_bw()+
  theme(#legend.position = c(.15,.8),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

# First for January
draws_NEC<-NECMod %>%
  spread_draws(b_Year) %>%
  as_tibble() 

# now for June
draws_NEC_June<-NECMod %>%
  spread_draws(`b_Year:SeasonWinter`) %>%
  as_tibble() 

# calculate proportion < 0
draws_NEC %>%
  mutate(lessthan = ifelse(b_Year<0,1,0)) %>%
  count(lessthan) %>%
  reframe(prop = n[lessthan == 1]/sum(n))

# probability of 1 that NEC is declining over time
plotdata_NEC<-as_tibble(density(draws_NEC$b_Year)) %>%
  mutate(variable = ifelse(x > -39.88 & x < -11.61, "On","Off"))

plotdata_NEC_June<-as_tibble(density(draws_NEC_June$`b_Year:SeasonWinter`))%>%
  mutate(variable = ifelse(x > 13.26& x < 61.35, "On","Off"))

# Figure showing prosterior for R being negative
NEC_dens<-ggplot() + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7,
             linewidth = 1.3)+
  geom_area(data = filter(plotdata_NEC, variable == 'On'),aes(x, y), fill =  "#ffbe4f", alpha = 0.5) +
  geom_area(data = filter(plotdata_NEC_June, variable == "On"),aes(x, y), fill =  "#0ea7b5", alpha = 0.5) +
   geom_line(data = tibble(x = -25.62, y = c(0,0.054)), aes(x=x, y=y), color = "#ffbe4f", linewidth = 2)+
  geom_line(data = tibble(x = 36.82, y = c(0,0.033)), aes(x=x, y=y), color = "#0ea7b5", linewidth = 2)+
  geom_line(data = plotdata_NEC, aes(x, y), linewidth = 1.3) +
  geom_line(data = plotdata_NEC_June, aes(x = x, y = y), linewidth = 1.3) +
  xlim(-100,80)+
  # annotate("text", x = 27.5, y = 0.015, label = "P(0.99) \n NEC is declining over time")+
  #  annotate("text", x = slop_NEC, y = 0.06, label = paste(slop_NEC,"\u00B1",SE_NEC))+
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
  mutate(variable = ifelse(x > -24.87 & x < 5.21, "On","Off"))

# Figure showing prosterior for R being negative
NEC_dens_total<-ggplot(plotdata_NEC_total, aes(x, y)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7)+
  #  geom_area(data = filter(plotdata_NEC_total, variable == 'Off'), fill = 'grey') + 
  geom_area(data = filter(plotdata_NEC_total, variable == 'On'), fill =  "skyblue", alpha = 0.5) +
  # geom_area(data = filter(plotdata_NEC_total, variable == 'On'), fill = 'grey') +
  geom_line(linewidth = 1.3) +
  geom_line(data = tibble(x = -9.69, y = c(0,0.0505)), aes(x=x, y=y), color = "skyblue", linewidth = 2)+
  xlim(-100,50)+
  # annotate("text", x = 27.5, y = 0.015, label = "P(0.90) \n NEC is declining over time")+
  #  annotate("text", x = slop_NEC_total, y = 0.06, label = paste(slop_NEC_total,"\u00B1",SE_NEC))+
  labs(x = "Change in NEC per year",
       y = "Density")+
  theme_bw()

NECplot<-NEC_pred+NEC_dens&theme(axis.text = element_text(size = 16),
                                       axis.title = element_text(size = 18),
                                       panel.grid.minor = element_blank())

GPPplot/Respplot/NPP_plot/NECplot
ggsave(filename = "Output/BayesRegression2.pdf", width = 10, height = 14)

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
LTER1_NA<-LTER1 %>%
  drop_na(daily_GPP)

GPP_enviro_mod<-brm(daily_GPP~alive_scale+Temp_scale+Light_scale+Flow_scale +(1|Month) , data = LTER1, 
                    control = list(adapt_delta = 0.92), iter = 5000)


#plot(conditional_effects(GPP_enviro_mod, re_formula =NULL), points = TRUE)
# visualize the pairs to inspect for multicollinearity
GP_pairs<-pairs(GPP_enviro_mod)
ggsave(here("Output","GP_pairs.png"), plot = GP_pairs)

GPP_enviro_coef<-summary(GPP_enviro_mod)$fixed[2:5,]%>%
  mutate(Parameter = row.names(.))%>%
  as_tibble()


GPP_coef_plot<-GPP_enviro_mod %>%
  spread_draws(b_Light_scale, b_alive_scale, b_Temp_scale, b_Flow_scale) %>%
  rename(Light = b_Light_scale, Cover =b_alive_scale,
         Temperature = b_Temp_scale, Flow = b_Flow_scale)%>%
  pivot_longer(Light:Flow)%>%
  ggplot(aes(y = name, x = value,
             #    fill = after_stat(x < 0)
  ),
  fill = "gray80"
  ) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Gross Photosynthesis",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))


### Projection predictive feature selection

## proj pred https://mc-stan.org/projpred/articles/projpred.html
refm_obj<-get_refmodel(GPP_enviro_mod) # get the reference model

# Refit the reference model K times: -  we can use this to see how many times each variable shows up in the model
cv_fits <- run_cvfun(
  refm_obj,
  ### Only for the sake of speed (not recommended in general):
  K = 25 # run this for 25 cross-validation folds (leaves one out for 25 times, and sample size is 26)
  ###
)

# Final cv_varsel() run:
#cv_varsel() performs a cross-validation (CV) using the kfold fits above for variable selection.
cvvs_gpp <- cv_varsel(
  refm_obj,
  cv_method = "kfold",
  cvfits = cv_fits,
  nterms_max = 5,
  #parallel = TRUE,
  verbose = FALSE
  ### 
)
# plot the results
plot(cvvs_gpp, stats = "mlpd", deltas = TRUE) # mean log predictive density
msize<-suggest_size(cvvs_gpp, stat = "mlpd") # suggest model size
smmry <- summary(cvvs_gpp, stats = "mlpd", type = c("mean", "lower", "upper"),
                 deltas = TRUE)
print(smmry, digits = 1)
rk <- ranking(cvvs_gpp)
pr_rk <- cv_proportions(rk) 
plot(pr_rk)
( predictors_final <- head(rk[["fulldata"]], msize) )
plot(cv_proportions(rk, cumulate = TRUE))

# extract the proportions of models that each predictor is present in models <1 to <5 predictors
GPP_proportions <-cv_proportions(rk, cumulate = TRUE)[1:5,] %>%
  as_tibble() %>%
  mutate(No_variables = 1:5)

## Make a barplot showing which variables are present in which model
#GPP_proportions %>%
#  pivot_longer(cols = alive_scale:Flow_scale) %>%
#  ggplot(aes(x = No_variables, y = value, fill = name))+
#  geom_col()


# post-selection inference
prj <- project(
  refm_obj,
  predictor_terms = predictors_final,
  ### In interactive use, we recommend not to deactivate the verbose mode:
  verbose = FALSE
  ###
)
prj_mat <- as.matrix(prj)

# look at the posterios
prj_drws <- as_draws_matrix(prj_mat)
prj_smmry <- summarize_draws(
  prj_drws,
  "median", "mad", function(x) quantile(x, probs = c(0.025, 0.975))
)
# Coerce to a `data.frame` because pkgdown versions > 1.6.1 don't print the
# tibble correctly:
prj_smmry <- as.data.frame(prj_smmry)
print(prj_smmry, digits = 1)
# plot the posterior
bayesplot_theme_set(ggplot2::theme_bw())
mcmc_intervals(prj_mat) +
  ggplot2::coord_cartesian(xlim = c(0,1500))

# now with the 2-dimentional
refm_mat <- as.matrix(GPP_enviro_mod)
mcmc_intervals(refm_mat, pars = colnames(prj_mat)) +
  mcmc_intervals(prj_mat) +
  ggplot2::coord_cartesian(xlim = c(0,1500))


dat_gauss_new <- setNames(
  as.data.frame(replicate(length(predictors_final), c(-1, 0, 1))),
  predictors_final
) 

prj_predict <- proj_predict(prj)

#prj_linpred <- proj_linpred(prj, newdata = dat_gauss_new, integrated = TRUE)
#cbind(dat_gauss_new, linpred = as.vector(prj_linpred$pred))

# posterio predictive checks
ppc_dens_overlay(y = LTER1_NA$daily_GPP, yrep = prj_predict)# the last 3 values are cut off for some reason

# Respiration ############################
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
  ggplot(aes(y = name, x = value,
             #fill = after_stat(x < 0)
  ),
  fill = "gray80"
  ) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  #  scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Respiration",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))


## variable selection for respiration
## proj pred https://mc-stan.org/projpred/articles/projpred.html
LTER1<-LTER1 %>%
  mutate(neg_daily_R =-daily_R )

R_enviro_mod<-brm(neg_daily_R~alive_scale+Temp_scale+Flow_scale +(1|Month) , data = LTER1, 
                  control = list(adapt_delta = 0.92), iter = 5000)

refm_obj<-get_refmodel(R_enviro_mod) # get the reference model

# Refit the reference model K times: -  we can use this to see how many times each variable shows up in the model
cv_fits <- run_cvfun(
  refm_obj,
  K = 25 # run this for 25 cross-validation folds (leaves one out for 25 times, and sample size is 26)
  ###
)

# Final cv_varsel() run:
#cv_varsel() performs a cross-validation (CV) using the kfold fits above for variable selection.
cvvs_r <- cv_varsel(
  refm_obj,
  cv_method = "kfold",
  cvfits = cv_fits,
  nterms_max = 4,
  #parallel = TRUE,
  verbose = FALSE
  ### 
)

# plot the results
plot(cvvs_r, stats = "mlpd", deltas = TRUE) # mean log predictive density
msize<-suggest_size(cvvs_r, stat = "mlpd") # suggest model size
smmry <- summary(cvvs_r, stats = "mlpd", type = c("mean", "lower", "upper"),
                 deltas = TRUE)
print(smmry, digits = 1)
rk <- ranking(cvvs_r)
pr_rk <- cv_proportions(rk) 
plot(pr_rk)
( predictors_final <- head(rk[["fulldata"]], msize) )
plot(cv_proportions(rk, cumulate = TRUE))

# extract the proportions of models that each predictor is present in models <1 to <5 predictors
R_proportions <-cv_proportions(rk, cumulate = TRUE)[1:4,] %>%
  as_tibble() %>%
  mutate(No_variables = 1:4)


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
  ggplot(aes(y = name, x = value,
             #fill = after_stat(x < 0)
  ),
  fill = "gray80"
  ) +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_fill_manual(values = c("gray80", "skyblue"))+
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
  ggplot(aes(y = name, x = value,
             #fill = after_stat(x < 0)
  ),
  fill = "gray80") +
  stat_halfeye(.width = c(.90, .5))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_fill_manual(values = c("gray80", "skyblue"))+
  labs(x = "Net ecosystem calcification (daytime)",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16))

GPP_coef_plot|R_coef_plot|C_coef_plot
ggsave(here("Output","CoefPlot_enviro_metab.pdf"), width = 14, height = 5)

refm_obj<-get_refmodel(C_enviro_mod) # get the reference model

# Refit the reference model K times: -  we can use this to see how many times each variable shows up in the model
cv_fits <- run_cvfun(
  refm_obj,
  ### Only for the sake of speed (not recommended in general):
  K = 15 # run this for 15 cross-validation folds (sample size here is 18)
  ###
)

# Final cv_varsel() run:
#cv_varsel() performs a cross-validation (CV) using the kfold fits above for variable selection.
cvvs_c <- cv_varsel(
  refm_obj,
  cv_method = "kfold",
  cvfits = cv_fits,
  nterms_max = 15,
  #parallel = TRUE,
  verbose = FALSE
  ### 
)

# plot the results
plot(cvvs_c, stats = "mlpd", deltas = TRUE) # mean log predictive density
msize<-suggest_size(cvvs_c, stat = "mlpd") # suggest model size
smmry <- summary(cvvs_c, stats = "mlpd", type = c("mean", "lower", "upper"),
                 deltas = TRUE)
print(smmry, digits = 1)
rk <- ranking(cvvs_c)
pr_rk <- cv_proportions(rk) 
plot(pr_rk)
( predictors_final <- head(rk[["fulldata"]], msize) )
plot(cv_proportions(rk, cumulate = TRUE))

# extract the proportions of models that each predictor is present in models <1 to <5 predictors
C_proportions <-cv_proportions(rk, cumulate = TRUE)[1:9,] %>%
  as_tibble() %>%
  mutate(No_variables = 1:9)


## Bring together the proportions selected
Allproportions<-GPP_proportions %>%
  pivot_longer(alive_scale:`(1 | Month)`)%>%
  mutate(model = "Gross Photosynthesis") %>%
  bind_rows(
    R_proportions %>%
      pivot_longer(alive_scale:Flow_scale)%>%
      mutate(model = "Respiration")
  )%>%
  bind_rows(
    C_proportions %>%
      pivot_longer(Light_scale:`Month:Temp_scale`)%>%
      mutate(model = "Calcification")
  )


Allproportions %>%
  filter(No_variables == 1,
         value >0) %>% # just bring out top ranking variables
  mutate(Variable = case_when(name == "alive_scale" ~ "% cover of macroproducers",
                              name == "Flow_scale" ~ "Flow",
                              name == "Light_scale" ~ "Light",
                              name == "Temp_scale" ~ "Temperature",
                              name == "Month"~ "Season"))%>%
  mutate(model = factor(model, levels = c("Gross Photosynthesis", "Respiration", "Calcification")),
         Variable = factor(Variable, levels =  c("% cover of macroproducers","Light","Season","Flow","Temperature")
         ))%>%
  ggplot(aes(x = model, y = value*100, fill = Variable))+
  geom_col()+
  labs(x = "",
       y = " % present in cross-validated variable selection ",
       fill = "")+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top")

ggsave(here("Output","Var_Selection.pdf"), width = 7, height = 6)

# MAKE A PLOT COMPARING ESTIMATES OF CHANGE OVER TIME HERE TO OTHER DATASETS AROUND THE WORLD
