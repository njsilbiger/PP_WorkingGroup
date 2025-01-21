###


### TPC model

#k <- 8.62e-05
#boltzmann.term <- r_tref * exp(e/k * (1/(tref+273.15 ) - 1/(temp + 273.15)))
#inactivation.term <- 1/(1 + exp(eh/k * (1/(th + 273.15) - 1/(temp + 273.15))))
#Rate ~ boltzmann.term*inactivation.term

## PI Model
#NEP = ((alpha*P_max*PAR)/(alpha*PAR + P_max))-Rd


## NEC Model
# all environmental data are centered so that we are fitting all the intercepts at the mean
# LEvel 1 TPC where rate at constant temp varies with light
#tref<-27
#k <- 8.62e-05
#NEC ~ boltzmann.term*inactivation.term

#boltzmann.term <- r_tref[f(Light,Cover)] * exp(e/k * (1/(tref+273.15 ) - 1/(temp + 273.15)))
#inactivation.term <- 1/(1 + exp(eh/k * (1/(th + 273.15) - 1/(temp + 273.15))))

# Level 2
# rate at constant temp varies with light, but the intercept is dependent on the % cover of calcifiers
#r_tref ~ B_par*PAR + B_0PAR[f(cover)] 

# Level 3
# Average baseline calcification rate on the reef is dependent on coral cover
#B_0PAR~ B_cover*Cover + B_0Cover


##### Attempt the Bayesian code

options("scipen"=100,digits=12) # stan doesnt like scientific notation. This fixes that

### PNEt with light
LTER1_Pnet <- LTER1 %>%
  mutate(tempc = Temp_mean,
         daily_R = -daily_R)%>%
  mutate(PARcenter = as.numeric(scale(TotalPAR_mean, center = TRUE, scale = FALSE)),
         Covercenter = as.numeric(scale(mean_alive, center = TRUE, scale = FALSE)),
         Tempcenter = as.numeric(scale(Temp_mean, center = TRUE, scale = FALSE)),
         Flowcenter = as.numeric(scale(Flow_mean, center = TRUE, scale = FALSE)))%>%
  rename(dailyNPP = daily_NPP,
         Rd = daily_R,
         PAR = TotalPAR_mean) 
 # select(dailyNPP, tempc, Tempcenter,PARcenter, Flowcenter,
#         Covercenter,  PAR = TotalPAR_mean, Rd = daily_R) %>%
  #drop_na()
 

LTER1_Pnet %>%
  ggplot(aes(x = PAR, y = dailyNPP))+
  geom_point()

# Make Rd an NP data point with no light... which is how it is in a PI curve
LTER1_Pnet2<-LTER1_Pnet %>%
  select(dailyNPP = Rd, Covercenter, tempc) %>%
  mutate(PAR = 0) %>%
  bind_rows(LTER1_Pnet %>%
              select(!Rd))

LTER1_Pnet2 %>%
  ggplot(aes(x = PAR, y = dailyNPP))+
  geom_point()

###### Maybe use cover residuals to decorrelate from light?????

fit1<-brm(
  bf(dailyNPP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
     nl = TRUE, alpha~1,Pmax~Tempcenter,
     Rd~Tempcenter+Covercenter)+ student(),
   data = LTER1_Pnet,
  set_rescor(FALSE),
  prior = c(
    prior(normal(1,100), nlpar = "alpha", lb = 0), 
    prior(normal(400,100), nlpar = "Pmax", lb = 0),
    prior(normal(1000, 500), nlpar = "Rd", lb = 0)
  ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
  init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 8000, warmup = 2000
  #silent = TRUE
) 

pp_check(fit1, resp = "dailyNPP", ndraws = 20)
pp_check(fit1, resp = "Rd", ndraws = 20)
#pp_check(fit1, resp = "Pmax", ndraws = 20)
summary(fit1)

#conditions <- data.frame(unique(LTER1_Pnet$Covercenter) ) 
#rownames(conditions) <- unique(LTER1_Pnet$Covercenter) 
#me_cover <- conditional_effects(fit1, 
#                               conditions = conditions, 
#                               re_formula = NULL, method = "predict") 

#plot(me_cover, ncol = 5, points = TRUE)

h <- data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                Tempcenter = rep(-1,250),
                Flowcenter = rep(0,250)) %>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(0,250),
                       Flowcenter = rep(0,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(0,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       Covercenter = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(2,250)))

pred<-predict(fit1, newdata = h, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(h) %>%
  mutate(Covercenter = as.factor(Covercenter),
         Tempcenter = as.factor(Tempcenter))

ggplot(pred, aes(x= PAR, y  =Estimate))+
  geom_line(aes(color = Tempcenter, lty = Covercenter))+
  scale_color_manual(values = c("blue","pink","red"))+
  
  geom_point(data = LTER1_Pnet, aes(x = PAR, 
                                    y = dailyNPP, 
                                    size = mean_alive,
                                    fill = Temp_mean), shape = 21)+
  labs(y = "NEP")


## how much do things change with a 10% change in cover, temp, and light

# This is NEC at average light, temperature, and producer cover
newdata<-tibble(PAR = c(mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE)-0.2*mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE)), 
                Tempcenter = c(0,0,0.2*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                        min(LTER1_Pnet$Temp_mean, na.rm = TRUE)),0), 
                Covercenter = c(0,0,0,-20),
                State = c("Mean all","Light decrease", "Temp increase", "Cover decrease"))

pred<-predict(fit1, newdata = newdata, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(newdata)

ggplot(pred, aes(x = State, y = Estimate))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = Estimate - Est.Error, ymax = Estimate+Est.Error))



## Same above but with GP instead of NEP ###########

### PNEt with light
LTER1_Pg <- LTER1 %>%
  mutate(tempc = Temp_mean,
         daily_R = -daily_R)%>%
  mutate(PARcenter = as.numeric(scale(TotalPAR_mean, center = TRUE, scale = FALSE)),
         Covercenter = as.numeric(scale(mean_alive, center = TRUE, scale = FALSE)),
         Tempcenter = as.numeric(scale(Temp_mean, center = TRUE, scale = FALSE)),
         Flowcenter = as.numeric(scale(Flow_mean, center = TRUE, scale = FALSE)))%>%
  rename(dailyNPP = daily_NPP,
         Rd = daily_R,
         PAR = TotalPAR_mean,
         dailyGPP = daily_GPP,
         Flow  = Flow_mean) %>%
  drop_na(PAR)

# plot relationship between cover and PAR to get residuals for model to remove effect of light on cover
mo<-lm(data = LTER1_Pg, Covercenter~PAR)

LTER1_Pg<-LTER1_Pg %>%
  bind_cols(augment(mo) %>%
  select(resid = .resid)) # cover residuals

# run the Bayesian model
# https://link.springer.com/article/10.1007/s00227-006-0465-3 
# In situ measurements of flow effects on primary production and
#dark respiration in reef corals LO 1991

# Flow power function P = aX^b
fit1<-brm(
  bf(dailyGPP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
     nl = TRUE, alpha~1,
     Pmax~Flowcenter,
     Rd~Tempcenter*resid)+ student(), # Rd changes by both temp and cover interactively
  data = LTER1_Pg,
  set_rescor(FALSE),
  prior = c(
    prior(normal(200,200), nlpar = "alpha", lb = 0), 
    #prior(normal(1,10), nlpar = "aFlow", lb = 0),
    #prior(uniform(0,1), nlpar = "bFlow", lb = 0),
    #prior(normal(0,10), nlpar = "Flowcenter"),
    prior(normal(3000,100), nlpar = "Pmax", lb = 0),
    prior(normal(1000, 500), nlpar = "Rd", lb = 0)
  ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
#  init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 20000
  #silent = TRUE
) 


pp_check(fit1, resp = "Rd", ndraws = 20)
pp_check(fit1, resp = "dailyGPP", ndraws = 20)
#pp_check(fit1, resp = "Pmax", ndraws = 20)
summary(fit1)

h <- data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                Tempcenter = rep(-1,250),
                Flowcenter = rep(0,250),
                Flow = rep(0.1,250)) %>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(0,250),
                       Flowcenter = rep(0,250),
                       Flow = rep(0.2,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(0,250),
                       Flow = rep(0.3,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(2,250),
                       Flow = rep(0,250)))

pred<-predict(fit1, newdata = h, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(h) %>%
  mutate(resid = as.factor(resid),
         Tempcenter = as.factor(Tempcenter),
         Flowcenter = as.factor(Flowcenter))

ggplot(pred %>% filter(resid == 0),
       aes(x= PAR, y  =Estimate))+
  geom_line(aes(color = Tempcenter,
                lty = Flowcenter,
                #alpha = resid
                ))+
  scale_color_manual(values = c("blue","pink","red"))+
  geom_point(data = LTER1_Pg, aes(x = PAR, 
                                    y = dailyGPP, 
                                  fill = Flow,
                                 #   size = mean_alive,
                                  #  fill = Temp_mean
                                  ), shape = 21)+
  labs(y = "GPP",
       linetype = "Flow rate (Center)",
       color = "Temperature (Center)")+
  theme_bw()

ggsave(here("Output","GPP_PI.png"), width = 6, height = 6)

# MOdel just the mean

pred<-predict(fit1, newdata = data.frame(PAR=seq(0,50, length.out = 50),
                                         Flowcenter = 0,
                                         resid = 0,
                                         Tempcenter =0),
                                         summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(data.frame(PAR=seq(0,50, length.out = 50)), 
                       resid = 0,
                       Tempcenter = 0)

ggplot(pred, aes(x= PAR, y  =Estimate))+
  geom_ribbon(aes(ymin = Estimate - Est.Error, ymax = Estimate+Est.Error),
              alpha = 0.5, fill = "lightgrey")+
  geom_line(linewidth = 1.2)+
  geom_point(data = LTER1_Pg, aes(x = PAR, 
                                  y = dailyGPP, 
                                  color = Temp_mean,
                                  size = mean_alive
                                  #   size = mean_alive,
                                  #  fill = Temp_mean
  ))+
   labs(y = "GPP",
        color = "Temperature",
        size = "% cover of macro producers")+
  theme_bw()

ggsave(here("Output","GPP_PI_mean.png"), width = 6, height = 6)


# This is NEC at average light, temperature, and producer cover
newdata<-tibble(PAR = c(mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE)-0.2*mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE),
                        mean(LTER1_Pnet$PAR, na.rm = TRUE)), 
                Tempcenter = c(0,0,
                               0.2*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                          min(LTER1_Pnet$Temp_mean, na.rm = TRUE)),
                               0, 0), 
                resid = c(0,0,0,-20,0),
                Flowcenter = c(0,0,0,0,0.2*(max(LTER1_Pnet$Flow_mean, na.rm = TRUE)-
                                              min(LTER1_Pnet$Flow_mean, na.rm = TRUE))),
                State = c("Mean all","Light decrease", 
                          "Temp increase", "Cover decrease", "Flow increase"))

pred<-predict(fit1, newdata = newdata, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(newdata)

ggplot(pred, aes(x = State, y = Estimate))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5))


  post<-posterior_predict(fit1,newdata = newdata)%>%
    as_tibble()
    
 colnames(post)<-pred$State   
 
 GPPchange<-post %>%
   pivot_longer(cols = `Mean all`:`Flow increase`) %>%
   group_by(name)%>%
   summarise(mean = mean(value),
             se = mean(value)/sqrt(n()),
             lwr = mean(value, na.rm = TRUE) - (1.96 * sd(value, na.rm = TRUE)/sqrt(n())),
             upr = mean(value, na.rm = TRUE) + (1.96 * sd(value, na.rm = TRUE)/sqrt(n()))) %>%
   mutate(name = factor(name, levels = c("Mean all", "Light decrease",
                                         "Temp increase", "Cover decrease", "Flow increase")))
 GPPchange%>%
   ggplot( aes(x = name, y = mean))+
   geom_point(size = 4)+
   geom_errorbar(aes(ymin = lwr, ymax = upr))+
   labs(y = "GPP")+
   theme_bw()
   
 # plot the percent change in GPP for a 10% change in light, Temp, and cover
 GPPchange %>%
   mutate(mean_change = 100*(mean - mean[name == "Mean all"])/mean[name == "Mean all"]) %>%
   filter(name != "Mean all")%>%
   ggplot(aes(x = name, y = mean_change)) +
   geom_col()+
   labs(y = "% Change in GP from 10% change in value",
        x = "")+
   theme_bw()
 
 
 
# newdata2<-tibble(PAR = c(mean(LTER1_Pnet$PAR, na.rm = TRUE),
#                         mean(LTER1_Pnet$PAR, na.rm = TRUE)-0.2*mean(LTER1_Pnet$PAR, na.rm = TRUE),
#                         mean(LTER1_Pnet$PAR, na.rm = TRUE)), 
#                 Tempcenter = c(0,0,+0.2*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
#                                            min(LTER1_Pnet$Temp_mean, na.rm = TRUE))), 
#                 resid = c(0,0,0),
#                 State = c("Mean all","Light decrease", "Temp increase"))
 
 
Newdata2<- tibble(PAR = mean(LTER1_Pnet$PAR, na.rm = TRUE)-seq(-1,1,length.out = 11)*mean(LTER1_Pnet$PAR, na.rm = TRUE),
           Tempcenter = seq(-1,1,length.out = 11)*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                                    min(LTER1_Pnet$Temp_mean, na.rm = TRUE)))
# expand out
Newdata2<-Newdata2 %>%
  mutate(PAR_per = seq(100,-100,length.out = 11),
         Temp_per = seq(-100,100,length.out = 11)) %>%
   expand.grid() %>%
  mutate(resid = 0,
         Flowcenter = 0)
 
post<-posterior_predict(fit1,newdata = Newdata2)%>%
  as_tibble()

post2<-post %>%
  colMeans()

New_update<-Newdata2 %>%
  mutate(Estimate = as.numeric(post2),
         percent_change = 100*(Estimate - mean(LTER1_Pg$dailyGPP,na.rm = TRUE))/mean(LTER1_Pg$dailyGPP,na.rm = TRUE),
         par_pro = 100*(PAR/mean(LTER1_Pnet$PAR, na.rm = TRUE)-1 ),
         temp_pro = 100*(Tempcenter/(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                           min(LTER1_Pnet$Temp_mean, na.rm = TRUE)))-1 )


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
       fill = "% change in GPP")+
  theme_classic()
ggsave(here("Output","GPP_predictions_percent.png"), width = 6, height = 6) 

New_update%>%
  #  filter(par_pro > -50 & par_pro < 50)%>%
  #  filter(temp_pro > -50 & temp_pro < 50) %>%
  ggplot(aes(x = PAR, 
             y = Tempcenter+mean(LTER1_Pnet$Temp_mean, na.rm = TRUE), 
             fill = Estimate))+
  geom_tile()+
  scale_fill_gradient2(limits = c(0,2500), low = "red", high = "blue", midpoint = 0, mid = "white") +
  labs(x = "PAR",
       y = "Temperature",
       fill = "GPP mmol m-2 d-1")+
  theme_classic()
 
ggsave(here("Output","GPP_predictions_mmol.png"), width = 6, height = 6) 
  

############
