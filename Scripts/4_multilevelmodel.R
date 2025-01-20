###


### TPC model

k <- 8.62e-05
boltzmann.term <- r_tref * exp(e/k * (1/(tref+273.15 ) - 1/(temp + 273.15)))
inactivation.term <- 1/(1 + exp(eh/k * (1/(th + 273.15) - 1/(temp + 273.15))))

Rate ~ boltzmann.term*inactivation.term

## PI Model
NEP = ((alpha*P_max*PAR)/(alpha*PAR + P_max))-Rd


## NEC Model
# all environmental data are centered so that we are fitting all the intercepts at the mean
# LEvel 1 TPC where rate at constant temp varies with light
tref<-27
k <- 8.62e-05
NEC ~ boltzmann.term*inactivation.term

boltzmann.term <- r_tref[f(Light,Cover)] * exp(e/k * (1/(tref+273.15 ) - 1/(temp + 273.15)))
inactivation.term <- 1/(1 + exp(eh/k * (1/(th + 273.15) - 1/(temp + 273.15))))

# Level 2
# rate at constant temp varies with light, but the intercept is dependent on the % cover of calcifiers
r_tref ~ B_par*PAR + B_0PAR[f(cover)] 

# Level 3
# Average baseline calcification rate on the reef is dependent on coral cover
B_0PAR~ B_cover*Cover + B_0Cover


##### Attempt the Bayesian code

options("scipen"=100,digits=12) # stan doesnt like scientific notation. This fixes that

# fit the model
# create a generated quantities block for Topt and for posterior predictive checks
stanvars <- stanvar(scode = "real Topt;
  vector[N] y_new;
  vector[N] nlp_lnc = X_lnc * b_lnc;
  vector[N] nlp_Eh = X_Eh * b_Eh;
  vector[N] nlp_Th = X_Th * b_Th;
  vector[N] nlp_E = X_E * b_E;
Topt =  (b_Eh[1] * b_Th[1]) / (b_Eh[1] + (0.0000862 * b_Th[1] * log((b_Eh[1] / b_E[1]) - 1)));
for (n in 1:N) {  
nlp_lnc[n] += r_1_lnc_1[J_1[n]] * Z_1_lnc_1[n];
 y_new[n] = student_t_rng(nu, nlp_lnc[n] + log(exp(nlp_E[n] / 0.0000862 * (1 / 299.15 - 1 / C_1[n]))) + log(1 / (1 + exp(nlp_Eh[n] / 0.0000862 * (1 / nlp_Th[n] - 1 / C_1[n])))), sigma);};",
                    block = "genquant")

LTER1test<-LTER1 %>%
  mutate(temp = Temp_mean+273.15) %>% # convert to K
  mutate(tempc = Temp_mean)%>%
  mutate(PARcenter = as.numeric(scale(TotalPAR_mean, center = TRUE, scale = FALSE)),
         Covercenter = as.numeric(scale(total_Calc, center = TRUE, scale = FALSE)),
         Tempcenter = as.numeric(scale(Temp_mean, center = TRUE, scale = FALSE)),
         Flowcenter = as.numeric(scale(Flow_mean, center = TRUE, scale = FALSE)))%>%
  rename(dailyNEC = daily_NEC) %>%
  select(dailyNEC, temp, PARcenter, Covercenter, tempc) %>%
  drop_na() %>%
  filter(dailyNEC>0)%>%
  mutate(dailyNEClog = log(dailyNEC))

#NECMod<-bf(dailyNEC ~ rtref + log(exp(E/.0000862*(1/(27+273.15) - 1/temp))) +log(1/(1 + exp(Eh/0.0000862*(1/Th - 1/temp)))), 
#           family = "student", nl = TRUE)
#RTrefMod<-bf(rtref ~ Bpar*PARcenter + B0PAR,family = "student", nl = TRUE)
#PARMod<-bf(B0PAR ~ Bcover*Covercenter + B0Cover, family = "student", nl = TRUE)

#dailyNEC ~ rtref + log(exp(E/.0000862*(1/(27+273.15) - 1/temp))) +log(1/(1 + exp(Eh/0.0000862*(1/Th - 1/temp)))), 
#rtref ~ Bpar*PARcenter + B0PAR,
#B0PAR ~ Bcover*Covercenter + B0Cover, 

#nl = TRUE,
#Eh~1, Th~1 , E~1  

#prior(normal(0, 10), nlpar = "rtref", lb = 0),

set_inits <- function(seed = 1) {
  
  set.seed(seed)
  list(
    Intercept = rnorm(n = 1, mean = 0, sd = 10),
    sigma     = runif(n = 1, min = 1, max = 200),
    beta      = runif(n = 1, min = 1, max = 10)
  )
  
}

my_inits <- list(
  # different seed values will return different results
  set_inits(seed = 1),
  set_inits(seed = 2),
  set_inits(seed = 3),
  set_inits(seed = 4)
)


fit1<-brm(
    bf(dailyNEClog ~ rtref + 
         log(exp(E/0.0000862*(1/300.15 - 1/temp))) +
         log(1/(1 + exp(Eh/0.0000862*(1/Th - 1/temp)))),  
       
       nlf(rtref ~ Bpar*PARcenter + B0PAR),
       nlf(B0PAR ~ Bcover*Covercenter + B0Cover), 
       E~1, Eh~1, Th~1,Bcover~1, B0Cover~1, Bpar~1,
       nl = TRUE
    )+gaussian(),
  data = LTER1test,
   set_rescor(FALSE),
  prior = c(
    prior(normal(0,10), nlpar = "E", lb = 0), 
    prior(normal(0,10), nlpar = "Eh", lb = 0),
    prior(normal(320, 10), nlpar = "Th", lb = 0),
    prior(normal(0, 10), nlpar = "Bpar", lb = 0),
    prior(normal(0, 10), nlpar = "Bcover")
    ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
  init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 3000, warmup = 2000
  #silent = TRUE
  ) 

h <- data.frame(temp=seq(273.15+27,273.15+32, length.out = 50), Covercenter = 0, PARcenter = 0)
pred<-predict(fit1, newdata = h) %>%
  bind_cols(h)

ggplot(pred, aes(x = temp, y = Estimate))+
  geom_line()+
  geom_point(data = LTER1test, aes(x = temp, y = dailyNEC))

ggplot(LTER1test, aes(x = temp, y = dailyNEC))+
  geom_point()


fit1<-brm(
  bf(dailyNEC ~ rtref * exp(E/k * (1/300.15 - 1/(tempc + 273.15)))*
       1/(1 + exp(Eh/k * (1/(Th + 273.15) - 1/(tempc + 273.15)))),  
     E~1, Eh~1, Th~1, rtref~1, tref~1,
     nl = TRUE
  )+gaussian(),
  data = LTER1test,
  set_rescor(FALSE),
  prior = c(
    prior(constant(24), nlpar = tref),
    prior(constant(8.62e-05), nlpar = k),
    prior(normal(10,5), nlpar = "E", lb = 0), 
    prior(normal(20,5), nlpar = "Eh", lb = 0),
    prior(normal(30,10), nlpar = "Th", lb = 0),
    prior(normal(1,10), nlpar = "rtref", lb = 0)
   # prior(normal(0, 10), nlpar = "Bpar", lb = 0),
  #  prior(normal(0, 10), nlpar = "Bcover")
  ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
 # init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 3000
  #warmup = 2000
  #silent = TRUE
) 

h <- data.frame(tempc=seq(26,30, length.out = 50))
pred<-predict(fit1, newdata = h, summary = TRUE) %>%
  bind_cols(h)

ggplot(pred, aes(x = tempc, y = Estimate))+
  geom_line()+
  geom_point(data = LTER1test, aes(x = tempc, y = dailyNEClog))

LTER1test<-LTER1test %>%
  filter(dailyNEC<260)%>%
  bind_rows(tibble(temp = 303, dailyNEClog=4))

fit1<-brm(
  bf(dailyNEClog ~ rtref + 
       log(exp(E/0.0000862*(1/301 - 1/temp))) +
       log(1/(1 + exp(Eh/0.0000862*(1/Th - 1/temp)))),
     E~1, Eh~1, Th~1,rtref~1,
     nl = TRUE
  )+student(),
  data = LTER1test,
  set_rescor(FALSE),
  prior = c(
    prior(normal(0,10), nlpar = "E", lb = 0), 
    prior(normal(0,10), nlpar = "Eh", lb = 0),
    prior(normal(302, 10), nlpar = "Th", lb = 0),
    prior(normal(0, 5), nlpar = "rtref")
 #   prior(normal(0, 10), nlpar = "Bpar", lb = 0),
  #  prior(normal(0, 10), nlpar = "Bcover")
  ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
  init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 6000, warmup = 2000
  #silent = TRUE
) 

h <- data.frame(temp=seq(273.15+26,273.15+35, length.out = 50))
pred<-predict(fit1, newdata = h, summary = TRUE) %>%
  bind_cols(h)

ggplot(pred, aes(x = temp, y = Estimate))+
  geom_line()+
 # geom_line(aes(x = temp, y = Q2.5))+
  #geom_line(aes(x = temp, y = Q97.5))+
  geom_point(data = LTER1test, aes(x = temp, y = dailyNEClog))+
  scale_y_continuous(limits = c(0,6))


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



## Same above but with GP instead of NEP

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
         dailyGPP = daily_GPP) %>%
  drop_na(PAR)

mo<-lm(data = LTER1_Pg, Covercenter~PAR)

LTER1_Pg<-LTER1_Pg %>%
  bind_cols(augment(mo) %>%
  select(resid = .resid)) # cover residuals

fit1<-brm(
  bf(dailyGPP ~ ((alpha*Pmax*PAR)/(alpha*PAR+Pmax))-Rd, 
     nl = TRUE, alpha~1,Pmax~1,
     Rd~Tempcenter*resid)+ student(),
  data = LTER1_Pg,
  set_rescor(FALSE),
  prior = c(
    prior(normal(200,200), nlpar = "alpha", lb = 0), 
    prior(normal(3000,100), nlpar = "Pmax", lb = 0),
    prior(normal(1000, 500), nlpar = "Rd", lb = 0)
  ), control = list(adapt_delta = 0.99, max_treedepth = 20), 
#  init = my_inits,
  cores = 4, 
  chains = 3, seed = 11, iter = 8000, warmup = 2000
  #silent = TRUE
) 

pp_check(fit1, resp = "dailyGPP", ndraws = 20)
pp_check(fit1, resp = "Rd", ndraws = 20)
#pp_check(fit1, resp = "Pmax", ndraws = 20)
summary(fit1)

h <- data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                Tempcenter = rep(-1,250),
                Flowcenter = rep(0,250)) %>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(0,250),
                       Flowcenter = rep(0,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(0,250)))%>%
  bind_rows(data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                       resid = c(rep(-20,50),rep(-10,50), rep(0,50), rep(10,50),rep(20,50)),
                       Tempcenter = rep(1,250),
                       Flowcenter = rep(2,250)))

pred<-predict(fit1, newdata = h, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(h) %>%
  mutate(resid = as.factor(resid),
         Tempcenter = as.factor(Tempcenter))

ggplot(pred, aes(x= PAR, y  =Estimate))+
  geom_line(aes(color = Tempcenter, lty = resid))+
  scale_color_manual(values = c("blue","pink","red"))+
  
  geom_point(data = LTER1_Pg, aes(x = PAR, 
                                    y = dailyGPP, 
                                 #   size = mean_alive,
                                  #  fill = Temp_mean
                                  ), shape = 21)+
  labs(y = "GPP",
       linetype = "Calcifier Cover (Center)",
       color = "Temperature (Center)")+
  theme_bw()
ggsave(here("Output","GPP_PI.png"), width = 6, height = 6)

# MOdel just the mean

pred<-predict(fit1, newdata = data.frame(PAR=seq(0,50, length.out = 50), 
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
                        mean(LTER1_Pnet$PAR, na.rm = TRUE)), 
                Tempcenter = c(0,0,+0.2*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                          min(LTER1_Pnet$Temp_mean, na.rm = TRUE)),0), 
                resid = c(0,0,0,-20),
                State = c("Mean all","Light decrease", "Temp increase", "Cover decrease"))

pred<-predict(fit1, newdata = newdata, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(newdata)

ggplot(pred, aes(x = State, y = Estimate))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5))


  post<-posterior_predict(fit1,newdata = newdata)%>%
    as_tibble()
    
 colnames(post)<-pred$State   
 
 GPPchange<-post %>%
   pivot_longer(cols = `Mean all`:`Cover decrease`) %>%
   group_by(name)%>%
   summarise(mean = mean(value),
             se = mean(value)/sqrt(n()),
             lwr = mean(value, na.rm = TRUE) - (1.96 * sd(value, na.rm = TRUE)/sqrt(n())),
             upr = mean(value, na.rm = TRUE) + (1.96 * sd(value, na.rm = TRUE)/sqrt(n()))) %>%
   mutate(name = factor(name, levels = c("Mean all", "Light decrease",
                                         "Temp increase", "Cover decrease")))
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
 
 
 
 newdata2<-tibble(PAR = c(mean(LTER1_Pnet$PAR, na.rm = TRUE),
                         mean(LTER1_Pnet$PAR, na.rm = TRUE)-0.2*mean(LTER1_Pnet$PAR, na.rm = TRUE),
                         mean(LTER1_Pnet$PAR, na.rm = TRUE)), 
                 Tempcenter = c(0,0,+0.2*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                            min(LTER1_Pnet$Temp_mean, na.rm = TRUE))), 
                 resid = c(0,0,0),
                 State = c("Mean all","Light decrease", "Temp increase"))
 
 
Newdata2<- tibble(PAR = mean(LTER1_Pnet$PAR, na.rm = TRUE)-seq(-1,1,length.out = 11)*mean(LTER1_Pnet$PAR, na.rm = TRUE),
           Tempcenter = seq(-1,1,length.out = 11)*(max(LTER1_Pnet$Temp_mean, na.rm = TRUE)-
                                                    min(LTER1_Pnet$Temp_mean, na.rm = TRUE)))
# expand out
Newdata2<-Newdata2 %>%
  mutate(PAR_per = seq(100,-100,length.out = 11),
         Temp_per = seq(-100,100,length.out = 11)) %>%
   expand.grid() %>%
  mutate(resid = 0)
 
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
  scale_fill_gradient2(limits = c(-100,75), low = "red", high = "blue", midpoint = 0, mid = "white") +
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
  
GPPchange<-post %>%
  pivot_longer(cols = `Mean all`:`Cover decrease`) %>%
  group_by(name)%>%
  summarise(mean = mean(value),
            se = mean(value)/sqrt(n()),
            lwr = mean(value, na.rm = TRUE) - (1.96 * sd(value, na.rm = TRUE)/sqrt(n())),
            upr = mean(value, na.rm = TRUE) + (1.96 * sd(value, na.rm = TRUE)/sqrt(n()))) %>%
  mutate(name = factor(name, levels = c("Mean all", "Light decrease",
                                        
############
 test<- LTER1_Pg %>%
    data_grid(PAR = seq_range(c(0,50), n = 50),
              Covercenter = seq_range(c(-20,20), n = 50),
              Tempcenter = c(-2,2)) %>%
    add_epred_draws(fit1, ndraws = 1000) 
  
 test %>% 
    ggplot(aes(x = PAR, y = .epred )) + 
    stat_lineribbon()
  ####

predict(fit1, newdata = h, summary = TRUE, resp = "Rd") %>%
  bind_cols(h) %>% 
  filter(PAR == 50) %>%
  mutate(Covercenter = as.factor(Covercenter))%>%
  ggplot(aes(x = Tempcenter, y = -Estimate))+
  geom_line(aes(color = Covercenter))+
  geom_point(data = LTER1_Pnet, aes(x = Tempcenter, y = Rd, size = mean_alive))

conditions <- data.frame(Covercenter = c(-10, 0, 10))
plot(conditional_effects(fit1, effects = "Tempcenter",
                         conditions = conditions))

ggplot()+
  geom_line()+
  geom_point(data = LTER1_Pnet, aes(x = Tempcenter, y = Rd, color = Tempcenter, size = Covercenter))

ggplot(pred, aes(x = PAR, y = Estimate))+
  geom_line(aes(color  = Tempcenter, lty = Covercenter))+
  # geom_line(aes(x = temp, y = Q2.5))+
  #geom_line(aes(x = temp, y = Q97.5))+
  scale_color_manual(values = c("blue","lightblue","pink","red"))+
  geom_point(data = LTER1_Pnet, aes(x = PAR, y = dailyNPP))
  scale_y_continuous(limits = c(-200,700))

h2 <- data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
                Covercenter = rep(0,250),
                tempc = c(rep(27,50),rep(28,50), rep(29,50), rep(30,50),rep(31,50))) %>%
  bind_rows(
  data.frame(PAR=rep(seq(0,50, length.out = 50),5), 
             Covercenter = rep(-10,250),
             tempc = c(rep(27,50),rep(28,50), rep(29,50), rep(30,50),rep(31,50)))
  )

pred<-predict(fit1, newdata = h2, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(h2) %>%
  mutate(tempc = as.factor(tempc),
         Covercenter = as.factor(Covercenter))

ggplot(pred, aes(x = PAR, y = Estimate))+
  geom_line(aes(color = tempc, alpha = Covercenter))+
  # geom_line(aes(x = temp, y = Q2.5))+
  #geom_line(aes(x = temp, y = Q97.5))+
  geom_point(data = LTER1_Pnet, aes(x = PAR, y = dailyNPP))

PAR<-0
((40.72*369.77*PAR)/(40.72*PAR+369.77))+-115.14 

## get rD from the interceps where PAR is 0
h2 <- data.frame(PAR=rep(0,50), 
                 Covercenter = seq(-25,25, length.out = 50),
                 tempc = rep(29,50) )
  
pred<-predict(fit1, newdata = h2, summary = TRUE, allow_new_levels = TRUE) %>%
  bind_cols(h2) 
  
ggplot(pred, aes(x = Covercenter, y = Estimate))+
  geom_line()+
  geom_point(data = LTER1_Pnet, aes(x = Covercenter, y = Rd))

summary(fit1)
 


  Newdata<-tibble(Covercenter = seq(-25,25, length.out = 50)) %>%
    mutate(.fitted = -5.35*Covercenter-275.26)

  ggplot(LTER1, aes(x = mean_alive, y = daily_R))+
    geom_point()
    geom_line(data = Newdata, aes(x = Covercenter, y = .fitted))
    
R<-conditional_effects(fit1, "Covercenter", resp = "Rd_Intercept",  resolution = 200)
  