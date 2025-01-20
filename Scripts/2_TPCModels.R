## Thermal perforance curves for NEC ###
## By Nyssa Silbiger ###

## Edited on 2025-01-16 ####


source(here("Scripts","0_DataCleaning.R"))


### Thermal performance models  

# choose model
mod = 'sharpschoolhigh_1981'

# get start vals
start_vals <- get_start_vals(LTER1$Temp_mean, LTER1$daily_NEC, model_name = 'sharpeschoolhigh_1981')
start_vals[4]<-0

# get limits
low_lims <- get_lower_lims(LTER1$Temp_mean, LTER1$daily_NEC, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(LTER1$Temp_mean, LTER1$daily_NEC, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(daily_NEC~sharpeschoolhigh_1981(temp = Temp_mean, r_tref,e,eh,th, tref = 25),
                     data = LTER1,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 20,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(Temp_mean = seq(min(LTER1$Temp_mean, na.rm = TRUE), max(LTER1$Temp_mean, na.rm = TRUE), 0.1))
preds <- augment(fit, newdata = new_data)


## Add a column for NEC normalized to light
LTER1 <- LTER1 %>%
  mutate(NEC_Light = (daily_NEC*12*.001)/(TotalPAR_mean))# convert both to mol per day
 
# remove 2 extreme outliers in the NEC data for the TPC  
LTER1_no_outliers<-LTER1 %>%
  filter(daily_NEC>0,
         daily_NEC<270)

# run the TPC model while accounting for light
# get start vals
start_vals <- get_start_vals(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$daily_NEC, model_name = 'sharpeschoolhigh_1981')
#start_vals[2]<-0

# get limits
low_lims <- get_lower_lims(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$daily_NEC, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$daily_NEC, model_name = 'sharpeschoolhigh_1981')


fit <- nls_multstart(daily_NEC~sharpeschoolhigh_1981(temp = Temp_mean, r_tref,e,eh,th, tref = 27),
                     data = LTER1_no_outliers,
                     iter = 500,
                     start_lower = start_vals ,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')


Topt_data<-calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(Temp_mean = seq(min(LTER1_no_outliers$Temp_mean, na.rm = TRUE)-0.5, max(LTER1_no_outliers$Temp_mean, na.rm = TRUE)+1, 0.1))
preds <- augment(fit, newdata = new_data) 

# plot the TPC
ggplot()+
  scale_x_continuous(limits = c(27.5,30.2))+
  geom_vline(xintercept = Topt_data$topt, linetype = 2, alpha = 0.5)+
  #scale_y_continuous(limits = c(0, 160))+
  geom_line(data = preds, aes(x = Temp_mean, y = .fitted), inherit.aes = FALSE)+
  geom_point(data = LTER1_no_outliers, aes(x = Temp_mean, y =daily_NEC, fill = Season, size = total_Calc), shape = 21)+
  scale_fill_manual(values = c("gray","black"))+
  annotate("text", x = Topt_data$topt+0.4, y = 200, label = expression(paste("T"[opt],"= 29.31 ", degree, "C")))+
  labs(size = "% Cover of Calcifiers",
       x = "Temperature ("~degree~"C)",
       y = bquote(atop("Net Ecosystem Calcification",
                       "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

ggsave(here("Output","NEC_TPC.pdf"), width = 6, height = 6)

## Run the above but with bootstrapps for confidence intervals
# refit model using nlsLM
fit_nlsLM <- minpack.lm::nlsLM(daily_NEC~sharpeschoolhigh_1981(temp = Temp_mean, r_tref,e,eh,th, tref = 27),
                               data = LTER1_no_outliers,
                               start = coef(fit),
                               lower = get_lower_lims(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$daily_NEC, model_name = 'sharpeschoolhigh_1981'),
                               upper = get_upper_lims(LTER1_no_outliers$Temp_mean,  LTER1_no_outliers$daily_NEC, model_name = 'sharpeschoolhigh_1981'),
                               weights = rep(1, times = nrow(LTER1_no_outliers)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(LTER1_no_outliers$Temp_mean)-0.5, max(LTER1_no_outliers$Temp_mean)+1, length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(temp, r_tref, e, eh, th, tref = 27))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p1_TPC <- ggplot() +
  scale_x_continuous(limits = c(27.5,30.2))+
  geom_vline(xintercept = Topt_data$topt, linetype = 2, alpha = 0.5)+
  geom_line(aes(Temp_mean, .fitted), preds, col = 'blue') +
  # geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_line(aes(temp, pred, group = iter), boot1_preds, col = 'blue', alpha = 0.007) +
  geom_point(aes(Temp_mean, daily_NEC, fill = Season, size = total_Calc), LTER1_no_outliers,  alpha = 0.5, shape = 21) +
  theme_bw() +
  scale_fill_manual(values = c("gray","black"))+
  annotate("text", x = Topt_data$topt+0.5, y = 220, label = expression(paste("T"[opt],"= 29.31 ", degree, "C")), size = 5)+
  labs(size = "% Cover of Calcifiers",
       x = "Temperature ("~degree~"C)",
       y = bquote(atop("Net Ecosystem Calcification",
                       "(mmol" ~ CaCO[3]~m^-2~d^-1~")")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "top")


# Bootstrapped parameters
# get parameters of fitted model
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)
#> Joining with `by = join_by(param)`

p2_TPC<-ci_extra_params %>%
  filter(param %in% c("topt", "ctmin","ctmax")) %>% 
  mutate(param = case_when(param == "topt"~ "T<sub>opt",
                           param == "ctmin"~ "CT<sub>min",
                           param == "ctmax"~ "CT<sub>max"))%>%
  ggplot(aes(y = param, x = estimate))+
  geom_point(size = 3)+
  geom_errorbar(aes(xmin = conf_lower, xmax = conf_upper), width = 0.1)+
  labs(x =  "Temperature ("~degree~"C)",
       y = "")+
  theme_bw()+
  theme(axis.text = element_markdown(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


p1_TPC+p2_TPC+plot_layout(guides = "collect") & theme(legend.position = 'top', legend.box = "vertical")

ggsave(here("Output","TPCmodels.pdf"), width = 10, height = 6)

## Same TPC,  but with NEC (mmol m2 hr1) normalized to light (mols m2 d1).
# First convert NEC to per day

LTER1_no_outliers<-LTER1 %>%
  filter(NEC_Light>0)  

ggplot(LTER1_no_outliers, aes(x = Temp_mean, y = NEC_Light))+
  geom_point()

# run the TPC model while accounting for light
# get start vals
start_vals <- get_start_vals(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$NEC_Light, model_name = 'sharpeschoolhigh_1981')
#start_vals[2]<-0

# get limits
low_lims <- get_lower_lims(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$NEC_Light, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(LTER1_no_outliers$Temp_mean, LTER1_no_outliers$NEC_Light, model_name = 'sharpeschoolhigh_1981')


fit_light <- nls_multstart(NEC_Light~sharpeschoolhigh_1981(temp = Temp_mean, r_tref,e,eh,th, tref = 27),
                           data = LTER1_no_outliers,
                           iter = 500,
                           start_lower = start_vals ,
                           start_upper = start_vals,
                           lower = low_lims,
                           upper = upper_lims,
                           supp_errors = 'Y')


Topt_data_Light<-calc_params(fit_light) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(Temp_mean = seq(min(LTER1_no_outliers$Temp_mean, na.rm = TRUE)-0.5, max(LTER1_no_outliers$Temp_mean, na.rm = TRUE)+1, 0.1))
preds <- augment(fit_light, newdata = new_data) 

ggplot()+
  scale_x_continuous(limits = c(27.5,30.2))+
  geom_vline(xintercept = Topt_data_Light$topt, linetype = 2, alpha = 0.5)+
  #scale_y_continuous(limits = c(0, 160))+
  geom_line(data = preds, aes(x = Temp_mean, y = .fitted), inherit.aes = FALSE)+
  geom_point(data = LTER1_no_outliers, aes(x = Temp_mean, y =NEC_Light, fill = Season, size = total_Calc), shape = 21)+
  scale_fill_manual(values = c("gray","black"))+
  annotate("text", x = Topt_data_Light$topt+0.4, y = 0.15, label = expression(paste("T"[opt],"= 29.26 ", degree, "C")))+
  labs( size = "% Cover of Calcifiers",
        x = "Temperature ("~degree~"C)",
        y = bquote(atop("Net Ecosystem Calcification light normalized",
                        "(mol" ~ CaCO[3]~" / mols photons )")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
