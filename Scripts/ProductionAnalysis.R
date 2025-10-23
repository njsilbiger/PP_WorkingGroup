### Updated and cleaned analysis for primary production paper###
## We first run a Bayesian analysis to extract Pmax and ER for each year##
## Then we look at drivers of Pmax and ER with a focus on coral cover, temperature,
## and flow. Then we end with a Bayesian SEM that also includes NEC ###

## By Nyssa Silbiger ###
## Created on 2025-10-22 ####

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
library(mgcv)
library(blavaan)
library(semPlot)
library(bayesplot)
library(ggridges)
library(ggsci)
library(psych)
library(ggcorrplot2)

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

# Turb %N
Turb<-read_csv(here("Data","MCR_LTER_Macroalgal_CHN_2005_to_2024_20250616.csv")) %>%
  filter(Habitat == "Backreef",
         Genus == "Turbinaria",
         Site == "LTER_1") %>%
  drop_na(N)%>%
  group_by(Year) %>%
  summarise(N_percent = mean(N, na.rm = TRUE),
            C_percent = mean(C, na.rm = TRUE),
            CN = mean(CN_ratio, na.rm = TRUE)) %>%
  ungroup()
  
## Bring in the yearly SST data 
yearly_sst<-read_csv(here("Data","SST_year.csv"))

# Total Fish Biomass
fish<-read_csv(here("Data","fish_clean.csv"))

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
  mutate(name = ifelse(name %in%c("Algal Turf", 
                                  "Damselfish Turf", 
                                  "Coral Rubble"
                                  #"Bare Space"
                                  ),"Turf", name))%>%
  group_by(Year, Site, name)%>%
  summarise(mean_cover = mean(Percent_Cover, na.rm = TRUE))

## Calculate the total percent of calcifiers
Total_Calc<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines"))%>%
  group_by(Year, Site)%>%
  reframe(total_Calc = mean_cover[name == "Coral"]+
            mean_cover[name == "Crustose Corallines"])

# Total macroproducers
TotalLiving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"))%>%
  group_by(Year, Site)%>%
  summarise(mean_alive = sum(mean_cover),
            mean_fleshy = sum(mean_cover[name == "Fleshy Macroalgae"]),
            mean_coral = sum(mean_cover[name == "Coral"]))

# calculate yearly averages for all the metabolism data
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


### Make a plot of all living cover (basically everything - sand + coral RUbble and algal turf)
LTER1_coverliving<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae"),
         Site == "LTER 1")%>%
  group_by(Year)%>%
  summarise(mean_alive = sum(mean_cover)) %>%
  ggplot(aes(x = Year, y = mean_alive))+
  geom_line(linewidth = 1, alpha = 0.2, lty = 2)+
  geom_point(size = 2)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(limits = c(0,40))+
  labs(x = "",
       y = "Cover of living macro-producers (%)",
       color = "",
       # title = "LTER 1 only"
  )+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank())

LTER1_cover<-Benthic_summary_Algae %>%
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae", "Turf"),
         Site == "LTER 1")%>%
  ggplot(aes(x = Year, y = mean_cover, color = name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen","lightgreen"))+
  geom_text(data = tibble(Year = c(2007, 2017, 2015, 2014), 
                          name = c("Coral","Crustose Corallines","Fleshy Macroalgae", "Turf"), 
                          mean_cover = c(33,20,0, 54)),
            aes(x = Year, y = mean_cover, label = name))+
  labs(x = "",
       y = "Cover (%)",
       color = "",
       #title = "LTER 1 only"
  )+
#  scale_y_continuous(limits = c(0,40))+
  # scale_x_continuous(limits = c(2008, 2025))+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        #     legend.position = c(0.71, 0.9),
        legend.position = "none",
        #    legend.background = element_blank(),
        legend.text = element_text(size = 12),
        #  axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

#### Bayesian analysis ###########
### Run PI curves allowing Rd and Pmax to change by year
All_PP_data<-All_PP_data %>%
  left_join(TotalLiving %>% # join the cover data
              filter(Site == "LTER 1"))

# make a dataframe with names the brms likes
LTER1_Pnet <-All_PP_data %>%
  mutate(tempc = Temperature_mean,
         Flowmean = Flow_mean,
         cover = mean_alive,
         coral = mean_coral,
         flow_log = log(Flowmean), # log scale the flow data since it fits in a power function
         # daily_R = -daily_R
         UPDN = as.factor(UPDN), # make factors for the analysis
         Year = as.factor(Year))

# Run a bayesian PI curve that varies by year
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

# extract the fixed effects
names<-rownames(fixef(fit1_f))

#extract the coefficients
coefficients<-as_tibble(fixef(fit1_f)) %>%
  mutate(params = names)

PRValues<-coefficients %>%
  separate(params, sep = "_",into = c("param_type","yearname"), remove = FALSE) %>%
  filter(param_type %in% c("Pmax","Rd")) %>% # extract Pmax and Rd
  separate(yearname, sep = "r", into = c("y","year")) %>% # clean up the years
  mutate(year = as.numeric(year))  %>% # make year a number again
  select(Year = year, Estimate, param_type) %>% # pivot wider so pmax and r in own column
  pivot_wider(names_from = param_type,
              values_from = Estimate)

#add Pmax and Rd to the yearly averages and make Rd positive
#### yearly averages
Year_Averages <- Year_Averages %>%
  full_join(PRValues) %>%
  mutate(Rd = - Rd) %>% # make Rd positive
  full_join(Turb)  %>% # add in the turbinaria data
  full_join(yearly_sst %>%
              select(Year = Year_Benthic, # make year last year because last years temp should affect next years coral cover
                     mean_SST, max_SST)
            ) %>% # add the SST data
  full_join(fish)%>% # bring in the fish biomass data
  arrange(Year) %>%
  mutate(log_coral = log(mean_coral)) # log transform the coral data

### How is everything changing over time ####

# create a dataframe of standardized data
std_data<- Year_Averages %>%
  select(mean_coral, mean_fleshy, Pmax, Rd, NEC_mean_Day, N_percent,
         mean_SST, mean_biomass, NP_mean) %>%
  mutate(across(everything(), 
                ~as.numeric(scale(.x)))) %>%
  bind_cols(Year_Averages %>% select(Year))

# Coral
coral_year<-brm(mean_coral~Year, data = std_data)
posterior_coral <- as_tibble(as.matrix(coral_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "% Coral Cover")

# Macroalgae
fleshy_year<-brm(mean_fleshy~Year, data = std_data)
posterior_algae <- as_tibble(as.matrix(fleshy_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "% Macroalgae Cover")

# fish biomass
fish_year<-brm(mean_biomass~Year, data = std_data)
posterior_fish <- as_tibble(as.matrix(fish_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Fish Biomass")

# SST
SST_year<-brm(mean_SST~Year, data = std_data)
posterior_sst <- as_tibble(as.matrix(SST_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Sea Surface Temperature")

# Percent N
N_year<-brm(N_percent~Year, data = std_data)
posterior_N <- as_tibble(as.matrix(N_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "%N Content")

# Percent C
C_year<-brm(N_percent~Year, data = std_data)
posterior_C <- as_tibble(as.matrix(C_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "%C Content")

# Rd
Rd_year<-brm(Rd~Year, data = std_data)
posterior_Rd <- as_tibble(as.matrix(Rd_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Ecosystem Respiration")

# Pmax
Pmax_year<-brm(Pmax~Year, data = std_data)
posterior_Pmax <- as_tibble(as.matrix(Pmax_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Maximum Photosynthetic Capacity")

# NEC
NEC_year<-brm(NEC_mean_Day~Year, data = std_data)
posterior_NEC <- as_tibble(as.matrix(NEC_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Net Ecosystem Calcification")

# NEP
NEP_year<-brm(NP_mean~Year, data = std_data)
posterior_NEP <- as_tibble(as.matrix(NEP_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Net Ecosystem Production")

# Bring together all the posterior data
All_posterior<-bind_rows(posterior_coral,
                         posterior_algae,
                         posterior_fish,
                         posterior_N,
                         posterior_Rd,
                         posterior_Pmax,
                         posterior_sst,
                         posterior_NEC,
                         posterior_NEP,
                         posterior_C)

# make a plot showing the change in each parameter over time
All_posterior %>%
  ggplot(aes(x = Year, y = fct_reorder(Parameter, Year, mean), 
             fill = Parameter))+
  geom_vline(xintercept = 0)+
  geom_density_ridges(alpha = 0.5, color = NA)+
  scale_fill_npg()+
  annotate("text",x = 0.15, y = 0.75, label = "Increasing over time")+
  annotate("text",x = -0.15, y = 0.75, label = "Decreasing over time")+
  labs(x = "Standardized change per year",
       y = "")+
  lims(x = c(-0.3,0.3))+
  theme_ridges()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12))

ggsave(here("Output","ChangePosterior.png"), height = 6, width = 10)

## Make a correlation plot of all the data we are interested in

cor_mat <- rstatix::cor_mat(Year_Averages %>% select(N_percent, C_percent, mean_coral,
                                                     mean_fleshy, NEC_mean_Day,
                                                     mean_SST, Rd, Pmax, mean_biomass, 
                                                     NP_mean, GP_mean, Flow_mean, PAR_mean),
                            method = "pearson")
cor_p   <- rstatix::cor_pmat(Year_Averages%>% select(N_percent, C_percent, mean_coral,
                                                mean_fleshy, NEC_mean_Day,
                                                mean_SST, Rd, Pmax,mean_biomass, 
                                                NP_mean, GP_mean, Flow_mean, PAR_mean),
                             method ="pearson")
ggcorrplot(
  cor_mat,
  hc.order = TRUE,
  type = "lower",
  p.mat = cor_p,
  sig.level = 0.05,
  #pch = 8,
  #insig = "blank",
  #insig = "pch",
  lab = TRUE,
  lab_size = 2.5,
  colors = c("#6D9EC1", "white", "#E46726"),
  title = "Significant correlations of all measured variables"
)+
  theme(panel.grid.major.x  = element_blank())+
 
ct <- corr.test(Year_Averages %>% select(N_percent, C_percent, mean_coral,
                                         mean_fleshy, NEC_mean_Day,
                                         mean_SST, Rd, Pmax, mean_biomass, 
                                         NP_mean, GP_mean, Flow_mean, PAR_mean), adjust = "none")
corr <- ct$r
p.mat <- ct$p

  
ggcorrplot.mixed(corr, 
                 upper = "ellipse", 
                 lower = "number", 
                 p.mat = p.mat, 
                insig = "label_sig", 
                sig.lvl = c(0.05, 0.01, 0.001))

ggsave(here("Output","correlations.png"), width = 6, height = 6)
## Use a GAM to test how different variables affect ER while controlling for the underlying time trend
model_ER_full <- gam(Rd ~ s(Year, k=5) + s(log(mean_coral), k=5) + s(Flow_mean, k=5) + 
                       s(Temperature_mean, k=5) +s(N_percent, k=5), 
                     data = Year_Averages, method = "REML")
summary(model_ER_full)
gratia::draw(model_ER_full, residuals = TRUE)

model_Pmax_full <- gam(Pmax ~ s(Year, k=5) + s(log(mean_coral), k=5) + s(Flow_mean, k=5) + 
                       s(Temperature_mean, k=5) +s(N_percent), 
                     data = Year_Averages, method = "REML")
summary(model_Pmax_full)
gratia::draw(model_Pmax_full, residuals = TRUE)

# --- Hypothesis 1: ER drives Nutrient Recycling (%N) ---
# Model: N_Turbinaria ~ s(Year) + s(ER)
# s(Year) accounts for the long-term trend, so the effect of s(ER) is independent of that trend.
# We use k=5 to limit the complexity of the smooth, which is wise for small N.
model_N <- gam(N_percent ~  s(Rd) +s(Year) +s(log(mean_coral)), data = Year_Averages, method = "REML")
# Check the summary. Look for a low p-value (<0.05) for s(ER).
summary(model_N)

# Visualize the partial effects using gratia::draw()
plot_N <- gratia::draw(model_N, residuals = TRUE) +
  labs(title = "Partial Effects on N in Turbinaria") +
  theme_bw()

## Run a Bayesian SEM to see how the different parameters are related
# First detrend the data

sem_data <- Year_Averages %>%
    select(mean_coral, mean_fleshy, Pmax, Rd, NEC_mean_Day, N_percent,
           Flow_mean, Temperature_mean, mean_SST) %>%
  mutate(across(everything(), 
                ~ residuals(gam(.x ~ s(Year, k=5), data = Year_Averages, na.action = na.exclude))))


# --- 2. Define the Expanded SEM Syntax ---
# This model is much more detailed and theoretically rich.

bsem_model_full_syntax <- '
  # LEVEL 1: Community Structure Models
  # Coral cover is driven by temperature stress and algal competition
  mean_coral ~ Temperature_mean + mean_fleshy 
  # Algal cover is driven by temp and controlled by fish
 # Macroalgae ~ Temp + Fish_Biomass

  # LEVEL 2: Metabolic Rate Models
  # GPP is driven by the producers (corals, algae) and abiotic factors
  Pmax ~ c1*mean_coral + c2*mean_fleshy + Temperature_mean + Flow_mean
  # ER is driven by all respiring organisms and abiotic factors
  Rd ~ r1*mean_coral + r2*mean_fleshy + r3*Temperature_mean + r4*Flow_mean

  # LEVEL 3: Ecosystem Function Models (Original Hypotheses)
  # Nutrient recycling is driven by total respiration
  N_percent  ~ n1*Rd
  # Calcification is driven by calcifiers and inhibited by algae
  NEC_mean_Day  ~ nc1*mean_coral + nc2*Pmax 

  # COVARIANCES: Allow unexplained parts of external drivers to correlate
  Temperature_mean ~~ Flow_mean
  Pmax ~~ Rd
  
  # Indirect effects 
  # coral to percent N via ER
  coral_ER_N:=n1*r1
'
# --- 3. Fit the Full Bayesian SEM ---
bsem_fit_full <- bsem(
  model = bsem_model_full_syntax,
  data = sem_data,
  missing = 'fiml', # Essential for handling the missing NEC data
  n.chains = 4,
  sample = 6000,      # Increased iterations for a more complex model
  burnin = 1000,
  std.lv = TRUE,
  seed = 42
)

# --- 4. Interpret and Visualize ---
summary(bsem_fit_full, standardize = TRUE, fit.measures = TRUE, ci = TRUE)

# The path diagram will be more complex, so a different layout might be better.
semPaths(bsem_fit_full, 
         what = "std",
         whatLabels = "est.std",
         layout = "spring", # "spring" layout handles web-like structures well
         edge.label.cex = 1.1,
         fade = FALSE,
         residuals = TRUE, # Show residuals to see unexplained variance
         nCharNodes = 0,
         sizeMan = 10,
         style = "lisrel",
         title = TRUE,
         main = "Expanded Bayesian SEM of Reef Ecosystem Drivers")

### SImple model with SST driving coral and coral driving %N and %N driving ER



bsem_model_full_syntax <- '
  # LEVEL 1: Community Structure Models
  # Coral cover is driven by temperature stress 
  log_coral ~ a1*mean_SST 
 
  # LEVEL 2: Coral drives nutrient concentration 
   N_percent ~ b1*log_coral
   
   # Level 3: Nutrients drive ecosystem respiration
   Rd ~c1*N_percent
 
  # Indirect effects 
  # SST to N percent via coral
  SST_coral_N:=a1*b1
  # coral to percent N via ER
  coral_ER_N:=b1*c1
  #total effect of SST on Rd
  total_SST_ER:= SST_coral_N+coral_ER_N
'
# --- 3. Fit the Full Bayesian SEM ---
bsem_fit_small <- bsem(
  model = bsem_model_full_syntax,
  data = Year_Averages,
  missing = 'fiml', # Essential for handling the missing NEC data
  n.chains = 4,
  sample = 6000,      # Increased iterations for a more complex model
  burnin = 1000,
  std.lv = TRUE,
  seed = 42
)

# --- 4. Interpret and Visualize ---
summary(bsem_fit_small, standardize = TRUE, fit.measures = TRUE, ci = TRUE)
semPaths(bsem_fit_small,
         what = "std",
         layout = "tree",
         intercepts = FALSE,
         residuals = FALSE,
         curveAdjacent = TRUE)
