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
library(viridis)
library(mgcv)
library(blavaan)
#library(semPlot)
library(bayesplot)
library(ggridges)
library(ggsci)
library(psych)
library(ggcorrplot)
library(ggcorrplot2)
library(ggeffects)


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

## Water nutrients - TAKE the mean from the bimonthly surveys (2005:2018)
water<-read_csv(here("Data/WaterColumnN.csv"))%>% 
  mutate(Date = mdy(Date), Year = year(Date))%>%
  group_by(Year)%>%
  summarise_at(vars(Phosphate:Nitrite_and_Nitrate), "mean") 

# Inpute the remaining water based on the turb data
AllNutrients<-Turb %>%
  full_join(water) %>%
  arrange(Year)

modN<-lm(Nitrite_and_Nitrate~N_percent, data = AllNutrients)
anova(modN)
summary(modN)

## predicted N+N from the turbinaria data which has a significant p value
AllNutrients$NN_impute<-predict(modN, newdata = AllNutrients)

## Note: Linda's paper says Porites produce ~ 0.25 umol L-1 N+N per 8 hour incubation
# x 3 would give per 24 hours = ~0.75 umol L-1 - coral surface area was 124 cm2
# 0.75/124 gives umol L-1 Cm-2 x 1000 = m2 6.05 umol L-1 m-2 - multiply this by the
# percent coral cover and the surface area of the transect (12.5m2)
# 75.625 umol L-1 tranect of 100% Porites-1

## BringmodN## Bring in the yearly SST data 
yearly_sst<-read_csv(here("Data","SST_year.csv"))
#InSituData
insitu_temp<-read_csv(here("Data","InSituTemp.csv"))

# Total Fish Biomass
fish<-read_csv(here("Data","fish_clean.csv"))
fish_trophic<-read_csv(here("Data","fish_summary.csv")) %>%
  pivot_wider(names_from = trophic_new, 
              values_from = fish_g_m2) %>%
  select(!Other)

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
## Note these are 0.25 m2 each and there are 50 quads per site (12.5 m2)
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
                                  "Coral Rubble",
                                  "Lobophora variegata",
                                  "Shell Debris",  
                                   "Bare Space"
                                  ),"Turf/Cyanobacteria", name))%>%
  mutate(name = ifelse(name %in%c("Peyssonnelia bornetii", 
                                  "Peyssonnelia sp.", 
                                  "Sponge",
                                  "Tridacna sp.",
                                  "No data"
  ),"Sand", name))%>% # there is barely any of this in the dataset so group it with sand for the visual
  group_by(Year, Site, name)%>%
  summarise(total_cover = sum(Percent_Cover, na.rm = TRUE),
            mean_cover = 100*total_cover/5000)

myPal <- c(Coral = "#c38370", `Fleshy Macroalgae` = "#9da",
           `Turf/Cyanobacteria` =  "#9da993",
           #   millepora = "#bdc3cb", 
             `Crustose Corallines` = "#523a28", 
           Other = "#bdc3cb", 
           Sand = "#d6ad60")

Benthic_summary_Algae %>%
  mutate(name = factor(name, levels = c("Coral","Fleshy Macroalgae",
                                        "Crustose Corallines","Turf/Cyanobacteria",
                                        "Sand","Other")))%>%
  filter(Site == "LTER 1")%>%
  ggplot(aes(x = Year, y = mean_cover, fill = name))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = myPal)+
  theme_classic() +
  labs(fill = "",
       y = "% Cover") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "bottom") 
  

ggsave(filename = here("Output","BenthicBob.pdf"), width = 8, height = 6)

# what is the coral cover in year 1 and year 2. Multiple that by 75.625 for N flux
Benthic_summary_Algae %>%
  filter(Year == 2006|Year == 2025) %>%
  filter(name == "Coral", Site == "LTER 1")

#31 % coral cover to 6.8%
0.31*75.625 #23.44 umol L-1 transect-1
0.068*75.625 #5.14 umol L-1 transect-1 - 78% reduction in N production

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

# Pete's benthic data which is slightly different than Bob's
PeteData<-read_csv(here("Data","PeteCoralCover.csv")) %>%
  rename(Year = year)


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
  full_join(TotalLiving %>%
              filter(Site == "LTER 1")) %>%
  full_join(NEC %>% 
              group_by(Year,Day_Night)%>% 
              summarise(NEC_mean = mean(NEC,na.rm = TRUE),
                        NEC_SE = sd(NEC, na.rm = TRUE)/sqrt(n())) %>%
              pivot_wider(names_from = Day_Night, values_from = c(NEC_mean, NEC_SE))) %>%
  full_join(PeteData) %>%
  full_join(water) %>%
  arrange(Year)


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
  filter(name %in% c("Coral","Crustose Corallines","Fleshy Macroalgae", "Turf/Cyanobacteria"),
         Site == "LTER 1")%>%
  ggplot(aes(x = Year, y = mean_cover, color = name))+
  geom_point(size = 2)+
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("#CC7161","lightpink","darkgreen","lightgreen"))+
  geom_text(data = tibble(Year = c(2007, 2017, 2015, 2014), 
                          name = c("Coral","Crustose Corallines","Fleshy Macroalgae", "Turf/Cyanobacteria"), 
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
  full_join(insitu_temp) %>% # bring in the in situ temperature data
  arrange(Year) %>%
  mutate(log_coral = log(mean_coral)) %>% # log transform the coral data
  rename(mean_biomass = total_fish_g_m2) %>%
  left_join(fish_trophic) %>%
  filter(Year <2026) %>%
  rename(fish_dead_coral = `Benefits from dead coral`)

### How is everything changing over time ####

# create a dataframe of standardized data
std_data<- Year_Averages %>%
  select(mean_coral, mean_fleshy, Pmax, Rd, NEC_mean_Day, N_percent,
         mean_SST, mean_biomass,fish_dead_coral, Corallivore, NP_mean, Max_temp, GP_mean, Nitrite_and_Nitrate,
         Phosphate) %>%
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
  mutate(Parameter = "Total Fish Biomass")

# Corallivore biomass
Corallivore_year<-brm(Corallivore~Year, data = std_data)
posterior_Corallivore <- as_tibble(as.matrix(Corallivore_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Corallivore Biomass")


# Dead coral fish biomass
dead_coral_fish_year<-brm(fish_dead_coral~Year, data = std_data)
posterior_dead_coral_fish <- as_tibble(as.matrix(dead_coral_fish_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Herbivore/bioeroding fish biomass")

# SST
SST_year<-brm(mean_SST~Year, data = std_data)
posterior_sst <- as_tibble(as.matrix(SST_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Sea Surface Temperature")

temp_year<-brm(Max_temp~Year, data = std_data)
posterior_temp <- as_tibble(as.matrix(temp_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Max Temperature")

# Percent N
N_year<-brm(N_percent~Year, data = std_data)
posterior_N <- as_tibble(as.matrix(N_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "%N Content")

# water N
Nwater_year<-brm(Nitrite_and_Nitrate~Year, data = std_data)
posterior_Nwater <- as_tibble(as.matrix(Nwater_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Nitrate + Nitrite")

# water N
Pwater_year<-brm(Phosphate~Year, data = std_data)
posterior_Pwater <- as_tibble(as.matrix(Pwater_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Phosphate")

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

# GP
GP_year<-brm(GP_mean~Year, data = std_data)
posterior_GP <- as_tibble(as.matrix(GP_year)) %>%
  select(Year = b_Year)%>%
  mutate(Parameter = "Gross Ecosystem Production")

# Bring together all the posterior data
All_posterior<-bind_rows(posterior_coral,
                         posterior_algae,
                         #posterior_fish,
                         posterior_Corallivore,
                         posterior_dead_coral_fish,
                         posterior_N,
                         posterior_Rd,
                         posterior_Pmax,
                         #posterior_sst,
                         #posterior_NEC,
                         posterior_NEP,
                        # posterior_C,
                         posterior_temp,
                        # posterior_GP,
                         posterior_Nwater,
                       #  posterior_Pwater
                        )

# make a plot showing the change in each parameter over time
# Get the default NPG palette colors
npg_colors <- pal_npg("nrc")(10) # Default NPG palette has 10 colors

# Create a color ramp function to extend the palette to 11 colors
# This will interpolate between the existing NPG colors to generate an 11th color
npg_extended_palette_function <- colorRampPalette(npg_colors)

# Generate 11 colors from the extended palette
npg_11_colors <- npg_extended_palette_function(12)

All_posterior %>%
  ggplot(aes(x = Year, y = fct_reorder(Parameter, Year, mean), 
             fill = Parameter))+
  geom_vline(xintercept = 0)+
  geom_density_ridges(alpha = 0.5, color = NA)+
  scale_fill_manual(values = npg_11_colors)+
  annotate("text",x = 0.1, y = 0.75, label = "Increasing over time")+
  annotate("text",x = -0.1, y = 0.75, label = "Decreasing over time")+
  labs(x = "Standardized change per year",
       y = "")+
  lims(x = c(-0.3,0.3))+
  theme_ridges()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12))

ggsave(here("Output","ChangePosterior.pdf"), height = 6, width = 10)

### make a plot showing the bulk % change from start to end of timeseries

## tricky because the timeseries is different for each variable

get_first_non_na_per_column <- function(tbl) {
  # Apply a function to each column of the tibble
  # The function finds the index of the first non-NA value
  # and then extracts that value from the column.
  # If a column contains only NAs, NA is returned for that column.
  sapply(tbl, function(col) {
    first_non_na_index <- which(!is.na(col))[1]
    if (is.na(first_non_na_index)) {
      NA  # Return NA if no non-NA value is found
    } else {
      col[first_non_na_index]
    }
  })
}

get_last_non_na_per_column <- function(data_tibble) {
  # Ensure the input is a tibble
  if (!inherits(data_tibble, "tbl_df")) {
    stop("Input must be a tibble.")
  }
  
  # Apply a function to each column to find the last non-NA value
  last_values <- purrr::map(data_tibble, function(col) {
    non_na_values <- col[!is.na(col)]
    if (length(non_na_values) > 0) {
      return(tail(non_na_values, 1))
    } else {
      return(NA) # Return NA if all values in the column are NA
    }
  })
  
  # Convert the list to a named vector
  unlist(last_values)
}

# get the first value
first<-get_first_non_na_per_column(Year_Averages %>%
                              select(NP_mean, #GP_mean, 
                                     mean_fleshy,
                                     mean_coral, NEC_mean_Day, Pmax, Rd,
                                     N_percent, C_percent, #mean_biomass,
                                     fish_dead_coral, Corallivore, Max_temp,
                                     Nitrite_and_Nitrate #, Phosphate
                                     ))
first<-as_tibble(first) %>%
  mutate(Params = names(first)) %>%
  rename(first = value)


last<-get_last_non_na_per_column(Year_Averages %>%
                              select(NP_mean, #GP_mean, 
                                     mean_fleshy,
                                     mean_coral, NEC_mean_Day, Pmax, Rd,
                                     N_percent, C_percent, #mean_biomass, 
                                     fish_dead_coral, Corallivore, Max_temp,
                                     Nitrite_and_Nitrate, #Phosphate
                                     ))

last<-as_tibble(last) %>%
  mutate(Params = names(last))%>%
  rename(last = value)

## average first 5 years and last 5 years to remove effect of anomalous 
first5<- Year_Averages%>%
  filter(Year %in% c(2008:2012)) %>%
  select(NP_mean, GP_mean, mean_fleshy,
         mean_coral, NEC_mean_Day, Pmax, Rd,
         N_percent, C_percent, mean_biomass,Nitrite_and_Nitrate, Phosphate, Max_temp)%>%
  pivot_longer(cols = NP_mean:Max_temp) %>%
  group_by(name) %>%
  summarise(first = mean(value, na.rm = TRUE))

last5<- Year_Averages%>%
  filter(Year %in% c(2019:2024)) %>%
  select(NP_mean, GP_mean, mean_fleshy,
         mean_coral, NEC_mean_Day, Pmax, Rd,
         N_percent, C_percent, mean_biomass,Nitrite_and_Nitrate, Phosphate, Max_temp)%>%
  pivot_longer(cols = NP_mean:Max_temp) %>%
  group_by(name) %>%
  summarise(last = mean(value, na.rm = TRUE)) 

# calculate the percent change in the variables
Per_change_var <- first %>%
  left_join(last) %>%
  mutate(percent_change = (last-first)/first*100) %>%
  mutate(percent_change = ifelse(Params == "NP_mean", -percent_change, percent_change)) %>%
  arrange(desc(percent_change))%>%
  mutate(nicenames = case_when(Params== "mean_fleshy" ~ "% Macroalgae Cover",
                               Params== "NP_mean" ~ "Net Ecosystem Production",
                               Params== "Max_temp" ~ "Max Temperature",
                               Params== "mean_biomass"~ "Fish Biomass",
                               Params== "fish_dead_coral"~ "Hebivore/bioeroding fish",
                               Params == "Corallivore"~"Corallivore",
                               Params== "C_percent" ~ "% C Content",
                               Params== "Pmax" ~ "Maximum Photosynthetic Capacity",
                               Params== "GP_mean" ~ "Gross Ecosystem Production",
                               Params== "N_percent"~ "%N Content",
                               Params== "Rd" ~ "Ecosystem Respirataion",
                               Params== "NEC_mean_Day" ~ "Net Ecosystem Calcification",
                               Params== "mean_coral" ~ "% Coral Cover",
                               Params == "Nitrite_and_Nitrate" ~"Nitrate + Nitrite",
                               Params == "Phosphate" ~ "Phosphate")
  )


## Make a lillipop plot

## What are the number of years with data from each group
Num_years<-Year_Averages %>%
  select(NP_mean, GP_mean, mean_fleshy,
         mean_coral, NEC_mean_Day, Pmax, Rd,
         N_percent, C_percent, mean_biomass,Corallivore, fish_dead_coral, Max_temp,
         Nitrite_and_Nitrate, Phosphate) %>%
  summarise_all(.funs = function(x){sum(!is.na(x))}) %>%
  pivot_longer(NP_mean:Phosphate) %>%
  rename(Params = name,
         N = value)

Per_change_var %>%
  left_join(Num_years) %>%
  mutate(nicenames = factor(nicenames, levels = c("Hebivore/bioeroding fish",
                                                  "% Macroalgae Cover",
                                                  "Fish Biomass",
                                                  "Net Ecosystem Production",
                                                  "Max Temperature",
                                                  "Gross Ecosystem Production",
                                                  "Phosphate",
                                                  "Maximum Photosynthetic Capacity",
                                                  "Net Ecosystem Calcification",
                                                  "Ecosystem Respirataion",
                                                  "Corallivore",
                                                  "% Coral Cover",
                                                  "%N Content",
                                                  "% C Content",
                                                  "Nitrate + Nitrite"
    
  ))) %>%
  mutate(color = ifelse(percent_change>0, "pos", "neg"))%>%
  ggplot(aes(x = percent_change, y = fct_rev(nicenames)))+
  geom_segment(aes(x = 0, xend = percent_change, yend = fct_rev(nicenames)), size = 1.1)+
  geom_point(aes(x = percent_change, color = color), size = 4)+
  geom_vline(xintercept = 0)+
  geom_text(aes(x = 180, y = nicenames, label = N), size = 6)+
  scale_color_manual(values = c("#B22222","#22B2B2"))+
  labs(x = "Percent change over collected time series (%)",
       y = "")+
  theme_minimal()+
  lims(x = c(-100,185))+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14))

ggsave(here("Output","lollipop.pdf"), height = 6, width = 8)  
## Make a correlation plot of all the data we are interested in

cor_mat <- rstatix::cor_mat(Year_Averages %>% select(N_percent, C_percent, mean_coral,
                                                     mean_fleshy, NEC_mean_Day,
                                                     Max_temp, Rd, Pmax, 
                                                    # mean_biomass,
                                                     fish_dead_coral, Corallivore, 
                                                     NP_mean, #GP_mean, 
                                                    mean_coral, mean_fleshy ),
                            method = "pearson")

cor_p   <- rstatix::cor_pmat(Year_Averages%>% select(N_percent, C_percent, mean_coral,
                                                mean_fleshy, NEC_mean_Day,
                                                Max_temp, Rd, Pmax,
                                                #mean_biomass, 
                                                fish_dead_coral, Corallivore, 
                                                NP_mean, #GP_mean,
                                                mean_coral, mean_fleshy ),
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
  theme(panel.grid.major.x  = element_blank())
 
ct <- corr.test(Year_Averages %>% 
                  mutate(log_fleshy = log(mean_fleshy),
                         log_fish = log(mean_biomass),
                         total_NEC = NEC_mean_Day+NEC_mean_Night)%>%
                  select( NP = NP_mean, #GP = GP_mean,
                          Rd, Pmax,
                                          #NEC=NEC_mean_Day,
                                          `%N`=N_percent, 
                                          #`%C`=C_percent, 
                                          `N+N`=Nitrite_and_Nitrate, 
                                         # PO = Phosphate, 
                                          `% Coral`=log_coral,
                                         `% Algae`= log_fleshy, 
                                        # `Fish` = log_fish, 
                                          `Herbs/eroder` = fish_dead_coral,
                                          `Corallivore` = Corallivore,
                                         `Max Temp`= Max_temp,
                                          #`Current Speed` = Flow_mean
                                        ), adjust = "none")
corr <- ct$r
p.mat <- ct$p

ggcorrplot.mixed(corr, 
                 upper = "ellipse", 
                 lower = "number", 
                 p.mat = p.mat, 
                insig = "label_sig", 
                sig.lvl = c(0.05, 0.01, 0.001))

ggsave(here("Output","correlations.pdf"), width = 10, height = 10)
## Use a GAM to test how different variables affect ER while controlling for the underlying time trend
model_ER_full <- gam(log(Rd) ~ s(Year, k=5) + s(log(mean_coral), k=5)  + 
                       s(Temperature_mean, k=5) , 
                     data = Year_Averages, method = "REML")
summary(model_ER_full)
gratia::draw(model_ER_full, residuals = TRUE)

model_Pmax_full <- gam(log(Pmax) ~ s(Year, k=5) + s(log(mean_coral), k=5) +
                       s(Temperature_mean, k=5), 
                     data = Year_Averages, method = "REML")
summary(model_Pmax_full)
gratia::draw(model_Pmax_full, residuals = TRUE)

# --- Hypothesis 1: ER drives Nutrient Recycling (%N) ---
# Model: N_Turbinaria ~ s(Year) + s(ER)
# s(Year) accounts for the long-term trend, so the effect of s(ER) is independent of that trend.
# We use k=5 to limit the complexity of the smooth, which is wise for small N.
model_N <- gam(N_percent ~  s(coral), data = Year_Averages, method = "REML")
# Check the summary. Look for a low p-value (<0.05) for s(ER).
summary(model_N)

# Visualize the partial effects using gratia::draw()
plot_N <- gratia::draw(model_N, residuals = TRUE) &
  #labs(title = "Partial Effects on N in Turbinaria") +
  theme_bw()

## Run a Bayesian SEM to see how the different parameters are related
# First detrend the data

sem_data <- Year_Averages  %>% 
  drop_na(mean_coral) %>%
  mutate(log_fleshy = log(mean_fleshy),
         log_alive = log(mean_alive),
         log_fish = log(mean_biomass),
         log_rd = log(Rd),
         log_pmax = log(Pmax))%>%
    select(mean_coral, mean_fleshy, Pmax, GP_mean, Rd, NEC_mean_Day, N_percent,
           Flow_mean, Max_temp, mean_alive, log_coral, 
           mean_biomass,log_fish,fish_dead_coral, Corallivore,
           log_fleshy, log_alive, C_percent, log_pmax, log_rd) %>%
#  mutate(across(everything(), 
#                ~ residuals(gam(.x ~ s(Year, k=3), data = Year_Averages, na.action = na.exclude)))) %>%
  mutate(across(everything(), # scale the data
                ~as.numeric(scale(.x)))) 
### Use the detrended data --this model makes the most sense right now


# --- 2. Define the Expanded SEM Syntax ---
# This model is much more detailed and theoretically rich.
bsem_model_full_syntax <- '
  log_coral ~ l1*Max_temp
  
  # LEVEL 2: Metabolic Rate Models
  # GPP is driven by the producers (corals, algae) and abiotic factors
  log_pmax ~ c1*log_coral +c3*Max_temp
  # ER is driven by all respiring organisms and abiotic factors (removed r2* log fish)
  log_rd ~ r1*log_coral +r3*Max_temp

  # Community models:
  fish_dead_coral~f1*log_fleshy
  Corallivore~f2*log_coral
  
  # LEVEL 3: Ecosystem Function Models (Original Hypotheses)
  # Nutrient recycling is driven by mean coral cover
  N_percent  ~ n1*log_rd
  # Calcification is driven by calcifiers and gross photosynthesis
#  NEC_mean_Day  ~ nc2*GP_mean 

  # COVARIANCES: Allow unexplained parts of external drivers to correlate
 #Temperature_mean ~~ Flow_mean
 log_pmax ~~ log_rd
 log_coral~~log_fleshy
  # N_percent~~C_percent
  
  # Indirect effects 
  # coral to percent N via ER
  #coral_ER_N:=n1*r1
  #Temperature to Rd via coral -- compare to c3 direct effect of temp on Rd
 # Temp_to_R:=l1*r1
#  Temp_to_Pmax:=l1*c1
#  Temp_to_N:=l1*n1
'

# --- 3. Fit the Full Bayesian SEM ---

### Need to standardize these still by centering the residuals above
#my_prior <- set_prior("normal(0, 2)", class = "b")

bsem_fit_full <- bsem(
  model = bsem_model_full_syntax,
  data = sem_data,
   # Year_Averages  %>% 
  #  drop_na(mean_coral) %>%
   # mutate(log_fleshy = log(mean_fleshy),
    #       log_alive = log(mean_alive),
     #      log_fish = log(mean_biomass)),
  n.chains = 3,
  sample = 10000,      # Increased iterations for a more complex model
  burnin = 5000,
  std.lv = TRUE,
  seed = 11,
  em.h1.iter.max = 5000,
  control = list(adapt_delta = 0.99, max_treedepth = 18),
  dp = dpriors(alpha = "normal(0,3)", beta = "normal(0,2)", 
               rho = "beta(1,1)", psi = "gamma(1,0.5)[sd]")
)

# https://www.martinmodrak.cz/2018/02/19/taming-divergences-in-stan-models/
# --- 4. Interpret and Visualize ---
summary(bsem_fit_full, fit.measures = TRUE, ci = TRUE)


# To run posterior predictive checks on the fit indices 'chisq' and 'srmr'
ppmc_res <- ppmc(bsem_fit_full, thin = 10, fit.measures = c("chisq", "srmr"))
# Plot the results
plot(ppmc_res)

plot(bsem_fit_full, plot.type = "dens")

# prepare a null model
model_null <- '
log_coral ~~ log_coral 
Max_temp ~~ Max_temp 
log_pmax ~~ log_pmax
log_rd ~~ log_rd
log_fleshy ~~ log_fleshy
fish_dead_coral ~~ fish_dead_coral
Corallivore ~~ Corallivore
N_percent ~~ N_percent
'
bsem_null <- bsem(
  model = model_null,
  data = sem_data,
  # Year_Averages  %>% 
  #  drop_na(mean_coral) %>%
  # mutate(log_fleshy = log(mean_fleshy),
  #       log_alive = log(mean_alive),
  #      log_fish = log(mean_biomass)),
  n.chains = 3,
  sample = 10000,      # Increased iterations for a more complex model
  burnin = 5000,
  std.lv = TRUE,
  seed = 11,
  em.h1.iter.max = 5000,
  control = list(adapt_delta = 0.99, max_treedepth = 18),
  dp = dpriors(alpha = "normal(0,3)", beta = "normal(0,2)", 
               rho = "beta(1,1)", psi = "gamma(1,0.5)[sd]")
)

# compare to null model
gl_fits_all <- blavFitIndices(bsem_fit_full, baseline.model = bsem_null, rescale = "MCMC")
summary(gl_fits_all, central.tendency = c("mean","median","mode"), prob = .90)

# Extract posterior samples
mcmc_samples <- as.matrix(blavInspect(bsem_fit_full, 'mcmc'))
# Plot the posterior areas for a selection of parameters
mcmc_areas(mcmc_samples, pars = c("l1","c1","c3","r1","r3","f1","f2","n1","log_coral~~log_fleshy","log_pmax~~log_rd"), prob = 0.95, prob_outer = 0.95)

mcmc_data<-as_tibble(mcmc_samples)

mcmc_data %>% 
  ggplot(aes(x = l1))+ 
  stat_halfeye(point_interval=median_hdi, .width=c(.95, .75), 
               fatten_point = 2, slab_alpha = 0.6, fill = scales::alpha("#009E73",0.3)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() + 
  xlab("")+
  ylab("")+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA),
        #  text=element_text(size=16,  family="sans"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

blavPredict(bsem_fit_full)

# Get effects for your blavaan model (e.g., a latent variable)
preds <- ggpredict(bsem_fit_full, terms = c("log_coral","log_rd"), type = "fixed")
plot(preds)
# 
# # Plot the path diagram for the fitted model
# semPaths(bsem_fit_full, what = "est", 
#          layout = "spring", edge.label.cex = 1.2, 
#          intercepts = FALSE, residuals = FALSE)
########## STOPPED HERE ###############



# The path diagram will be more complex, so a different layout might be better.
# semPaths(bsem_fit_full, 
#          what = "est",
#          whatLabels = "est",
#          layout = "spring", # "spring" layout handles web-like structures well
#          edge.label.cex = 1.1,
#          fade = TRUE,
#          residuals = FALSE, # Show residuals to see unexplained variance
#          nCharNodes = 0,
#          sizeMan = 10,
#          style = "lisrel",
#          title = TRUE,
#          main = "Expanded Bayesian SEM of Reef Ecosystem Drivers")

### SImple model with SST driving coral and coral driving %N and %N driving ER

## Lagoon regression says %N from T and water column N are tightly correlated.. 
# bring in the relationship to highlight the expected decline in water column N concentration
# pair with Linda's loss of N




############### A BRMS example #############
# get standardized SEM data
semdata2<-Year_Averages %>%
  mutate( logfleshy = log(mean_fleshy),
          logfish = log(mean_biomass),
          logrd = log(Rd),
          logpmax = log(Pmax),
          Rd =-R_mean,
          NEC = NEC_mean_Day+NEC_mean_Night)%>%
  select(Year,meancoral = mean_coral, 
         meanfleshy = mean_fleshy,
         temperature = Max_temp,
         flow = Flow_mean,
         Npercent = N_percent,
         fish = mean_biomass,
         GP = GP_mean,
         logrd,logpmax,
         logcoral = log_coral,
         logfleshy, logfish, Corallivore, herbs = fish_dead_coral
        ) %>%
  drop_na(logcoral) %>%
  mutate(across(everything(), # scale the data
                ~as.numeric(scale(.x)))) 


# rename for brms
sem_data2 <-sem_data %>%
  rename(logcoral = log_coral,
         logalive = log_alive,
         logfleshy = log_fleshy,
         MaxTemp = Max_temp,
         Npercent = N_percent,
         logfish = log_fish)
# make sure mgcv is installed for smooths
# install.packages("mgcv")

# ----------------------------
# SEM with smooth terms + mi()
# ----------------------------

# imputation for missing temp data
bf_temp <- bf(
  temperature | mi() ~ 1 + Year
)
# temp drives coral cover
bf_coraltemp <- bf(
  logcoral | mi() ~ 1 + mi(temperature)
)

# LEVEL 2: Metabolic Rate Models
# GPP is driven by the producers (corals, algae) and abiotic factors
bf_logpmax <- bf(
  logpmax | mi() ~ 1 +logcoral+ mi(temperature)
)

# ER is driven by all respiring organisms and abiotic factors (removed r2* log fish)

bf_logrd <- bf(
  logrd | mi() ~ 1 +logcoral+ mi(temperature)
)
# Community models:
bf_herbs <- bf(
  herbs | mi() ~ 1 +logfleshy
)

bf_corallivore <- bf(
  Corallivore | mi() ~ 1 +logcoral
)


# LEVEL 3: Ecosystem Function Models (Original Hypotheses)
# Nutrient recycling is driven by mean coral cover to Rd
bf_nutrients<- bf(
  Npercent | mi() ~ 1 +mi(logrd)
)


# COVARIANCES: Allow unexplained parts of external drivers to correlate
#Temperature_mean ~~ Flow_mean
#log_pmax ~~ log_rd
#log_coral~~log_fleshy

# algae and coral are correlated


# ----------------------------
# Auxiliary imputation models
# (intercept-only is fine to start; you can also add covariates/smooths here)
# ----------------------------
bf_temp    <- bf(temperature | mi() ~ 1)
bf_flow    <- bf(flow        | mi() ~ 1)
bf_coral   <- bf(meancoral       | mi() ~ 1)
bf_fleshy  <- bf(meanfleshy      | mi() ~ 1)
bf_biomass <- bf(fish     | mi() ~ 1)

# ----------------------------
# Fit
# ----------------------------
brms_sem_full <- brm(
  bf_temp+bf_coraltemp+bf_logpmax + bf_logrd + bf_herbs +bf_corallivore+bf_nutrients + bf_fleshy+
  set_rescor(TRUE),             # residual correlations among responses
  data = semdata2,
  chains = 3,
  iter = 10000,
  warmup = 5000,
  seed = 11,
  sample_prior = "no",
  prior = c(set_prior("normal(0, 2)", class = "b"),
            set_prior("normal(0, 0.25)", resp = "temperature", class = "sigma", lb = 0),
            set_prior("normal(0, 2)", class = "Intercept")),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  family = gaussian()
)

summary(brms_sem_full) # Rhat and ESS good for everything
# look at the trace plots
plot(brms_sem_full) # looks good
# look at PP checks (looks good)
pp_check(brms_sem_full, resp = "temperature")
pp_check(brms_sem_full, resp = "herbs")
pp_check(brms_sem_full, resp = "logcoral")
pp_check(brms_sem_full, resp = "Npercent")
pp_check(brms_sem_full, resp = "logpmax")
pp_check(brms_sem_full, resp = "logrd")

# plot all the posteriors
brms_sem_full %>%
  spread_draws(`b.*`, regex = TRUE) %>%
  select(b_temperature_Year, b_logpmax_logcoral,b_logrd_logcoral,b_herbs_logfleshy,
         b_Corallivore_logcoral, bsp_logcoral_mitemperature, bsp_logpmax_mitemperature,
         bsp_logrd_mitemperature, bsp_Npercent_milogrd) %>%
  pivot_longer(cols = b_temperature_Year:bsp_Npercent_milogrd)%>%
  ggplot(aes(x = value, y = name))+
  stat_halfeye(point_interval=median_hdi, .width=c(.95, .75), 
               fatten_point = 2, slab_alpha = 0.6, fill = scales::alpha("#009E73",0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
#  facet_wrap(~name, ncol = 1)+
  theme_bw() + 
  xlab("")+
  ylab("")+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA),
      #  axis.text.y = element_blank(),
       # axis.ticks.y = element_blank()
        )

# get the nonscaled values for the model
Yearmodelvalues<-Year_Averages %>%
  mutate( logfleshy = log(mean_fleshy),
          logfish = log(mean_biomass),
          logrd = log(Rd),
          logpmax = log(Pmax),
          Rd =-R_mean,
          NEC = NEC_mean_Day+NEC_mean_Night)%>%
  select(Year,meancoral = mean_coral, 
         meanfleshy = mean_fleshy,
         temperature = Max_temp,
         flow = Flow_mean,
         Npercent = N_percent,
         fish = mean_biomass,
         GP = GP_mean,
         logrd,logpmax,
         logcoral = log_coral,
         logfleshy, logfish, Corallivore, herbs = fish_dead_coral
  ) %>%
  drop_na(logcoral)

# get the means and SD to unscale the plots
modelmean<-Yearmodelvalues%>%
  mutate(across(everything(), # scale the data
                ~as.numeric(mean(.x, na.rm = TRUE)))) %>%
  distinct()

modelsd<-Yearmodelvalues%>%
  mutate(across(everything(), # scale the data
                ~as.numeric(sd(.x, na.rm = TRUE)))) %>%
  distinct()

# set the theme for everything
theme_regression<-
  theme_bw()+
  theme(text = element_text(size = 14, family = "Arial"),
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA)
  )

theme_set(theme_regression) 

coral_temp<-conditional_effects(brms_sem_full, resp = "logcoral", effects = "temperature") 
ct_plot<-as_tibble(coral_temp$logcoral.logcoral_temperature) %>%
  mutate(temperature = temperature*modelsd$temperature+modelmean$temperature,
         estimate__ = exp(estimate__*modelsd$logcoral+modelmean$logcoral),
         lower__ = exp(lower__*modelsd$logcoral+modelmean$logcoral),
         upper__ = exp(upper__*modelsd$logcoral+modelmean$logcoral))  %>% # unscale the data
  ggplot(aes(x = temperature, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = Max_temp, y = exp(log_coral)), alpha = 0.5)+
  coord_trans(y = "log")+
  scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(x = expression("Temperature ("*degree*"C)"),
       y = "Coral Cover (%)")
  
  ### Temperature Year
temp_year<-conditional_effects(brms_sem_full, resp = "temperature", effects = "Year") 
ty_plot<-as_tibble(temp_year$temperature.temperature_Year) %>%
  mutate(Year = Year*modelsd$Year+modelmean$Year,
         estimate__ = estimate__*modelsd$temperature+modelmean$temperature,
         lower__ = lower__*modelsd$temperature+modelmean$temperature,
         upper__ = upper__*modelsd$temperature+modelmean$temperature)  %>% # unscale the data
  ggplot(aes(x = Year, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = Year, y = Max_temp), alpha = 0.5)+
  #coord_trans(y = "log")+
 # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(y = expression("Temperature ("*degree*"C)"),
       x = "Year")


# log Pmax ~ coral and temperature
pmax_coral<-conditional_effects(brms_sem_full, resp = "logpmax", effects = "logcoral") 
pc_plot<-as_tibble(pmax_coral$logpmax.logpmax_logcoral) %>%
  mutate(logcoral = exp(logcoral*modelsd$logcoral+modelmean$logcoral),
         estimate__ = exp(estimate__*modelsd$logpmax+modelmean$logpmax),
         lower__ = exp(lower__*modelsd$logpmax+modelmean$logpmax),
         upper__ = exp(upper__*modelsd$logpmax+modelmean$logpmax) ) %>% # unscale the data
  ggplot(aes(x = logcoral, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = exp(log_coral), y = Pmax), alpha = 0.5)+
  coord_trans(x = "log", y = "log")+
  # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(y = expression("Pmax (mmol O"[2]*" m"^2*" hr"^-1*")"),
       x = "Coral (%)")

# log Rd ~ coral and temperature
rd_coral<-conditional_effects(brms_sem_full, resp = "logrd", effects = "logcoral") 
rc_plot<-as_tibble(rd_coral$logrd.logrd_logcoral) %>%
  mutate(logcoral = exp(logcoral*modelsd$logcoral+modelmean$logcoral),
         estimate__ = exp(estimate__*modelsd$logrd+modelmean$logrd),
         lower__ = exp(lower__*modelsd$logrd+modelmean$logrd),
         upper__ = exp(upper__*modelsd$logrd+modelmean$logrd) ) %>% # unscale the data
  ggplot(aes(x = logcoral, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = exp(log_coral), y = Rd), alpha = 0.5)+
  coord_trans(x = "log", y = "log")+
  # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(y = expression("Ecosystem Respiration (mmol O"[2]*" m"^2*" hr"^-1*")"),
       x = "Coral (%)")

# herbs ~ log fleshy
herbs_fleshy<-conditional_effects(brms_sem_full, resp = "herbs", effects = "logfleshy") 
hf_plot<-as_tibble(herbs_fleshy$herbs.herbs_logfleshy) %>%
  mutate(logfleshy = exp(logfleshy*modelsd$logfleshy+modelmean$logfleshy),
         estimate__ = estimate__*modelsd$herbs+modelmean$herbs,
         lower__ = lower__*modelsd$herbs+modelmean$herbs,
         upper__ = upper__*modelsd$herbs+modelmean$herbs ) %>% # unscale the data
  ggplot(aes(x = logfleshy, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = mean_fleshy, y = fish_dead_coral), alpha = 0.5)+
  coord_trans(x = "log")+
  # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(y = expression("Herbivore Fish Biomass (g m "^-1*")"),
       x = "Fleshy Macroalgae (%)")

# corallivore ~ corals
corallivore_coral<-conditional_effects(brms_sem_full, resp = "Corallivore", effects = "logcoral") 
cc_plot<-as_tibble(corallivore_coral$Corallivore.Corallivore_logcoral) %>%
  mutate(logcoral = exp(logcoral *modelsd$logcoral +modelmean$logcoral ),
         estimate__ = estimate__*modelsd$Corallivore+modelmean$Corallivore,
         lower__ = lower__*modelsd$Corallivore+modelmean$Corallivore,
         upper__ = upper__*modelsd$Corallivore+modelmean$Corallivore ) %>% # unscale the data
  ggplot(aes(x = logcoral , y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = exp(log_coral), y = Corallivore), alpha = 0.5)+
  coord_trans(x = "log")+
  # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(y = expression("Corallivore Fish Biomass (g m "^-1*")"),
       x = "Coral Cover (%)")

# N% ~ respiration
N_Rd<-conditional_effects(brms_sem_full, resp = "Npercent", effects = "logrd") 
nr_plot<-as_tibble(N_Rd$Npercent.Npercent_logrd) %>%
  mutate(logrd = exp(logrd*modelsd$logrd+modelmean$logrd),
         estimate__ = estimate__*modelsd$Npercent+modelmean$Npercent,
         lower__ = lower__*modelsd$Npercent+modelmean$Npercent,
         upper__ = upper__*modelsd$Npercent+modelmean$Npercent ) %>% # unscale the data
  ggplot(aes(x = logrd, y = estimate__))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin =lower__, ymax =  upper__), alpha = 0.25, fill = "#67bed9")+
  geom_point(data = Year_Averages, aes(x = Rd, y = N_percent), alpha = 0.5)+
  coord_trans(x = "log", y = "log")+
  # scale_y_continuous(breaks = c(1,5,10,20,50,100))+
  labs(x = expression("Ecosystem Respiration (mmol O"[2]*" m"^2*" hr"^-1*")"),
       y = "Nitrogen Content (%)")


nr_plot/rc_plot/hf_plot/cc_plot/ct_plot/ty_plot

#########
gp_edges<-as_tibble(fixef(brms_sem_full)) %>%
  mutate(coef =rownames(fixef(brms_sem_full))) %>%
  separate(col = coef, into = c("resp","pred"))%>%
  filter(pred != "Intercept") %>%
  rename(est = Estimate, lo = `Q2.5`, hi = `Q97.5`)%>%
  mutate(sig = ifelse(sign(lo)==sign(hi),1,0.5)) # determine significance

gp_edges %>%
  ggplot(aes(x = est, y = pred))+
  geom_point(size = 3, color = "firebrick", aes(alpha = sig))+
  geom_errorbarh(aes(xmin = lo, xmax = hi,alpha = sig), height = 0, color = "firebrick")+
  geom_vline(xintercept = 0)+
  scale_alpha(range = c(0.25,1))+
  labs(x = "Standardized Effect Size",
       y = "Predictor Variables")+
  theme_minimal()+
  facet_wrap(~resp, nrow = 1)+
  theme(text = element_text(size = 14),
        legend.position = "none")


## get the correlated errors
a<-summary(brms_sem_full) 

cor_res<-as_tibble(a$rescor_pars) %>%
  mutate(coef = rownames(a$rescor_pars)) %>%
  mutate(coef = str_remove_all(coef, "[rescor()]")) %>%
  separate(coef, into = c("x","y"), sep = ",")

cor_res %>%
  ggplot(aes(x=x, y=y, fill = Estimate))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "pink")+
  theme_minimal()
