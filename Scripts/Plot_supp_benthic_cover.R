### Supplemental Benthic Cover


### LOAD LIBRARIES
library(tidyverse)
library(here)
library(googlesheets4)
library(lubridate)
library(patchwork)



### READ IN DATA
# Benthic Cover data
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')
# Patches are dispersed around reef, unrelated to upstream and downstream and surveyed radially

# CHN Turbinaria
CHN<-read_sheet("https://docs.google.com/spreadsheets/d/18BCnI9u5h4pOQ3n0RCT00TSRcqvBYsP7OQsmFmAGjac/edit?usp=sharing")

## Temperature LTER 1
Temp_LTER1<-read_sheet("https://docs.google.com/spreadsheets/d/1YJRyNAeo03xLoThBGX2-BtHpWqUpYXIarmYTsIhw5aI/edit?usp=sharing")




### CLEAN DATA

# benthic
BenthicCover <- BenthicCover %>% 
  mutate_at(.vars = vars(coral:ctb), .funs = as.numeric)

BC_edit <- BenthicCover %>% 
  mutate_at(vars(coral:ctb), .funs=as.numeric) 

mean_BC <- BC_edit %>% 
  filter(Site == "LTER 1") %>% 
  group_by(Year) %>% # not by patch
  summarise_at(.vars = vars(coral:ctb), .funs = c(mean), na.rm = TRUE) %>% 
  pivot_longer(cols = coral:ctb, names_to = "benthic", values_to = "mean")

se_BC <- BC_edit %>% 
  filter(Site == "LTER 1") %>% 
  group_by(Year) %>% # not by patch
  summarise_at(.vars = vars(coral:ctb), .funs = c(plotrix::std.error), na.rm = TRUE) %>% 
  pivot_longer(cols = coral:ctb, names_to = "benthic", values_to = "SE")


# dataframe with mean and standard error values of percent cover of coral and macroalgae per year
sum_BC <- full_join(mean_BC, se_BC)


### PLOT BENTHIC DATA TIMESERIES ###

rename <- c(coral = "Coral", ctb = "CCA + Turf", macroalgae = "Macroalgae",
            millepora = "Millepora", sand = "Sand")

## LINE PLOT
sum_BC %>% 
  drop_na(mean, SE)%>%
  ggplot(aes(x=Year, 
             y=mean)) + 
  facet_wrap(~benthic, scales = "free_y", labeller=as_labeller(rename)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = Year, ymin = mean-SE, ymax = mean+SE),
                width = 0.1)+
  geom_vline(xintercept=vertical.lines, color="coral2", linetype="dashed") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size=10, face="bold"))


anova(lm(data = summaryBC, coral ~ poly(Year,2)))
anova(lm(data = summaryBC, ctb ~ poly(Year,2)))
anova(lm(data = summaryBC, macroalgae ~ Year))
anova(lm(data = summaryBC, millepora ~ poly(Year,2)))
anova(lm(data = summaryBC, sand ~ poly(Year,2)))


## STACKED BAR PLOT

myPal <- c(coral = "#c38370", macroalgae = "#9da993", millepora = "#bdc3cb", ctb = "#523a28", sand = "#d6ad60")


BC_plot <- sum_BC %>% 
  mutate(benthic = factor(benthic, levels = c("coral", "macroalgae", "millepora", "ctb", "sand"))) %>% 
  ggplot(aes(x = Year, y = mean)) +
  geom_col(aes(fill = benthic), color = "grey") +
  theme_classic() +
  labs(fill = "Substrate",
       y = "Mean benthic % cover") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18)) +
  scale_fill_manual(values = myPal, labels = c("Coral", "Macroalgae", "Millepora", "CCA+Turf", "Sand"))

ggsave(here("Output", "Supplemental_benthic_cover.png"), BC_plot, device = "png", height = 7, width = 7)



