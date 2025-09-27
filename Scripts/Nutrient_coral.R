

library(here)
library(tidyverse)

NData<-read_csv(here("Data","MCR_LTER_Macroalgal_CHN_2005_to_2024_20250616.csv"))

N_summary<-NData %>%
  filter(Site == "LTER_1",
         Habitat == "Backreef",
         Genus != "Sargassum") %>%
  mutate(N_percent = N/Dry_Weight*100,
         C_percent = C/Dry_Weight*100) %>%
  group_by(Year) %>%
  summarise(mean_N = mean(N_percent, na.rm = TRUE),
            se_N = sd(N_percent)/sqrt(n()),
            mean_C = mean(C_percent, na.rm = TRUE),
            se_C = sd(C_percent)/sqrt(n()),
            mean_CN = mean(CN_ratio, na.rm = TRUE),
            se_CN = sd(CN_ratio, na.rm = TRUE)) %>%
  full_join(TotalLiving %>%
              filter(Site == "LTER 1"))


p1<-N_summary %>%
  ggplot(aes(x = log10(mean_coral), y = mean_CN))+
  geom_point()+
#  coord_trans(x = "log")+
 # geom_errorbar(aes(ymin = mean_CN-se_CN, ymax = mean_CN+se_CN), width = 0.1)+
  geom_smooth(method = "lm")+
  labs(x = "log10 % coral cover",
       y = "mean C:N of Turbinaria")+
  theme_bw()


p2<-N_summary %>%
  ggplot(aes(x = Year, y = mean_CN))+
  geom_point()+
  #  coord_trans(x = "log")+
  # geom_errorbar(aes(ymin = mean_CN-se_CN, ymax = mean_CN+se_CN), width = 0.1)+
  geom_smooth(method = "lm")+
  labs(x = "Year",
       y = "mean C:N of Turbinaria")+
  theme_bw()


p1|p2
ggsave(here("Output","coral_Npercent.png"), width = 8, height = 5)


N_summary %>%
  ggplot(aes(x = log10(mean_coral), y = mean_N))+
  geom_point()+
 # geom_errorbar(aes(ymin = mean_N-se_N, ymax = mean_N+se_N), width = 0.1)+
  geom_smooth(method = "lm")


N_summary %>%
  ggplot(aes(x = Year, y = mean_N))+
  geom_point()+
 # geom_errorbar(aes(ymin = mean_N-se_N, ymax = mean_N+se_N), width = 0.1)+
  geom_smooth(method = "lm")



N_summary %>%
    ggplot(aes(x = mean_fleshy, y = mean_C))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_C-se_C, ymax = mean_C+se_C), width = 0.1)+
  geom_smooth(method = "lm")
