

Year_Averages %>%
  ggplot(aes(x = Year, y = Max_temp))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#0072B2",
              fill = "#0072B2")+
  labs(y = "Maximum temperature"~degree~"C")+
  theme_minimal()+
  annotate(x = 2019, y = 28.75, geom = "text",
          label = paste0("Increase in 0.034 ", intToUtf8(176), "C per year"), size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

an<-lm(Max_temp~Year, data = Year_Averages)
summary(an)

ggsave(here("Output","Tempplot.png"), width = 6, height = 6)

mac<-Year_Averages %>%
  drop_na(macroalgae)%>%
  ggplot(aes(x = Year, y = macroalgae))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#0072B2",
             fill = "#0072B2")+
  labs(y = "Macroalgae Cover (%)")+
 # lims(y = c(0,30))+
 # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 5, geom = "text",
           label = "6-fold increase over 20 yrs", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

corl<-Year_Averages %>%
  drop_na(coral)%>%
  ggplot(aes(x = Year, y = coral))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y = "Coral Cover (%)")+
  # lims(y = c(0,30))+
 #  coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2011, y = 3, geom = "text",
           label = "Lost > 2/3 of total coral cover", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

mac/corl

ggsave(here("Output","coverploe.png"), width = 6, height = 12)

fishdead<-Year_Averages %>%
  drop_na(fish_dead_coral)%>%
  ggplot(aes(x = Year, y = fish_dead_coral))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#0072B2",
              fill = "#0072B2")+
  labs(y = expression(paste("Fish Biomass (g m"^-2,") Herbivores/eroders")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 5, geom = "text",
           label = "5-fold increase", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

fishcoral<-Year_Averages %>%
  drop_na(Corallivore)%>%
  ggplot(aes(x = Year, y = Corallivore))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y = expression(paste("Fish Biomass (g m"^-2,") Corallivores")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 1.5, geom = "text",
           label = "12-fold reduction", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

fishdead/fishcoral
ggsave(here("Output","fishplot.png"), width = 6, height = 12)


pmax<-Year_Averages %>%
  drop_na(Pmax)%>%
  ggplot(aes(x = Year, y = Pmax))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y=expression(paste("Max Photosynthetic capacity (mmol O"[2]," m"^-2, " d"^-1,")")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 150, geom = "text",
           label = "~30% reduction", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

Rd<-Year_Averages %>%
  drop_na(Rd)%>%
  ggplot(aes(x = Year, y = Rd))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y=expression(paste("Ecosystem Respiration (mmol O"[2]," m"^-2, " d"^-1,")")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 50, geom = "text",
           label = ">50% reduction", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

pmax/Rd
ggsave(here("Output","metabplot.png"), width = 6, height = 12)

TurbNplot<-Year_Averages %>%
  #drop_na(Rd)%>%
  ggplot(aes(x = Year, y = N_percent))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y=expression(paste("Turbinaria %N")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2019, y = 0.9, geom = "text",
           label = ">50% reduction", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

Nplot<-Year_Averages %>%
  #drop_na(Rd)%>%
  ggplot(aes(x = Year, y = Nitrite_and_Nitrate))+
  geom_point(size= 2, alpha = 0.5)+
  geom_smooth(method = "lm", color = "#D55E00",
              fill = "#D55E00")+
  labs(y=expression(paste("Water Column N+N ("~mu~"mol L"^-1,")")))+
  # lims(y = c(0,30))+
  # coord_trans(y = "log")+
  theme_minimal()+
  annotate(x = 2009, y = 0.2, geom = "text",
           label = ">75% reduction", size = 6)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

TurbNplot/Nplot
ggsave(here("Output","Nplots.png"), width = 6, height = 12)
