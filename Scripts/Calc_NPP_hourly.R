### calculate raw PP values myself - Note I could not recreate the values so this is wrong


library(tidyverse)
library(here)


# read in the raw PP data

PP_raw<-read_csv(here("Data","PP_raw_timeseries_updated.csv"))

DOsat<-function(S,T){
  
  T=T+273.15 #Celsius to Kelvin
  
  #From chapter 6.4 p.276 from Jordan Clark's reader.  FRAM 11/1/06 which is allegedly from APHA 1985, but is apparently wrong.
  tmp<- -139.34411 + 1.575701 * 1e5 / T - 6.642308 * 1e7 / T^2 + 1.243800 * 1e10 / T^3 - 8.631949 * 1e11 / T^4
  
  DO_fresh<-exp(tmp)
  
  tpm<- -S * (1.7674 * 1e-2-1.0754 * 10 / T + 2.1407 * 1e3 / T ^2)
  
  tmp<-log(DO_fresh) + tpm
  
  clark<-exp(tmp)  #for Moorea at 35 PPT, 28.5 = 5.7 mg/L
  
  # From Weiss 1970
  A1<- -173.4292
  A2<- 249.6339
  A3<- 143.3483
  A4<- -21.8492
  B1<- -0.033096
  B2<- 0.014259
  B3<- -0.001700
  mlL2mgL<- 1.4276
  weiss<- exp(A1 + A2 * (100 / T) + A3 * log(T / 100) + A4 * T / 100 + S *(B1 + B2 * T / 100 + B3 * (T / 100)^2)) * mlL2mgL
  
  #Benson & Krause 1984.
  #Benson and Krause also has Henry's coeff for oxygen
  tmp<- -135.29996 + 1.572288 * 1e5 / T - 6.637149 * 1e7 /T^2 + 1.243678 * 1e10/ T^3 - 8.621061 * 1e11 /T^4
  DO_fresh<- exp(tmp)
  tpm<- -S * (2.0573 * 1e-2 - 12.142 / T + 2.3631 * 1e3 / T^2)
  tmp<- log(DO_fresh) + tpm
  DO<- exp(tmp) * 32 / 1000  #for Moorea at 35 PPT, 28.5 = 6.3 mg/L
  
  DO
}


#Chemical enhancment coefficeint from Banerjee and MacIntyre
alp<-1 

#partial pressure of O2 in air (assuming there is no water vapor in the atmosphere, the pp of oxygen is 20.948 kPa O2 per kPa air or 20.95%)
perO2<-0.2095 

#Kilopascal to mmHg- 1 kPa=7.500617 mmHG
ppO2=760*perO2 

#Molecualr weight of O2
MWO=16 

#Assumed Salinty
Sal=35 
n<-0.5 #Hintsa McGillis Zappa 2004 

#Schmidt number for CO2 at standard temp which corrisponds to the piston velocity we calculated
ScCO2<-600 

#Schmidt number for O2. Diffusivity ratio from CRC at Moorea water temps
ScO2<-400*2.1/2.7 

#Piston velocity of O2
kO2<-kCO2*(ScO2/ScCO2)^(-n) 


#mg/L in Air
#Note this calculation used to be part of the flux calculation. Now it is no longer used
#O2fromAirSide=ppO2/(16.0*MWO*2*ppO2/(273.15 + cels)/DOsat(35,cels)) 


PP_raw<- PP_raw %>%
  mutate(Velocity = (Raw_Speed_UP+Raw_Speed_DN)/2, #average velocity
         Off = Transect_Length/Velocity, #Time it takes in seconds for water to travel from UP to DN
         Depth = (Depth_UP+Depth_DN)/2,
         # Piston velocity from wind. Wind speed is in m/s. k  is in cm/h (Wanninkhof 1992)
         k600wind=0.45*Wind_Speed^1.64,
         #Piston velocity from currents. u water in cm/s, H in m. from currents Borges 2004. The u equation can't deal with negative numbers (^.5 is a square root, and cant take square root of a negative number). Negative numbers will throw a NaN. So changing this to absolute u
         k600water=1.719*((abs(Velocity)*100)^.5)*(Depth^(-.5)),
         #piston velocity from currents. u water in cm/s, H in m. from currents%Borges 2004
         kCO2=k600water+k600wind,
         #Piston velocity of O2
         kO2=kCO2*(ScO2/ScCO2)^(-n),
         meanO2_wat = (DN_Oxy+UP_Oxy)/2,
         cels = (DN_Temp+UP_Temp)/2,
         #Calculate airfulx (transfer of O2 between air and water). DOsat outputs in mg/L. So must convert to umol/L
         airflux=kO2*alp*(meanO2_wat-(DOsat(35,cels)*1000/ 31.998 ))/100,
         corfac=mean(kO2,na.rm=TRUE) * mean(meanO2_wat-DOsat(35,cels),na.rm=TRUE)/10, #SM correction)
         O2_Diff = DN_Oxy - UP_Oxy, # Up - Down ozygen
         VDup = Raw_Speed_UP*(Depth_UP+0.1), # +.1 is to approximate the actual depth and account for the fact the ADP's are sitting .1m off the sea bottom
         VDdn = Raw_Speed_DN*(Depth_DN +0.1),
         P = (VDup+VDdn)*1*(O2_Diff)/(1*Transect_Length*2), # productivity without airflux
         PP = P*3600-(airflux), # account for airflux
         DayNight = case_when(PAR > 10 ~"Day",
                              PAR <= 10 ~"Night",
                              is.na(PAR) ~"NA"),
         Date = as_date(mdy_hm(time_local)),
         time_local = mdy_hm(time_local)
         
  )
         

         
 ## calculate GP and R
Daytime<-PP_raw %>%
  filter(DayNight == "Day")

Nighttime<-PP_raw %>%
  filter(DayNight == "Night")

# get the median nighttime R value to add to daytime NEP to get GP
Average_R<-Nighttime %>%
  group_by(Site,Date)%>%
  summarise(Daily_R = median(PP, na.rm = TRUE))
  
Data_all<-Daytime %>%
  left_join(Average_R)%>%
  mutate(GPP = PP-Daily_R)
         
         
Data_all %>%
  filter(Site == "LTER 1") %>%
  filter(Deployment == 29) %>%
  ggplot(aes(x = time_local, y = PP))+
  geom_line()


Data_all %>%
  filter(Site == "LTER 1") %>%
  filter(Deployment == 29) %>%
  select(PP) %>%
  reframe(MeanPP = mean(PP, na.rm = TRUE))
