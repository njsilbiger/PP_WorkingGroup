# Clean the primary productivity data and calculate GP and R by day and Season
# By Nyssa Silbiger
# Updated on 2/2/2025


##### load libraries #############
library(tidyverse)
library(here)


### Read in the data ######

filedir<-here("Data","QC_PP") # file path for the QC PP files
files<-dir(path = filedir, pattern = ".csv", full.names = TRUE)

# bring in all the PP Data
All_PP_data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") 


# From 2007-2014 the PP units are in g O2/m2/h. 
#From June 2014 on, the units are mmol O2/m2/h. So those early rates will need to be converted

All_PP_data %>%
  mutate(UP_Oxy = ifelse(),
         DN_Oxy = ,
         PP = ,)