## get SST data

library(tidyverse)
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing


# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


# Moorea NOrth shore 17°27'41"S 149°47'04"W
# -17.461389, 149.784444


# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = c(-17.46, -17.46),
                                longitude = c(149.78, 149.78),
                                fields = "sst")$data |> 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) |> 
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) |> 
    dplyr::select(lon, lat, t, temp) |> 
    stats::na.omit()
}

dl_years <- data.frame(date_index = 1:2,
                       start = c("2006-01-01", "2014-01-01" 
                                 ),
                       end = c("2015-12-31", "2025-08-01"))

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed

  OISST_data <- dl_years |> 
    dplyr::group_by(date_index) |> 
    dplyr::group_modify(~OISST_sub_dl(.x)) |> 
    dplyr::ungroup() |> 
    dplyr::select(lon, lat, t, temp)
 # 1.19    0.19  360.18 


# plot it
#OISST_data |> 
#dplyr::filter(t == "2019-12-01") |> 
#  ggplot2::ggplot(aes(x = lon, y = lat)) +
#  ggplot2::geom_tile(aes(fill = temp)) +
#   ggplot2::borders() + # Activate this line to see the global map
#  ggplot2::scale_fill_viridis_c() +
#  ggplot2::coord_quickmap(expand = F) +
#  ggplot2::labs(x = NULL, y = NULL, fill = "SST (°C)") +
#  ggplot2::theme(legend.position = "bottom")


SST_year_month<-OISST_data %>%
  mutate(Year = year(t),
         Month = month(t)) %>%
  group_by(Year, Month)%>%
  summarise(mean_SST = mean(temp, na.rm = TRUE),
            max_SST = max(temp, na.rm = TRUE)) 

SST_year_month %>%
  mutate(Year_Month = paste(Year, Month))%>%
  ggplot(aes(x = Year_Month, y = max_SST))+
  geom_point()
  
SST_year<-OISST_data %>%
  mutate(Year = year(t),
         Month = month(t)) %>%
  group_by(Year)%>%
  summarise(mean_SST = mean(temp, na.rm = TRUE),
            max_SST = max(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Year_Benthic = Year + 1) # the benthic data should be last years temperature driving the change


SST_plot<-SST_year %>%
  ggplot(aes(x = Year, y = mean_SST))+
  geom_point()+
  geom_smooth(method = "lm", color = "black")+
  labs(x = "Year",
       y = "Mean Annual SST ("~degree~"C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size =14))
ggsave(here("Output","YearlySST.pdf"), width = 6, height = 6)

write_csv(SST_year, here("Data","SST_year.csv"))

write_csv(SST_year_month, here("Data","SST_year_month.csv"))
