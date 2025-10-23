## get SST data

library(tidyverse)
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(slider)

# Bring in the in situ temperature data
Temp_LTER1<-read_csv(here("Data","MCR_LTER02_BTM_Backreef_Forereef_20250522.csv")) %>%
  filter(reef_type_code == "Backreef") 

Mean_daily_Temp<-Temp_LTER1 %>%
  mutate(date = as.Date(time_local)) %>%
  group_by(date)%>%
  summarise(daily_temp =mean(temperature_c, na.rm = TRUE))


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
                       end = c("2015-12-31", "2025-09-01"))

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

## Calcualte DHW by year
# ---- Helper to compute MMM from a chosen climatology window ----
compute_mmm <- function(df, date_col = "date", sst_col = "sst",
                        clim_years = 2006:2025) {
  df %>%
    transmute(date = .data[[date_col]],
              sst  = .data[[sst_col]],
              year = year(date),
              month = month(date)) %>%
    filter(year %in% clim_years) %>%
    group_by(year, month) %>%
    summarize(monthly_mean = mean(sst, na.rm = TRUE), .groups = "drop_last") %>%
    summarize(clim_monthly_mean = mean(monthly_mean, na.rm = TRUE), .groups = "drop") %>%
    summarize(MMM = max(clim_monthly_mean, na.rm = TRUE)) %>%
    pull(MMM)
}

# ---- Main function: compute daily DHW, then summarize by year ----
OISST_data<-OISST_data %>%
  rename(date = t, sst  = temp)

compute_dhw <- function(df,
                        date_col = "date",
                        sst_col  = "sst",
                        mmm = NULL,                 # set to numeric to override climatology
                        clim_years = 2006:2015,     # used if mmm is NULL
                        window_days = 84) {
  
  stopifnot(all(c(date_col, sst_col) %in% names(df)))
  dat <- df %>%
    transmute(date = as_date(.data[[date_col]]),
              sst  = as.numeric(.data[[sst_col]])) %>%
    arrange(date)
  
  # Optional: check daily regularity and warn if large gaps
  gaps <- diff(dat$date)
  if (length(gaps) && max(as.integer(gaps), na.rm = TRUE) > 3) {
    warning("Detected date gaps >3 days; DHW may be underestimated in gap periods.")
  }
  
  # MMM: compute if not provided
  if (is.null(mmm)) {
    mmm <- compute_mmm(dat, date_col = "date", sst_col = "sst", clim_years = clim_years)
  }
  
  # HotSpot (°C): only count when SST > MMM + 1°C
  dat <- dat %>%
    mutate(
      hotspot = pmax(0, sst - (mmm + 1))
    )
  
  # DHW daily (°C-weeks): trailing 84-day sum of hotspot/7
  # Use slider with .before = window_days - 1 for inclusive trailing window
  dat <- dat %>%
    mutate(
      dhw = slide_dbl(hotspot,
                      .before = window_days - 1,
                      .complete = FALSE,
                      .f = ~ sum(.x, na.rm = TRUE) / 7)
    )
  
  # Annual summaries
  yearly <- dat %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(
      dhw_max = max(dhw, na.rm = TRUE),                 # standard report metric
      dhw_cumulative = sum(dhw, na.rm = TRUE),          # total DHW across the year
      days_with_hotspot = sum(hotspot > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    mmm = mmm,
    daily = dat,     # columns: date, sst, hotspot, dhw
    yearly = yearly  # columns: year, dhw_max, dhw_cumulative, days_with_hotspot
  )
}

res <- compute_dhw(Mean_daily_Temp, date_col = "date",
                   sst_col  = "daily_temp",)
 res$mmm
 res$yearly
 head(res$daily)

res$daily %>%
  ggplot(aes(date, dhw)) +
  geom_line() +
  geom_area(fill = "firebrick", alpha = 0.6, color = NA) +
  labs(x = "Date", y = "DHW (°C-weeks)", title = "Mo'orea Daily Degree Heating Weeks (trailing 12 weeks)")+
  #scale_fill_manual("firebrick")+
  theme_bw()

res$yearly %>%
  ggplot(aes(year, dhw_max)) +
  geom_col(fill = "firebrick") +
  labs(x = "Year", y = "Annual Max DHW (°C-weeks)", 
      # title = "Annual Maximum DHW by Year"
       )+
  theme_minimal()
  

Mean_daily_Temp %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  summarise(temp_mean = mean(daily_temp, na.rm = TRUE),
            temp_max = max(daily_temp, na.rm = TRUE))%>%
  ggplot(aes(x = Year, y = temp_max))+
  geom_point()+
  geom_smooth(method = "lm")

write_csv(SST_year, here("Data","SST_year.csv"))

write_csv(SST_year_month, here("Data","SST_year_month.csv"))
