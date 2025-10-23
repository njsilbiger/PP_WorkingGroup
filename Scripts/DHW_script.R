# Calculate DHW
## Make sure the data is converted to Daily Mean before doing this 
# install.packages(c("tidyverse", "lubridate", "slider"))
library(tidyverse)
library(lubridate)
library(slider)

# ---- Helper to compute MMM from a chosen climatology window ----
compute_mmm <- function(df, date_col = "date", sst_col = "sst",
                        clim_years = 1985:2012) {
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
compute_dhw <- function(df,
                        date_col = "date",
                        sst_col  = "sst",
                        mmm = NULL,                 # set to numeric to override climatology
                        clim_years = 1985:2012,     # used if mmm is NULL
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

# ---- Example usage ----
# Suppose you have a CSV with columns: date, sst (°C) for Moorea
# moorea <- read_csv("moorea_sst_daily.csv")  # date, sst

# res <- compute_dhw(moorea)
# res$mmm
# res$yearly
# head(res$daily)

# ---- (Optional) Quick plots ----
# library(ggplot2)
# res$daily %>%
#   ggplot(aes(date, dhw)) +
#   geom_line() +
#   labs(x = "Date", y = "DHW (°C-weeks)", title = "Mo'orea Daily Degree Heating Weeks (trailing 12 weeks)")

# res$yearly %>%
#   ggplot(aes(year, dhw_max)) +
#   geom_col() +
#   labs(x = "Year", y = "Annual Max DHW (°C-weeks)", title = "Annual Maximum DHW by Year")
