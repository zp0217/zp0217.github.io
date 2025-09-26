```{r,warning = F,message = F}

library(dplyr)
library(readr)
library(lubridate)
library(here)

japan<-read.csv(here("data/yield", "yield_curve_japan.csv"))
us_2y <- read.csv(here("data/yield", "yield_curve_2_us.csv"),skip = 1)
us_10y <- read.csv(here("data/yield", "yield_curve_10_us.csv"))
euro_2y <- read.csv(here("data/yield", "yield_curve_euro_2.csv"))
euro_10y <- read.csv(here("data/yield", "yield_curve_euro_10.csv"))
uk <- read.csv(here("data/yield", "yield_curve_uk.csv"))

japan_clean <- japan %>%
  rename(Date = Interest.Rate) %>%
  select(Date, JP_2Y = X.1, JP_10Y = X.9) %>%
  mutate(Date = ymd(Date))

us_clean <- us_2y %>%
  rename(Date = X2015.01.02, US_2Y = X0.66) %>%
  left_join(
    us_10y %>% rename(Date = observation_date, US_10Y = DGS10),
    by = "Date"
  ) %>%
  mutate(Date = ymd(Date))

euro_clean <- euro_2y %>%
  rename(Date = DATE, EU_2Y = last_col()) %>%
  left_join(
    euro_10y %>% rename(Date = DATE, EU_10Y = last_col()),
    by = "Date"
  ) %>%
  mutate(Date = ymd(Date))

uk_clean <- uk %>%
  rename(Date = date, UK_2Y = y2, UK_10Y = y10) %>%
  mutate(Date = ymd(Date))

merged <- japan_clean %>%
  full_join(us_clean, by = "Date") %>%
  full_join(euro_clean, by = "Date") %>%
  full_join(uk_clean, by = "Date") %>%
  arrange(Date)
clean_numeric <- function(x) {
  suppressWarnings(as.numeric(ifelse(x %in% c("-", "", "NA"), NA, x)))
}

merged <- merged %>%
  mutate(
    JP_2Y  = clean_numeric(JP_2Y),
    JP_10Y  = clean_numeric(JP_10Y),
    US_2Y  = clean_numeric(US_2Y),
    US_10Y = clean_numeric(US_10Y),
    EU_2Y  = clean_numeric(EU_2Y),
    EU_10Y = clean_numeric(EU_10Y),
    UK_2Y  = clean_numeric(UK_2Y),
    UK_10Y = clean_numeric(UK_10Y),
    JP_Spread = JP_10Y - JP_2Y,   # ← fixed name
    US_Spread = US_10Y - US_2Y,
    EU_Spread = EU_10Y - EU_2Y,
    UK_Spread = UK_10Y - UK_2Y
  )

#write_csv(merged, "merged_yield_curves.csv")

```