require(dplyr)
require(tidyr)
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

ts_confirm <- read.csv(file.path(url,"time_series_19-covid-Confirmed.csv"))
ts_death <- read.csv(file.path(url,"time_series_19-covid-Deaths.csv"))
ts_recover <- read.csv(file.path(url,"time_series_19-covid-Recovered.csv"))

fspread <- function(df, vname){
  df %>% 
    pivot_longer(cols = starts_with("X"),
                 names_to = "t", values_to = vname) %>%
    mutate(t = as.POSIXct(t, format = "X%m.%d.%y"))
}

ts_confirm <- fspread(ts_confirm, "Confirmed")
ts_death <- fspread(ts_death, "Dead")
ts_recover <- fspread(ts_recover, "Cured")

n_global <- ts_confirm %>% left_join(ts_death) %>% left_join(ts_recover)

save(n_global,file = "Data/data_global_0318.rdata")
