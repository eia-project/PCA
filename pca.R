source("XLSXweather_to_csv.R")
source("hourly_summary_wdata.R")
source("estimate_thresholds.R")
source("check_weather_data.R")
source("fill_wdata.R")


library(dplyr)
library(readxl)


res=XLSXweather_to_csv("./DATOS ESTACIONES-PCA-NICARAGUA.xlsx")
head(res$las_lomas)

wd = list()
for(i in 1:length(res)){
  
  wd[[i]] = hourly_summary_wdata(res[[i]], check_datetime = TRUE)

  names(wd)[i] = names(res)[i]
  print(names(wd)[i])
  print(wd[[i]]$missing_wd)

  
  write.csv(wd[[i]]$hourly, paste0("out_", names(wd)[i],".csv"))
  
}


fwd = list()
for(i in 1:length(wd)){
  
  xx <- fill_wdata(weather = wd[[i]]$hourly, method = "mean")
  
  fwd[[i]] = hourly_summary_wdata(xx, check_datetime = TRUE)
  names(fwd)[i] = names(wd)[i]
  print(names(wd)[i])
  print(fwd[[i]]$missing_wd)
  
}






tsl = estimate_thresholds(weather = wd$cacauli$hourly)


check_wd <- check_weather_data(weather = wd$cacauli$hourly, tsl = tsl, z = 500)
head(check_wd$out_cwd)
check_wd$smr




