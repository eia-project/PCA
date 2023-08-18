#
rm(list=ls())

# work directory
WD <- setwd("C:/Users/jninanya/OneDrive - CGIAR/Desktop/PCA/R-project/")

# libraries
library(dplyr)
library(openxlsx)
library(readxl)
library(lubridate)

# R-functions
source("./R-scripts/XLSXweather_to_csv.R")
source("./R-scripts/hourly_summary_wdata.R")
source("./R-scripts/estimate_thresholds.R")
source("./R-scripts/check_weather_data.R")
source("./R-scripts/fill_wdata.R")
source("./R-scripts/QualityControlData.R")


# XLSX file
xlsx_file <- "./Data/DATOS ESTACIONES-PCA-NICARAGUA.xlsx"

################################################################################
# XLSXweather_to_csv(xlsx_file, create_dir = FALSE, dir.path = NULL)
# *** Separates data from XLSX file into individual .csv files ***
################################################################################

#res <- XLSXweather_to_csv(xlsx_file, create_dir = TRUE, dir.path = "./Data/XLSX_to_csv/")
res <- XLSXweather_to_csv(xlsx_file)
head(res$las_lomas)


################################################################################
# hourly_summary_wdata(weather, check_datetime)
# *** hourly summary of weather data ***
################################################################################

wd <- list()
dir.create(path = "./Output/out-hourly/", showWarnings = FALSE)

for(i in 1:length(res)){
  
  wd[[i]] = hourly_summary_wdata(res[[i]], check_datetime = TRUE)
  
  names(wd)[i] = names(res)[i]
  print(names(wd)[i])
  print(wd[[i]]$missing_wd)
  
  write.csv(wd[[i]]$hourly, paste0("./Output/out-hourly/out_", names(wd)[i],".csv"))
  
}


################################################################################
# fill_wdata(weather, method)
# *** complete missing data ***
################################################################################

fwd <- list()
for(i in 1:length(wd)){
  
  xx <- fill_wdata(weather = wd[[i]]$hourly, method = "mean")
  
  fwd[[i]] = hourly_summary_wdata(xx, check_datetime = TRUE)
  names(fwd)[i] = names(wd)[i]
  print(names(wd)[i])
  print(fwd[[i]]$missing_wd)
  
}


################################################################################
# estimate_thresholds(weather, graph, wd_var)
# *** estimates thresholds for checking data ***
################################################################################

tsl = estimate_thresholds(weather = fwd$cacauli$hourly, loc = "cacauli")

tsl <- list()
for(i in 1:length(fwd)){
  
  xx <- fill_wdata(weather = fwd[[i]]$hourly, method = "mean")
  
  tsl[[i]] = estimate_thresholds(weather = fwd[[i]]$hourly, loc = names(fwd)[i])
  names(tsl)[i] = names(fwd)[i]
  
}


################################################################################
# check_weather_data(weather, graph, wd_var)
# *** estimates thresholds for checking data ***
################################################################################

check_wd <- check_weather_data(weather = fwd$cacauli$hourly, tsl = tsl$cacauli, z = 500)
head(check_wd$out_cwd)
check_wd$smr

cwd <- list()
dir.create(path = "./Output/out-check/", showWarnings = FALSE)

for(i in 1:length(fwd)){
  
  cwd[[i]] <- check_weather_data(weather = fwd[[i]]$hourly, tsl = tsl[[i]], z = 500)
  names(cwd)[i] = names(fwd)[i]
  
  print(names(cwd)[i])
  print(cwd[[i]]$smr)
  
  write.csv(cwd[[i]]$out_cwd, paste0("./Output/out-check/cwd_", names(cwd)[i], ".csv"))
  
}


################################################################################









