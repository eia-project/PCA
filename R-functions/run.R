#
rm(list=ls())

# functions
f1 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/XLSXweather_to_csv.R"
f2 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/hourly_summary_wdata.R"
f3 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/estimate_thresholds.R"
f4 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/check_weather_data.R"
f5 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/fill_wdata.R"
f6 <- "https://raw.githubusercontent.com/jninanya/PCA/main/R-functions/QualityControlData.R"

source(url(f1))
source(url(f2))
source(url(f3))
source(url(f4))
source(url(f5))
source(url(f6))

# librarires
#install.packages(c("readxl", "lubridate", "dplyr", "openair"))
library(readxl)      # lectura de archivos XLSX
library(lubridate)   # manejo de fechas
library(dplyr)       # manejo de data frames
library(openair)     # generación rosa de viento


# XLSX file
xlsx_file <- "../../Data/DATOS ESTACIONES-PCA-NICARAGUA.xlsx"

################################################################################
# XLSXweather_to_csv(xlsx_file, create_dir = FALSE, dir.path = NULL)
# *** Separates data from XLSX file into individual .csv files ***
################################################################################

#res <- XLSXweather_to_csv(xlsx_file, sheet = "ALL")
res <- XLSXweather_to_csv(xlsx_file, sheet = "casa blanca")
head(res$casa_blanca)

################################################################################
# hourly_summary_wdata(weather, check_datetime)
# *** hourly summary of weather data ***
################################################################################

weather <- res$casa_blanca
wd <- hourly_summary_wdata(weather, check_datetime = TRUE)

head(wd$hourly)


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




############

library(sf)
library(ggplot2)

# Generar datos aleatorios de coordenadas y temperatura
set.seed(123)  # Para reproducibilidad
n_estaciones <- 8

latitudes <- runif(n_estaciones, 11.8, 12.2)  # Ejemplo de latitudes en el rango de 37 a 40
longitudes <- runif(n_estaciones, -86.8, -84.6)  # Ejemplo de longitudes en el rango de -120 a -117
temperaturas <- runif(n_estaciones, 10, 30)  # Ejemplo de temperaturas en el rango de 10 a 30 grados Celsius

# Crear un data frame con los datos de las estaciones
estaciones <- data.frame(
  Latitud = latitudes,
  Longitud = longitudes,
  Temperatura = temperaturas
)

estaciones_sf <- st_as_sf(estaciones, coords = c("Longitud", "Latitud"), crs = 4326)

# Crear una grilla espacial
grid <- st_as_sf(st_make_grid(st_bbox(estaciones_sf), cellsize = c(0.1, 0.1)))

# Unir la grilla a los datos de las estaciones
datos_grillados <- st_join(grid, estaciones_sf)

# Visualizar los datos grillados
ggplot() +
  geom_sf(data = datos_grillados, aes(fill = Temperatura)) +
  geom_sf(data = estaciones_sf, color = "red", size = 3) +
  labs(title = "Mapa de Observaciones Meteorológicas Grilladas")


###########
datos_estaciones <- estaciones
estaciones_sf <- st_as_sf(datos_estaciones, coords = c("Longitud", "Latitud"), crs = 4326)

# Crear una grilla espacial regular basada en las coordenadas de las estaciones
grid_resolution <- 0.1  # Ajusta la resolución de la grilla según tu necesidad
bounding_box <- st_bbox(estaciones_sf)  # Bounding box de las estaciones
lon <- seq(bounding_box$xmin, bounding_box$xmax, by = grid_resolution)
lat <- seq(bounding_box$ymin, bounding_box$ymax, by = grid_resolution)
grid_sf <- st_as_sf(expand.grid(x = lon, y = lat), coords = c("x", "y"), crs = 4326)


datos_grillados <- st_join(grid_sf, estaciones_sf)

ggplot() +
  geom_sf(data = datos_grillados, aes(fill = Temperatura)) +
  geom_sf(data = estaciones_sf, color = "red", size = 3) +
  labs(title = "Mapa de Observaciones Meteorológicas Grilladas")




# Crear un modelo de variograma (ajusta los parámetros según tus datos)
variogram_model <- vgm(psill = 1, model = "Sph", range = 0.2, nugget = 0.1)

# Realizar interpolación con gstat
interpolation_result <- gstat::gstat(
  formula = Temperatura ~ 1,
  locations = estaciones_sf,
  data = datos_estaciones,
  model = variogram_model
)

# Predecir en la grilla
grid_sf$Temperatura_Interpolada <- predict(interpolation_result, newdata = grid_sf)

