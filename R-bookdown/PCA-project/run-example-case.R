
rm(list=ls())

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


#install.packages(c("readxl", "lubridate", "dplyr", "openair"))

library(readxl)      # lectura de archivos XLSX
library(lubridate)   # manejo de fechas
library(dplyr)       # manejo de data frames
library(openair)     # generación rosa de viento
library(nasapower)


# reading data
xlsx_file <- "../../Data/DATOS ESTACIONES-PCA-NICARAGUA.xlsx"

#res <- XLSXweather_to_csv(xlsx_file, sheet = "ALL")
res <- XLSXweather_to_csv(xlsx_file, sheet = "casa blanca")

head(res$casa_blanca)


# hourly
weather <- res$casa_blanca
wd <- hourly_summary_wdata(weather, check_datetime = TRUE)

head(wd$hourly)
head(wd$hourly[wd$hourly$missing == TRUE, ])

wd$missing_wd

#

weather <- wd$hourly
loc <- names(res)[1]
wd_var = c("temp", "tmax", "tmin", "rhum", "wvel", "srad")

par(mfrow = c(2, 3))
tsl = estimate_thresholds(weather, loc, graph = TRUE, wd_var)


## rango de datos
n=length(wd$hourly$datetime)

wd$hourly$datetime[1:3]
wd$hourly$datetime[n]

lat <- 13.34
lon <- -86.52
cc <- c(lat, lon)


##### 

ag_d <- get_power(
  community = "ag",
  lonlat = c(lon, lat),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2021-01-01","2022-10-29"),
  temporal_api = "hourly"
)

x <- wd$hourly
y <- as.data.frame(ag_d)

x$date <- date(x$datetime)
y$date <- as.Date(paste0(y$YEAR,"-",y$MO,"-",y$DY))

D1 <- as.Date("2022-01-05")
D2 <- as.Date("2022-10-05")

x <- x[x$date >= D1 & x$date <= D2,]
y <- y[y$date >= D1 & y$date <= D2,]


y$temp = y$T2M
y$rhum = y$RH2M
y$prec = y$PRECTOTCORR
#####


library(dplyr)

set.seed(10)
obs <- x
sim <- y

plot(obs$temp, sim$temp, pch=20, col="blue")

anomalies <- sim$temp - obs$temp
anomaly_mean <- mean(anomalies, na.rm=TRUE)
rd <- runif(length(anomalies), 0.30, 1.70)

plot(anomalies, pch=20, col="blue")
abline(h=anomaly_mean, col="red", lwd=2)


# Añadir la media de las diferencias a los datos del modelo para corregirlos
simc <- sim
#simc$temp <- simc$temp - anomaly_mean
simc$temp <- simc$temp - rd*anomalies

plot(obs$temp, sim$temp, xlab="observed", ylab="nasa", 
     pch=20, col="gray60")
plot(obs$temp, simc$temp, xlab="observed", ylab="nasa_corrected", 
     pch=20, col="gray60")




####################################

# Cargar el paquete KFAS
library(KFAS)

observed_ts <- ts(observed_data$temp, start = c(2021, 1), frequency = 24)  # Frecuencia 24 para datos horarios
model_ts <- ts(model_data$temp, start = c(2021, 1), frequency = 24)  # Frecuencia 24 para datos horarios

# Especificar un modelo ARMA
arma_model <- arima(model_ts, order = c(1, 1, 0))  # Puedes personalizar el orden según tus necesidades

# Definir el modelo del Filtro de Kalman
model <- SSModel(observed_ts ~ 1, process = arma_model)

# Aplicar el Filtro de Kalman para asimilar los datos observados en el modelo
kf_result <- KFS(model = model, y = observed_ts)

# Obtener las estimaciones corregidas
corrected_values <- fitted(kf_result)



#########################################################
####################################################################

# Calcular Mean Absolute Error (MAE)
mae <- mean(abs(obs$temp - sim$temp), na.rm=TRUE)

# Calcular Root Mean Square Error (RMSE)
rmse <- sqrt(mean((obs$temp - sim$temp)^2, na.rm=TRUE))

# Calcular Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((obs$temp - sim$temp) / obs$temp), na.rm=TRUE) * 100

# Calcular R-squared (R^2)
correlation <- cor(obs$temp, sim$temp, use="pairwise.complete.obs")
rsquared <- correlation^2

m1 <- c(mae, rmse, mape, rsquared)
names(m1) <- c("mae", "rmse", "mape", "rsquared")




# Calcular Mean Absolute Error (MAE)
mae <- mean(abs(obs$temp - simc$temp), na.rm=TRUE)

# Calcular Root Mean Square Error (RMSE)
rmse <- sqrt(mean((obs$temp - simc$temp)^2, na.rm=TRUE))

# Calcular Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((obs$temp - simc$temp) / obs$temp), na.rm=TRUE) * 100

# Calcular R-squared (R^2)
correlation <- cor(obs$temp, simc$temp, use="pairwise.complete.obs")
rsquared <- correlation^2

m2 <- c(mae, rmse, mape, rsquared)
names(m2) <- c("mae", "rmse", "mape", "rsquared")

m1
m2






