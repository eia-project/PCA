## missing data per variable

hourly_summary_wdata <- function(weather, check_datetime = FALSE){
  
  df <- weather
  
  # required librarires
  if (!require("dplyr")) stop("The package 'dplyr' was not installed")
  if (!require("lubridate")) stop("The package 'lubridate' was not installed")
  
  # summary data
  if(!("datetime" %in% colnames(df))){
  df$group <- paste0(df$date, " ", substr(df$time,1,2), ":00:00")
  df$datetime <- as_datetime(df$group)
  }
  
  smr <- df %>%
    group_by(datetime) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  smr <- as.data.frame(smr)
  smr$missing <- FALSE
  wd <- smr[,c(1,13,2:12)]
  mdt <- NULL
  
  missing_wd <- matrix(nrow = 2, ncol = ncol(wd)-1)
  missing_wd <- as.data.frame(missing_wd)
  rownames(missing_wd) <- c("n", "percentage")
  colnames(missing_wd) <- colnames(wd)[c(1,3:length(wd))]

  if(check_datetime){
    
    time_0 <- as_datetime(wd$datetime[1])
    time_n <- as_datetime(wd$datetime[nrow(wd)])
    dt <- data.frame(datetime = seq(time_0, time_n, by = "hour"))
    
    mdt <- dt$datetime[!(dt$datetime %in% wd$datetime)]
    wd <- full_join(wd, dt, by = "datetime")
    wd$missing[is.na(wd$missing)] <- TRUE
    
    n = length(dt$datetime)
    missing_wd[1,1] <- length(mdt)
    missing_wd[2,1] <- length(mdt)/n*100
    
    for(i in colnames(wd)[3:ncol(wd)]){
      
      missing_wd[1, i] <- sum(is.na(wd[,i]))
      missing_wd[2, i] <- sum(is.na(wd[,i]))/n*100
      
    }
    
    missing_wd <- t(missing_wd)
    
}
  
 return(out = list("hourly" = wd, "missing_datetime" = mdt, "missing_wd" = missing_wd))
  
}