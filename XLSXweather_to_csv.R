#
XLSXweather_to_csv <- function(xlsx_file, create_dir = FALSE, dir.path = NULL){
  
  # required librarires
  if (!require("readxl")) stop("The package 'readxl' was not installed")
  if (!require("lubridate")) stop("The package 'lubridate' was not installed")
  
  # sheets names
  sn <- excel_sheets(xlsx_file)
  n <- length(sn)
  out <- list()
  
  # extracting data
  for(i in 1:n){
    
    df <- read_xlsx(xlsx_file, sheet = sn[i], skip = 1)
    df <- as.data.frame(df)
    
    df$Time <- as.character.Date(df$Time)
    df$date <- date(df$Date)
    df$time <- NA
    
    for(j in 1:nrow(df)){
      df$time[j] <- substr(df$Time[j],12,16)
    }
    
    # variables selection
    wd <- df[, c(39:40, 3:5, 6:7, 8:9, 17, 18, 20:21, 34)]
    colnames(wd) <- c("date", "time", "temp", "tmax", "tmin", 
                      "rhum", "tdew", "wvel", "wdir", "pbar",
                      "prec", "srad", "prad", "etpo")
    
    wd <- wd %>% mutate_at(c("temp", "tmax", "tmin", "rhum", "tdew", "wvel", 
                             "pbar", "prec", "srad", "prad", "etpo"), as.numeric)
    
    # creating a directory for .csv files
    if(create_dir){
      dir.create(path = dir.path, showWarnings = FALSE)
      write.csv(wd, paste0(dir.path, sub(" ","_",sn[i]), "_wdata.csv"))
    }
    
    # output
    out[[i]] <- wd
    names(out)[i] <- sub(" ","_",sn[i])
    
  }
  
  return(out)

} 
