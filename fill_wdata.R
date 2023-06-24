fill_wdata <- function(weather, method = c("mean", "NA", "ml")){
  
  wd <- weather
  
  for(i in 3:ncol(wd)){
    
    wd[is.na(wd[, i]), i] <- mean(wd[,i], na.rm = TRUE)
    
  }
  
  return(filled_wd = wd)
}