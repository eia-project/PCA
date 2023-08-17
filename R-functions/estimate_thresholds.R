estimate_thresholds <- function(weather, graph = TRUE, loc,
                                wd_var = c("temp", "tmax", "tmin", 
                                            "rhum", "wvel")){
  
  df <- weather
  df$hour <- NA
  
  for(i in 1:nrow(df)){
    
    df$hour[i] <- substr(df$datetime[i],12,13)
   
  }
  
  out <- list()
 
  for(i in 1:length(wd_var)){
    
    xx <- data.frame(x = df$hour, y = df[, wd_var[i]])
    xx <- na.omit(xx)
    
    hh <- unique(df$hour)
    hh <- hh[c(24,1:23)]
    
    lsup <- vector()
    linf <- vector()
    
    for(j in 1:24){
      
      linf[j] = quantile(xx$y[xx$x == hh[j]], .01)
      lsup[j] = quantile(xx$y[xx$x == hh[j]], .99)
      
    }
    
    tsl_hourly <- data.frame(h=0:23, "p01"=linf, "p99"=lsup)
    
    
    b=boxplot(xx$y ~ xx$x, xlab = "hours", ylab = wd_var[i])
    lines(linf, lwd = 2, lty = 2, col = "red")
    lines(lsup, lwd = 2, lty = 2, col = "blue")
    title(paste0(toupper(wd_var[i]), " threshold for ", 
                 toupper(chartr("_", " ", loc))))
    
    l_inf = quantile(xx$y, .05)
    l_sup = quantile(xx$y, .95)
    
    tsl_daily <- c(l_inf, l_sup) 
    
    out[[i]] <- list("tsl_hourly"=tsl_hourly, "tsl_daily"=tsl_daily)
  }
  
  names(out) <- wd_var
  
  return(out)
  
}