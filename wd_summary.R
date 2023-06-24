wd_summary <- function(weather, from, to){
  
  wd <- weather
  from <- as.Date(from)
  to <- as.Date(to)
  x <- 1:(as.numeric(to-from)+1)
  
  wd <- wd[date(wd$datetime) >= from & date(wd$datetime) <= to, ]
  wd$date <- as.character(date(wd$datetime))
  
  wd$group <- substr(wd$date, 1, 10)
  
  smr_mean <- wd %>%
    group_by(group) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  smr_sd <- wd %>%
    group_by(group) %>%
    summarise_if(is.numeric, sd, na.rm = TRUE)
  
  
  plot(x, smr_mean$temp, type = "l", las = 1)
  
  
  
  
}