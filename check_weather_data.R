check_weather_data <- function(weather, tsl, z){
  
  wd <- weather
  out <- as.data.frame(matrix(nrow = nrow(wd), ncol = 12))
  
  for(i in 1:nrow(wd)){
    
    temp = wd$temp[i]
    tmin = wd$tmin[i]
    tmax = wd$tmax[i]
    rhum = wd$rhum[i]
    tdew = wd$tdew[i]
    wvel = wd$wvel[i]
    wdir = wd$wdir[i]
    pbar = wd$pbar[i]
    prec = wd$prec[i]
    srad = wd$srad[i]
    prad = wd$prad[i]
    etpo = wd$etpo[i]
    
    
    
    #--- rule 0: tmin <= temp <= tmax 
    if(is.na(temp) | is.na(tmin) | is.na(tmax)){
      rule0 = "ND"
    } else {
      if(temp >= tmin & temp <= tmax){
        rule0 = "C" 
      } else {rule0 = "M"}
    }
    
    #--- rule 1: -40C <=  temp <= 60C
    if(is.na(temp) | is.na(tmin) | is.na(tmax) | is.na(tdew)){
      rule1 = "ND"
    } else {
      if((temp >= -40 & temp <= 60) & (tmin >= -40 & tmin <= 60) & 
         (tmax >= -40 & tmax <= 60) & (tdew >= -40 & tdew <= 60)){
        rule1 = "C"
      } else {rule1 = "M"}
    }
    
    #--- rule 2: 0% <=  rhum <= 100%
    if(is.na(rhum)){
      rule2 = "ND"
    } else {
      if(rhum >= 0 & rhum <= 100){
        rule2 = "C"
      } else {rule2 = "M"}
    }
    
    #--- rule 3: 0.0 <= prec <= 401 
    if(is.na(prec)){
      rule3 = "ND"
    } else {
      if(prec >= 0.0 & prec <= 401){
        rule3 = "C" 
      } else {rule3 = "M"}
    }
    
    #--- rule 6: 0.0 <= wvel <= 45 
    if(is.na(wvel)){
      rule6 = "ND"
    } else {
      if(wvel >= 0.0 & wvel <= 45){
        rule6 = "C" 
      } else {rule6 = "M"}
    }
    
    #--- rule 7: 300.0 <= pbar <= 1100 
    if(is.na(pbar)){
      rule7 = "ND"
    } else {
      if(pbar >= 300.0 & pbar <= 1100){
        rule7 = "C" 
      } else {rule7 = "M"}
    }
    
    #--- rule 8: -1. <= pbar <= 1400. 
    if(is.na(srad)){
      rule8 = "ND"
    } else {
      if(srad >= -1. & srad <= 1400){
        rule8 = "C" 
      } else {rule8 = "M"}
    }
    
    #--- rule 9a: Linf <= temp <= Lsup
    Linf = tsl$temp$tsl_daily[1]
    Lsup = tsl$temp$tsl_daily[2]
    
    if(is.na(temp)){
      rule9a = "ND"
    } else {
      if(temp >= Linf & temp <= Lsup){
        rule9a = "C" 
      } else {rule9a = "D"}
    }
    
    #--- rule 9b: Linf <= tmin <= Lsup
    Linf = tsl$tmin$tsl_daily[1]
    Lsup = tsl$tmin$tsl_daily[2]
    
    if(is.na(tmin)){
      rule9b = "ND"
    } else {
      if(tmin >= Linf & tmin <= Lsup){
        rule9b = "C" 
      } else {rule9b = "D"}
    }
    
    #--- rule 9c: Linf <= tmax <= Lsup
    Linf = tsl$tmax$tsl_daily[1]
    Lsup = tsl$tmax$tsl_daily[2]
    
    if(is.na(tmax)){
      rule9c = "ND"
    } else {
      if(tmax >= Linf & tmax <= Lsup){
        rule9c = "C" 
      } else {rule9c = "D"}
    }
    
    #--- rule 10: Linf <= rhum <= Lsup
    Linf = tsl$rhum$tsl_daily[1]
    Lsup = tsl$rhum$tsl_daily[2]
    
    if(is.na(rhum)){
      rule10 = "ND"
    } else {
      if(rhum >= Linf & rhum <= Lsup){
        rule10 = "C" 
      } else {rule10 = "D"}
    }
    
    #--- rule 14: Linf <= wvel <= Lsup
    Linf = tsl$wvel$tsl_daily[1]
    Lsup = tsl$wvel$tsl_daily[2]
    
    if(is.na(wvel)){
      rule14 = "ND"
    } else {
      if(wvel >= Linf & wvel <= Lsup){
        rule14 = "C" 
      } else {rule14 = "D"}
    }
    
    #--- rule 15b: Linf <= wvel <= Lsup
#    Pz = 1013*(((293-0.0065*z)/293)^(5.26))
#    
#    if(is.na(pbar)){
#      rule15b = "ND"
#    } else {
#      if(pbar >= Pz-30.5 & pbar <= Pz+30.5){
#        rule15b = "C" 
#      } else {rule15b = "D"}
#    }
    
    ###
    
    #    #--- rule 31a: |temp - temp1| <= 4
    #    if(i >= 2 & abs(temp-wd$temp[i-1]) <= 4){
    #        rule31a = "C" 
    #    } else {rule31a = "D"}
    #    
    #    #--- rule 31b: |temp - temp2| <= 7
    #    if(i >= 3 & abs(temp-wd$temp[i-2]) <= 7){
    #      rule31b = "C" 
    #    } else {rule31b = "D"}
    #    
    #    #--- rule 31c: |temp - temp3| <= 9
    #    if(i >= 4 & abs(temp-wd$temp[i-3]) <= 9){
    #      rule31c = "C" 
    #    } else {rule31c = "D"}
    #    
    #    #--- rule 31d: |temp - temp6| <= 15
    #    if(i >= 7 & abs(temp-wd$temp[i-6]) <= 15){
    #      rule31d = "C" 
    #    } else {rule31d = "D"}
    #    
    #    #--- rule 31e: |temp - temp12| <= 25
    #    if(i >= 13 & abs(temp-wd$temp[i-12]) <= 25){
    #      rule31e = "C" 
    #    } else {rule31e = "D"}
    #    
    #    #--- rule 32: tmin <= temp <= tmax 
    #    if(temp >= tmin & temp <= tmax){
    #      rule32 = "C" 
    #    } else {rule32 = "D"}
    
    
    out[i, 1] = rule0
    out[i, 2] = rule1
    out[i, 3] = rule2
    out[i, 4] = rule3
    
    out[i, 5] = rule6
    out[i, 6] = rule7
    out[i, 7] = rule8
    out[i, 8] = rule9a
    out[i, 9] = rule9b
    out[i, 10] = rule9c
    out[i, 11] = rule10
    
    out[i, 12] = rule14
    #out[i, 12] = rule15b
    
    #    out[i, 14] = rule31a
    #    out[i, 15] = rule31b
    #    out[i, 16] = rule31c
    #    out[i, 17] = rule31d
    #    out[i, 18] = rule31e
    #    out[i, 19] = rule32
  }
  xnames =  c("rule0", "rule1", "rule2", "rule3",
                     "rule6", "rule7", "rule8", "rule9a",
                     "rule9b", "rule9c", "rule10", "rule14",
                     "rule15b", "rule31a", "rule31b", "rule31c",
                     "rule31d", "rule31e", "rule32")
  colnames(out) <- xnames[1:12]

  smr <- matrix(nrow = 3, ncol = 12)
  smr <- as.data.frame(smr)
  rownames(smr) <- c("C", "M", "ND")
  colnames(smr) <- colnames(out)
  
  for(i in 1:12){
    
    smr[1,i] <- sum(out[,i]=="C", na.rm = TRUE)/nrow(out)*100
    smr[2,i] <- sum(out[,i]=="M", na.rm = TRUE)/nrow(out)*100
    smr[3,i] <- sum(out[,i]=="ND", na.rm = TRUE)/nrow(out)*100
    
  }
  
  out$datetime <- wd$datetime
  
  return(cwd = list("out_cwd" = out[,c(13,1:12)], "smr" = smr))
  
  
}






