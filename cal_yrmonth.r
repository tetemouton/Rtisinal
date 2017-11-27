cal_yrmonth <- function(dat=trip_effort_catch, yrmn="yrmon", grpnm="vessel_name", site="site", xtrnm=NULL){
  
  if(is.null(xtrnm)) {
    
    tmp <- dat %>% group_by_(yrmn, grpnm, site) %>% summarise(N=n())
    tmp1 <- tmp %>% group_by_(yrmn, site) %>% summarise(N=n()) %>% as.data.frame()
    
  } else {
    
    tmp <- dat %>% group_by_(yrmn, grpnm, site, xtrnm) %>% summarise(N=n())
    tmp1 <- tmp %>% group_by_(yrmn, site, xtrnm) %>% summarise(N=n()) %>% as.data.frame()
    
  }
  
  tmp1$mind <- round(tmp1[, yrmn] %% 1 * 12 + 1)   # Necessary to round to make a proper integer, kind of...
  tmp1$ticnm <- labm[tmp1$mind] 
  tmp1$ticnm <- ifelse(tmp1$ticnm == "Jan", paste(floor(tmp1$yrmon), tmp1$ticnm), tmp1$ticnm)
  
  ticknm <- tmp1$ticnm[match(unique(tmp1$yrmon), tmp1$yrmon)]

  tmp1[, yrmn] <- as.factor(tmp1[, yrmn])
  
  return(list(tmp1=tmp1, ticknm=ticknm))
}


