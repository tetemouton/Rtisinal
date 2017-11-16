produce_calender <- function(act=act, keep.site="Atafu", datrng=datrng, site.ctch=ATFdat, sav.dir="C:/Tokelau/"){
  
  
  act.tab <- act %>% filter(site == keep.site)   # Just grab the site you are interested in
  
  ctch.trps <- site.ctch$ctch %>% group_by(date1, art_trip_id) %>% summarise(N=n())
  ctch.trps %<>% group_by(date1) %>% summarise(N=n())
  
  
  act.trps <- act %>% filter(site == keep.site) %>% mutate(AllVes=motor + paddle + sail) %>% group_by(date1) %>% summarise(Act=sum(AllVes))
  act.trps$Pres <- ifelse(act.trps$Act > 0, 1, 0)  
  
  tmp <- left_join(datrng, ctch.trps) %>% left_join(act.trps) %>% mutate(Catch=ifelse(is.na(N), 0, 1)) 
  
  
  windows(2500,3000)
    pl <- ggcal1(dates=tmp$date1, fills=tmp$Catch, textvl=tmp$Pres) + theme(legend.position="none") + scale_fill_gradient2(low="grey55", mid="white", high="steelblue2", midpoint=0.5, na.value="grey92")
    print(pl)
    savePlot(file=paste0(sav.dir, "Calender_Activity_", keep.site, ".png"), type="png")
  dev.off()
  
  windows(2500,3000)
    pl <- ggcal1(dates=tmp$date1, fills=tmp$Catch, textvl=tmp$N) + theme(legend.position="none") + scale_fill_gradient2(low="grey55", mid="white", high="steelblue2", midpoint=0.5, na.value="grey92")
    print(pl)
    savePlot(file=paste0(sav.dir, "Calender_Trips_", keep.site, ".png"), type="png")
  dev.off()
  
  table(tmp$Catch, tmp$Pres, useNA="always")
  
}