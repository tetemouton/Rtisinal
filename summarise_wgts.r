summarise_wgts <- function(tmp=tmp, sppcds=sppcds, sav.dir="C:/Tokelau_Artisinal/tmp.tex", captn="Empty", txsz="tiny", lbl="tab:meanwttab"){
  
  avwgts <- tmp %>% filter(!is.na(sp_n), sp_kg > 0) %>%
                    mutate(Name=sppcds$sp_name[match(sp_code, sppcds$sp_code)], SciName=sppcds$sp_sci_name[match(sp_code, sppcds$sp_code)],
                           PltName=sppcds$plotnm[match(sp_code, sppcds$sp_code)]) %>%
                    group_by(sp_code, Name, SciName, PltName) %>% summarise(mean.wt=sum(sp_kg)/sum(sp_n), fish.weighed=sum(sp_n)) %>% as.data.frame()
  
  pr.tab <- avwgts %>% mutate(mean.wt=round(mean.wt, 2))
  
  # Make the names more suitable
  names(pr.tab) <- c("Code","Common","Scientific","Fig Name","Weight","N")
  
  # Convert to latex table and save
  pr.xtab <- xtable(pr.tab, align=c(rep("l", 5),rep("c", 2)), digits=c(rep(0, 5), 2, 0)) #, caption=captn, label=lbl) # digits=rep(0,5), 
  print.xtable(pr.xtab, file=sav.dir, include.rownames=FALSE, sanitize.text.function = function(x) x, size=txsz, caption.placement="top")
  
  return(avwgts)
  
  
}






















