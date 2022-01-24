get_recorded_catch <- function(tmp=tmp, keep.site="Atafu", mth.cut=500, cut.wgt=100, sppcds=sppcds,
                               captn="Empty", txsz="footnotesize", lbl="tab:AtafuWt", sav.dir="C:/Tokelau_Artisinal/tmp.tex", tim.vec=c("2016-8-31","2017-8-31")){
  
  mth.tmp <- tmp %>% filter(site == keep.site, date1 > tim.vec[1], date1 <= tim.vec[2]) %>% group_by(meth) %>% summarise(catch=sum(newwt)) %>%
                     filter(catch >= mth.cut) %>% mutate(meth=as.character(meth))
  
  ctch <- tmp %>% filter(site == keep.site, date1 > tim.vec[1], date1 <= tim.vec[2]) %>% mutate(Method=ifelse(meth %in% mth.tmp$meth, as.character(meth), "Other"))
  
  ctch.tab <- ctch %>% group_by(sp_code, Method) %>% summarise(catch=sum(newwt)) %>% spread(key="Method", value="catch", fill=0)
  
  ctch.tab$Total <- apply(ctch.tab[, -1], 1, sum)
  
  ctch.tab %<>% arrange(desc(Total)) %>% as.data.frame()
  
  tmp1 <- ctch.tab[ctch.tab$Total >= cut.wgt,]
  tmp2 <- ctch.tab[ctch.tab$Total < cut.wgt,]
  
  if(dim(tmp2)[1] > 0){
    tmp2$sp_code <- "Other"
    tmp2 %<>% group_by(sp_code) %>% summarise_all(funs(sum))
  } else{
    
    tmp2 <- tmp1[1,]
    tmp2[] <- 0
    tmp2$sp_code <- "Other"
  }
  
  occs <- ctch %>% mutate(catgry=sppcds$form[match(sp_code, sppcds$sp_code)])
  occs$catgry <- ifelse(is.na(occs$catgry), "Coastal", occs$catgry)
  
  tmp3 <- occs %>% group_by(catgry, Method) %>% summarise(catch=sum(newwt)) %>% spread(key="Method", value="catch", fill=0) %>% rename(sp_code=catgry)
  tmp3$Total <- apply(tmp3[, -1], 1, sum)
  tmp3 %<>% arrange(desc(Total)) %>% as.data.frame()
  
  tmp4 <- data.frame(sp_code="Total", t(apply(ctch.tab[, -1], 2, sum)))
  
  pr.tab <- rbind(tmp1, tmp2, tmp3, tmp4) %>% mutate(Name=sppcds$plotnm[match(sp_code, sppcds$sp_code)], Name=ifelse(is.na(Name), sp_code, Name))
  
  if(any(names(pr.tab) == "Other")){
    sel.vec <- c("Name",names(pr.tab)[-match(c("Name","sp_code","Other","Total"),names(pr.tab))],"Other","Total")
  } else{
    sel.vec <- c("Name",names(pr.tab)[-match(c("Name","sp_code","Total"),names(pr.tab))],"Total")
  }
  pr.tab %<>% select(sel.vec)    # Reorder the columns
  
  # Convert to latex table and save
  pr.xtab <- xtable(pr.tab, align=c(rep("l", 2),rep("c", dim(pr.tab)[2]-1)), caption=captn, label=lbl, digits=0) 
  print.xtable(pr.xtab, file=sav.dir, include.rownames=FALSE, sanitize.text.function = function(x) x, size=txsz, caption.placement="top")     
  
  return(list(ctch=ctch, ctch.tab=ctch.tab, Prtab=pr.tab))
  
}