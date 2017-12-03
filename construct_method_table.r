construct_method_table <- function(fldir="C:/Tokelau_Artisinal/Data/Fishing_Methods_Table.csv", svdir="C:/Tokelau_Artisinal/Plots/",
                                   captn="Blah blah", lbl="tab:fishmeth", txsz="normalsize"){
  
  tmp <- read.csv(fldir, header=TRUE, stringsAsFactors=FALSE)
  
  tmp %<>% rename(Code=Ident, "Full name"=Longnm, Plot=Shortnm)
  
  tmp$Plot <- str_replace_all(tmp$Plot, fixed("_"), "\\_")
  
  # Convert to latex table and save
  pr.xtab <- xtable(tmp, align=c("c","c","l","l"), caption=captn, label=lbl, digits=0) 
  print.xtable(pr.xtab, file=paste0(svdir, "Fish_meth_tab.tex"), include.rownames=FALSE, sanitize.text.function = function(x) x, size=txsz, caption.placement="top") 
  
}

