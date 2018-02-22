plot_meth_sppcomp <- function(dat=tmp, vrble="Troll", col.lst=sp_grp_col, ylb="", plot.tit="Troll"){
  
  tmp <- spp[spp$meth == vrble,][1:15,] %>% filter(!is.na(P))
  tmp$sp_code <- sppcds$plotnm[match(tmp$sp_code, sppcds$sp_code)]
  
  pl <- ggplot(tmp, aes(x=reorder(sp_code,P), y=P, fill=types)) + geom_bar(stat="identity", width=0.5) + ylim(0,1) + coord_flip() + ggtitle(plot.tit) +
               scale_fill_manual(name="types", values=col.lst) + ylab(ylb) + xlab("Species") +
               theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(),
               plot.title=element_text(size=20, hjust=0.5), legend.position=c(0.8,0.2))
  
  return(pl)
}