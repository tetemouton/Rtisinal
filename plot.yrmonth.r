plot.yrmonth <- function(dat=tmp1, xcl="yrmon", ycl="N", flcl=NULL, faccl="site", xlbl="Year-month", ylbl="Number of unique vessels", txsz=16,
                         ticknm=ticknm, flcols=NULL, legpos=c(0.95,0.85), postype="stack"){
  
  
  if(is.null(flcl)){
    
    pl <- ggplot(dat, aes_string(x=xcl, y=ycl)) + geom_bar(stat="identity", fill=alpha("dodgerblue3",0.7), colour="black")
    
  } else {
    
    pl <- ggplot(dat, aes_string(x=xcl, y=ycl, fill=flcl)) + geom_bar(position=postype, stat="identity", colour="black") +
                 theme(legend.position=legpos, legend.title=element_blank()) + scale_fill_manual(values=flcols)
    
  }
  
  if(!is.null(faccl)) pl <- pl + facet_wrap(as.formula(paste("~", faccl)), ncol = 1)
  
  pl <- pl + xlab(xlbl) + ylab(ylbl) + scale_x_discrete(labels=ticknm) +
             theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   text=element_text(size=txsz), axis.text.x=element_text(angle=90, hjust=1))
  
  
  return(pl)
}


