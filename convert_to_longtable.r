convert_to_longtable <- function(flpath=paste0(dirpth, "/Plots/Mean_Wt_tab.tex"), alignmt="llllcc"){
  
  dat <- readLines(flpath)   # Pull in the .tex table as a vector of characters
  
  # Modify head area
  dat[2] <- "\\begin{tiny}"
  dat[grep("centering", dat)] <- "%centering"
  dat[grep("begingroup", dat)] <- "%\\begingroup"
  dat[grep("begin\\{table", dat)] <- paste0("\\begin{longtable}{", alignmt,"}")
  dat[grep("begin\\{tabular", dat)] <- "%\\begin{tabular}"
  dat[grep("hline", dat)[2]] <- "\\hline \\endhead \\hline \\endfoot"
  
  # Modify tail area
  dat[grep("end\\{tabular", dat)] <- "%\\end{tabular}"
  dat[grep("endgroup", dat)] <- "%\\endgroup"
  dat[grep("end\\{table", dat)] <- "\\end{longtable} \\end{tiny}"
  
  writeLines(dat, flpath)
  
  return(dat)
  
}







