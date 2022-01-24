library(data.table)
library(scales)
library(magrittr)
library(tidyverse)
library(grid)
library(lubridate)
library(RColorBrewer)
library(forcats)



  dat <- read.csv("C:/Users/SamM/Downloads/Expl_Dat.csv", header = TRUE)

  dat$newdat <- dmy(dat$Dat)

  # If you make dat$Num1 a zero or 1, e.g. presence absence, I think it will make the calendar colours blue vs white. My example is a continuous one
  # You don't have to put values in for each of fills, textvl and textvl2 I don't think
  
  
  
calendar_plot_imputed <-
  function (dates = "newdat", fills = "Num1", textvl = "Num2", textvl2 = "Num3") 
  {
    require(forcats)
    
    months <- month.name
    
    mindate <- min(dat$newdat)
    maxdate <- ceiling_date(max(dat$newdat), "month") - days(1)
    
    filler <- tibble(date = seq(mindate, maxdate, by = "1 day"))
    
    t1 <- tibble(date = dat[, dates], fill = dat[, fills], txtval = dat[, textvl], txtval2 = dat[, textvl2]) %>% right_join(filler, by = "date") %>%
                 mutate(dow = as.numeric(format(date, "%w"))) %>% mutate(month = format(date, "%B")) %>%
                 mutate(woy = as.numeric(format(date, "%U"))) %>%
                 mutate(year = as.numeric(format(date, "%Y"))) %>% 
                 mutate(month = factor(month, levels = months, ordered = TRUE)) %>% 
                 arrange(year, month) %>% mutate(monlabel = month)
    
  
    if (length(unique(t1$year)) > 1) {
      t1$monlabel <- paste(t1$month, t1$year)
    }
    
    t2 <- t1 %>% mutate(monlabel = factor(monlabel, ordered = TRUE)) %>% 
                 mutate(monlabel = fct_inorder(monlabel)) %>%
                 mutate(monthweek = woy - min(woy), y = max(monthweek) - monthweek + 1)
    
    weekdays <- c("S", "M", "T", "W", "T", "F", "S")
    
    ggplot(t2, aes(dow, y)) + geom_tile(aes(fill=fill), color = "gray80") + geom_text(aes(label=txtval), size=3, nudge_x=0.2, nudge_y=-0.2) +
           facet_wrap(~monlabel, ncol = 3, scales = "free") + scale_x_continuous(expand = c(0, 0), position = "top", breaks = seq(0, 6), labels = weekdays) + 
           scale_y_continuous(expand = c(0, 0)) + geom_text(aes(label=round(txtval2)), size=3, nudge_x=-0.1, nudge_y=0.25, colour="white") +
           theme(panel.background = element_rect(fill = NA, color = NA), strip.background = element_rect(fill = NA, color = NA),
                 strip.text.x = element_text(hjust = 0, face = "bold"), 
                 legend.title = element_blank(), axis.ticks = element_blank(), 
                 axis.title = element_blank(), axis.text.y = element_blank(), 
                 strip.placement = "outsite")
  }

