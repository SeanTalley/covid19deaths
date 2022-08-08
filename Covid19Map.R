library(usmap)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(stringr)
library(stringi)
library(scales)
library(rayshader)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

usa.dat <- read.csv("cd1.csv", header = T)

my_limits <- range(c(0, usa.dat[,50:ncol(usa.dat)]))
max_value <- max(usa.dat[, 50:ncol(usa.dat)])

#Final Frame - try other frames if you'd like
i <- 926

da <- data.frame(fips=usa.dat$countyFIPS,val=usa.dat[,i])
tot <- sum(da$val)
colour_breaks <- c(0,1,100,1000,5000,15000,44000)
colours <- c("#101010","#0D0887FF","#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF","#F0F921FF")

current_max <- max(da$val)
relative_max <- max_value / current_max

theDate <- substr(names(usa.dat)[i],2,100)
theMap <- plot_usmap(regions = "counties",
           data=da,
           values="val"
          ) + 
          labs(title=paste("Covid 19 Deaths - ",str_replace_all(theDate,"\\.","/")," - Total: ",comma(tot),sep='')) +
          scale_fill_gradientn(limits  = range(0, current_max),
          colours = colours,
          values = scales::rescale(colour_breaks, to = c(0, relative_max), from = c(0, max_value)),
          name="Deaths",na.value="#101010") + 
          theme(panel.background = element_rect(color = "#555555", fill = "#555555"))

#Feel free to tweak these values:
plot_gg(theMap, multicore = TRUE, width = 7, height = 7, fov = 70, scale = 1000)