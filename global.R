###########################################
## Title: Schools                        ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
###########################################


##First file run - Environment Setup
## load necessary libraries
require(dplyr)
##require(sp)
require(maptools)
##require(rgeos)
require(Hmisc)
require(reshape2)
require(shiny)
#require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)
require(DT)
require(lubridate)
require(ggplot2)
require(reshape2)
library(plotly)



mod <- readRDS(file="mod.rds")
densmod <- readRDS(file="revi_100K_50K_2chain_spatialdensity.rds")
birdraw <- readRDS(file="revi.rds")
antennae <- readRDS(file="towers.rds")
towers <- antennae %>% group_by(site) %>% summarise_each(funs(first))
towers <- towers[-1,]
melt.towers <- melt(towers, measure=c("lat", "lon"))

track <- as.character(unique(mod$id))
