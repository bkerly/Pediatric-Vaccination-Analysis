# The purpose of this script is to load libraries and functions necssary for the rest of the analysis
 library(tidyverse)
 library(tigris)
 library(tidycensus)
 library(jsonlite)
 library(httr)
 library(ggmap) 
 library(lubridate)
 library(broom)
 library(ggthemes)
 library(glue)
 library(viridis)
 library(hrbrthemes)
 library(ggpubr)
 library(bslib)

 "%!in%" <- function(x,y)!('%in%'(x,y)) 
 
 berra <- function(expr){
   tryCatch(expr,
            error = function(e){
              message("An error occurred:\n", e)
            },
            warning = function(w){
              expr
              message("A warning occured:\n", w)
            },
            finally = {})
 }
 
 age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
   calc.age = lubridate::interval(dob, age.day) / duration(num = 1, units = units)
   if (floor) return(as.integer(floor(calc.age)))
   return(calc.age)
 }
 
 
 theme_covid <- function(){
   ggplot2::theme_minimal() +
     ggplot2::theme(
       # panel parameters
       panel.grid.major.x = ggplot2::element_blank(),
       panel.grid.major.y = ggplot2::element_blank(),
       panel.grid.minor.x = ggplot2::element_blank(),
       panel.grid.minor.y = ggplot2::element_blank(),
       # caption parameters
       plot.caption = ggplot2::element_text(hjust = 0, size = 8),
       plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
       plot.caption.position =  "plot",
       # title, axis text, and plot text parameters
       title = ggplot2::element_text(size = 14, colour = "#000000"),
       axis.title.x = ggplot2::element_text(face = "bold", size = 14),
       axis.title.y = ggplot2::element_text(face = "bold", size = 14, angle = 0,
                                            vjust = 0.5, hjust = 0),
       axis.text = ggplot2::element_text(size = 10, face = "bold"),
       text = ggplot2::element_text(size = 11, colour = "#000000"),
       # additional parameters
       legend.position = "none",
       axis.line = ggplot2::element_line(
         size = 0.2645833, colour =("#A6A6A6"))
       # axis.line.x = element_blank(),
       # axis.line.y = element_blank()
     )
 }