#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)



liga <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GETDataClubs_with_seasons.csv")

#'    Order |  ', '    Club |  ','    State |  ', '    Competition |  ','    Expenditures |  ',
#'    Arrivals |  ','    Income  |  ', '    Departures |  ','    Balance |  ','    Season |  ',
#' Inflacion + Income |  ',' Inflacion + Expenditures |  ',' Inflacion + Balance |  
#' 
colnames(liga) <- c("Order","Club","State","Competition","Expenditures",
                    "Arrivals","Income","Departures","Balance","Season",
                    "Inflacion_Income","Inflacion_Expenditures","Inflacion_Balance")
View(liga)