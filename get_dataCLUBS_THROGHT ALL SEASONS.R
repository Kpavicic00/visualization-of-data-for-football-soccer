#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)


liga_throught <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GetDate_for_Clubs_throught_all_seasons.csv")

colnames(liga_throught) <- c("Order_of_Expend","Club","State","Competition","Expenditures",
                             "Income","Arrivals","Departures","Balance",
                             "inflation_Expenditure","inflation_Income","inflation_Balance")
View(liga_throught)