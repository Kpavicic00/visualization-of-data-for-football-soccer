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

petica_La_liga <- liga_throught %>%
  filter(Competition == "LaLiga")
View(petica_La_liga)
fill <- "#4271AE"
ggplot(petica_La_liga, aes(x = Club, y = Departures)) + 
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
