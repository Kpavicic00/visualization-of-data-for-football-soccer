#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

#######################
#'    Name of leauge |  ', '    Expend |  ','    Income |  ', '    Balance |  ','    number of Season |  ',
#'    sum of Arrivlas |  ','    sum of Depatrues |  ', '    avg Expend of Arrivlas |  ','    avg Income of Depatrues |  ',
#'    avg Balance of Depatrues |  ','    avg Expend/Season |  ', '    avg Income/Season |  ','    avg Balance/Season |  '

liga <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GetDataForLeauge_AVG_Seasons.csv")

colnames(liga) <- c("Name_of_Legue","Expend","Income","Balance","number_of_Season",
                           "sum_of_Arrivlas ","sum_of_Depatrues","avg_Expend_of_Arrivlas ","avg_Income_of_Depatrues",
                           "avg_Income_of_Depatrues","avg_Expend_Season","avg_Income_Season","avg_Balance_Season ")
View(liga)