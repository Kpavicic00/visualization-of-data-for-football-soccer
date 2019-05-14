#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

#1
#######################################################################################################
# league five with average and relative curves, average league consumption per player
# Expend_by_player

liga_petice <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Expend_BATCH.csv")
colnames(liga_petice) <- c("Name_of_Legue","Year","Nationality","Expend_by_player","Expend_INFLACION")

gr <- ggplot(liga_petice,aes(x = Year, y = Expend_by_player , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average and Relative  Expenditure for top five Leagues  League player consumption per player",color = " Names of Leagues\n") 

gr + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

gr +theme( axis.title=element_text(size=17,face="bold"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "red"))
#####################################################################################################

#2
#######################################################################################################
# league five with average and relative curves, average league consumption per player + INFLATION
# Expend_INFLACION

gr2 <- ggplot(liga_petice,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average and Relative  Expenditure for top five Leagues  League player consumption per player + INFLATION",color = " Names of Leagues\n") 

gr2 + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

gr2 +theme( axis.title=element_text(size=17,face="bold"),
            axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

#3
#####################################################################################################
# Expenditure with inflation and without inflation"

lige <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Expend.csv")
colnames(lige) <- c("Name_of_Legue","Year","Nationality","Expend_by_player","Expend_INFLACION")

g3 <- ggplot(lige, aes(Year)) +
  geom_smooth(aes(y = Expend_by_player, colour = "Expenditure without inflation"),lwd = 2, se = F) +
  geom_smooth(aes(y = Expend_INFLACION, colour = "Expenditure with inflation"),lwd = 2, se = F) +
  scale_colour_manual(values = c("orange4", "cyan4"))+
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Expenditure with inflation and without inflation",color = " Consumption\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

#4
#####################################################################################################
#the average cost of a league per player purchased

g4 <- ggplot(liga_petice, aes(x = Year,y = Expend_by_player, col = Name_of_Legue)) +
  geom_smooth(lwd = 2, se = F)+
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average Expenditure top five Leagues   without inflation",color = " Names of Leagues \n")

g4 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

#5
#####################################################################################################
#the average cost of a league per player purchased

g5 <-ggplot(liga_petice, aes(x = Year,y = Expend_by_player, col = Name_of_Legue)) +
  geom_line(aes(group = Name_of_Legue)) +
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_colour_manual(values = c("maroon4","green2","red4","brown","midnightblue")) +
  scale_x_continuous(" Year ")+
  labs(title = "Relative Expenditure top five Leagues   without inflation",color = " Names of Leagues \n")

g5 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#####################################################################################################


#6
#####################################################################################################
#the and relative average cost of a Serie A per player purchased


#########################################
# Seria A 
seriaA <- liga_petice %>%
  filter(Name_of_Legue == "SerieA")
#########################################
g_seriaA <- ggplot(seriaA,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkolivegreen")) +
  labs(title = "Average and Relative  Expenditure for Seria A  per player + INFLATION",color = " Names of Leagues\n") 

g_seriaA + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_seriaA +theme( axis.title=element_text(size=17,face="bold"),
            axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################
# PremierLeague 
Premierleague <- liga_petice %>%
  filter(Name_of_Legue == "PremierLeague")
#########################################
g_Premierleague <- ggplot(Premierleague,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("cyan4")) +
  labs(title = "Average and Relative  Expenditure for Premier League  per player + INFLATION",color = " Names of Leagues\n") 

g_Premierleague + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Premierleague +theme( axis.title=element_text(size=17,face="bold"),
                 axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                 plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################
# LaLiga
Laliga <- liga_petice %>%
  filter(Name_of_Legue == "LaLiga")
#########################################
g_Laliga <- ggplot(Laliga,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkorange3")) +
  labs(title = "Average and Relative  Expenditure for La Liga  per player + INFLATION",color = " Names of Leagues\n") 

g_Laliga + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Laliga +theme( axis.title=element_text(size=17,face="bold"),
                        axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################
# Bundesliga
Bundesligaa <- liga_petice %>%
  filter(Name_of_Legue == "Bundesliga")
#########################################
g_Bundesligaa <- ggplot(Bundesligaa,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkred")) +
  labs(title = "Average and Relative  Expenditure for Bundesliga  per player + INFLATION",color = " Names of Leagues\n") 

g_Bundesligaa + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Bundesligaa +theme( axis.title=element_text(size=17,face="bold"),
                 axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                 plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################
# Ligue1
Ligue1a <- liga_petice %>%
  filter(Name_of_Legue == "Ligue1")
#########################################

#####################################################################################################



















