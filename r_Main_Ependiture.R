#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

liga_petice <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Expend_BATCH.csv")
colnames(liga_petice) <- c("Name_of_Legue","Year","Nationality","Expend_by_player","Expend_INFLACION")

# 1 verage and Relative  Expenditure for top five Leagues  League player consumption per player
#######################################################################################################
# league five with average and relative curves, average league consumption per player
# Expend_by_player

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

# 2 verage and Relative  Expenditure for top five Leagues  League player consumption per player + INFLATION
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

# 3 Expenditure with inflation and without inflation
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

# 4 Average Expenditure top five Leagues   without inflation
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

# 5 Relative Expenditure top five Leagues   without inflation 
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


# 6 Average Expenditure top five Leagues   WITH inflation -> Expend_INFLACION
#####################################################################################################
#the average cost of a league per player purchased

g4 <- ggplot(liga_petice, aes(x = Year,y = Expend_INFLACION, col = Name_of_Legue)) +
  geom_smooth(lwd = 2, se = F)+
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average Expenditure top five Leagues   WITH inflation",color = " Names of Leagues \n")

g4 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# 7 Relative Expenditure top five Leagues   WITH inflation -> Expend_INFLACION
#####################################################################################################
#the average cost of a league per player purchased

g5 <-ggplot(liga_petice, aes(x = Year,y = Expend_INFLACION, col = Name_of_Legue)) +
  geom_line(aes(group = Name_of_Legue)) +
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_colour_manual(values = c("maroon4","green2","red4","brown","midnightblue")) +
  scale_x_continuous(" Year ")+
  labs(title = "Relative Expenditure top five Leagues   WITH inflation",color = " Names of Leagues \n")

g5 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#####################################################################################################

# 8
#####################################################################################################
#the and relative average cost of a Serie A per player purchased


# Seria A 
#########################################
seriaA <- liga_petice %>%
  filter(Name_of_Legue == "SerieA")

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
#########################################
Premierleague <- liga_petice %>%
  filter(Name_of_Legue == "PremierLeague")

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
#########################################
Laliga <- liga_petice %>%
  filter(Name_of_Legue == "LaLiga")

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
#########################################
Bundesligaa <- liga_petice %>%
  filter(Name_of_Legue == "Bundesliga")

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
#########################################
Ligue1a <- liga_petice %>%
  filter(Name_of_Legue == "Ligue1")

g_Ligue1a <- ggplot(Ligue1a,aes(x = Year, y = Expend_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Expend  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkgreen")) +
  labs(title = "Average and Relative  Expenditure for Ligue 1  per player + INFLATION",color = " Names of Leagues\n") 

g_Ligue1a + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Ligue1a +theme( axis.title=element_text(size=17,face="bold"),
                      axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                      plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# load csv stat for Leagues 
###########################
sve_lige <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Expend.csv")
colnames(sve_lige) <- c("Name_of_Legue","Year","Nationality","Expend_by_player","Expend_INFLACION")
View(sve_lige)
###########################


#############################################
# expenditures WITHOUT Inflation per  LEAGUE
# sum of expenditures over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Name_of_Legue, y = Expend_by_player)) + 
  labs(title=" Sum of Expenditures", 
       subtitle="sum of avg Expend  per player throught all seasons from 2000/2001 to day ", 
       caption="Transfmarket.com")+
  xlab("Names of Leagues")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Expend  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################
#############################################
# expenditures WITH Inflation per  LEAGUE
# sum of expenditures over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Name_of_Legue, y = Expend_INFLACION)) + 
  labs(title=" Sum of Expenditures WITH Inflation", 
       subtitle="sum of avg Expend  per player throught all seasons from 2000/2001 to day + Inflation", 
       caption="Transfmarket.com")+
  xlab("Names of Leagues")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Expend  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################


#############################################
# expenditures WITHOUT Inflation per  Nationality
# sum of expenditures over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Nationality, y = Expend_by_player)) + 
  labs(title=" Sum of Expenditures for Nationality", 
       subtitle="sum of avg Expend  per player throught all seasons from 2000/2001 to day for Nationality", 
       caption="Transfmarket.com")+
  xlab("Names of Nationality")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Expend  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################
#############################################
# expenditures WITH Inflation per  Nationality
# sum of expenditures over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Nationality, y = Expend_INFLACION)) + 
  labs(title=" Sum of Expenditures WITH Inflation for Nationality", 
       subtitle="sum of avg Expend  per player throught all seasons from 2000/2001 to day + Inflation for Nationality", 
       caption="Transfmarket.com")+
  xlab("Names of Nationality")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Expend  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################












