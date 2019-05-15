#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

liga_petice <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Income_BATCH.csv")
colnames(liga_petice) <- c("Name_of_Legue","Year","Nationality","Income_by_player","Income_INFLACION")

# 1 verage and Relative  Income for top five Leagues  League player consumption per player
#######################################################################################################
# league five with average and relative curves, average league consumption per player
# Income_by_player

gr <- ggplot(liga_petice,aes(x = Year, y = Income_by_player , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average and Relative  Income for top five Leagues  League player consumption per player",caption="Transfmarket.com",color = " Names of Leagues\n") 

gr + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

gr +theme( axis.title=element_text(size=17,face="bold"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "#285b21"))
#####################################################################################################

# 2 verage and Relative  Income for top five Leagues  League player consumption per player + INFLATION
#######################################################################################################
# league five with average and relative curves, average league consumption per player + INFLATION
# Income_INFLACION

gr2 <- ggplot(liga_petice,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average and Relative  Incomes for top five Leagues  League player consumption per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

gr2 + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

gr2 +theme( axis.title=element_text(size=17,face="bold"),
            axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# 3 Income with inflation and without inflation
#####################################################################################################
# Income with inflation and without inflation"

lige <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_Income_BATCH.csv")
colnames(lige) <- c("Name_of_Legue","Year","Nationality","Income_by_player","Income_INFLACION")

g3 <- ggplot(lige, aes(Year)) +
  geom_smooth(aes(y = Income_by_player, colour = "Income without inflation"),lwd = 2, se = F) +
  geom_smooth(aes(y = Income_INFLACION, colour = "Income with inflation"),lwd = 2, se = F) +
  scale_colour_manual(values = c("orange4", "cyan4"))+
  scale_y_continuous(" Expend ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Income with inflation and without inflation",caption="Transfmarket.com",color = " Earned\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# 4 Average Income top five Leagues   without inflation
#####################################################################################################
#the average cost of a league per player purchased

g4 <- ggplot(liga_petice, aes(x = Year,y = Income_by_player, col = Name_of_Legue)) +
  geom_smooth(lwd = 2, se = F)+
  scale_y_continuous(" Income ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average Income top five Leagues   without inflation",caption="Transfmarket.com",color = " Names of Leagues \n")

g4 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# 5 Relative Income top five Leagues   without inflation 
#####################################################################################################
#the average cost of a league per player purchased

g5 <-ggplot(liga_petice, aes(x = Year,y = Income_by_player, col = Name_of_Legue)) +
  geom_line(aes(group = Name_of_Legue)) +
  scale_y_continuous(" Income ",labels = scales::comma)+
  scale_colour_manual(values = c("maroon4","green2","red4","brown","midnightblue")) +
  scale_x_continuous(" Year ")+
  labs(title = "Relative Income top five Leagues   without inflation",caption="Transfmarket.com",color = " Names of Leagues \n")

g5 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#####################################################################################################


# 6 Average Income top five Leagues   WITH inflation -> Income_INFLACION
#####################################################################################################
#the average cost of a league per player purchased

g4 <- ggplot(liga_petice, aes(x = Year,y = Income_INFLACION, col = Name_of_Legue)) +
  geom_smooth(lwd = 2, se = F)+
  scale_y_continuous(" Income ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  scale_colour_manual(values = c("gray15", "orange4", "cyan4","darkolivegreen","red4")) +
  labs(title = "Average Income top five Leagues   WITH inflation",caption="Transfmarket.com",color = " Names of Leagues \n")

g4 + theme(axis.title=element_text(size=17,face="bold",color = "gray16"),
           axis.text = element_text(face = "bold", size = 17,color = "gray16"),
           plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################

# 7 Relative Income top five Leagues   WITH inflation -> Income_INFLACION
#####################################################################################################
#the average cost of a league per player purchased

g5 <-ggplot(liga_petice, aes(x = Year,y = Income_INFLACION, col = Name_of_Legue)) +
  geom_line(aes(group = Name_of_Legue)) +
  scale_y_continuous(" Income ",labels = scales::comma)+
  scale_colour_manual(values = c("maroon4","green2","red4","brown","midnightblue")) +
  scale_x_continuous(" Year ")+
  labs(title = "Relative Income top five Leagues   WITH inflation",caption="Transfmarket.com",color = " Names of Leagues \n")

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

g_seriaA <- ggplot(seriaA,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkolivegreen")) +
  labs(title = "Average and Relative  Income for Seria A  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

g_seriaA + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_seriaA +theme( axis.title=element_text(size=17,face="bold"),
                 axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                 plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#####################################################################################################
# Premier League 
#########################################
Premierleague <- liga_petice %>%
  filter(Name_of_Legue == "PremierLeague")

g_Premierleague <- ggplot(Premierleague,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("cyan4")) +
  labs(title = "Average and Relative  Income for Premier League  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

g_Premierleague + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Premierleague +theme( axis.title=element_text(size=17,face="bold"),
                        axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#####################################################################################################
# La Liga
#########################################
Laliga <- liga_petice %>%
  filter(Name_of_Legue == "LaLiga")

g_Laliga <- ggplot(Laliga,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkorange3")) +
  labs(title = "Average and Relative  Income for La Liga  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

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

g_Bundesligaa <- ggplot(Bundesligaa,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkred")) +
  labs(title = "Average and Relative  Income for Bundesliga  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

g_Bundesligaa + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_Bundesligaa +theme( axis.title=element_text(size=17,face="bold"),
                      axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                      plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#####################################################################################################
# Ligue 1
#########################################
Ligue1a <- liga_petice %>%
  filter(Name_of_Legue == "Ligue1")

g_Ligue1a <- ggplot(Ligue1a,aes(x = Year, y = Income_INFLACION , col = Name_of_Legue)) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Income  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkgreen")) +
  labs(title = "Average and Relative  Income for Ligue 1  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

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
colnames(sve_lige) <- c("Name_of_Legue","Year","Nationality","Income_by_player","Income_INFLACION")
View(sve_lige)
###########################


#############################################
# Income WITHOUT Inflation per  LEAGUE
# sum of expenditures over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Name_of_Legue, y = Income_by_player)) + 
  labs(title=" Sum of Income", 
       subtitle="sum of avg Income  per player throught all seasons from 2000/2001 to day ", 
       caption="Transfmarket.com")+
  xlab("Names of Leagues")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Income  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################
#############################################
# Income WITH Inflation per  LEAGUE
# sum of Income over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Name_of_Legue, y = Income_INFLACION)) + 
  labs(title=" Sum of Income WITH Inflation", 
       subtitle="sum of avg Income  per player throught all seasons from 2000/2001 to day + Inflation", 
       caption="Transfmarket.com")+
  xlab("Names of Leagues")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Income  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################


#############################################
# Income WITHOUT Inflation per  Nationality
# sum of Income over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Nationality, y = Income_by_player)) + 
  labs(title=" Sum of Income for Nationality", 
       subtitle="sum of avg Income  per player throught all seasons from 2000/2001 to day for Nationality", 
       caption="Transfmarket.com")+
  xlab("Names of Nationality")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Income  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################
#############################################
# Income WITH Inflation per  Nationality
# sum of Income over the period from the 2000/2001 season until the 2018/2019 season, 
# where the average consumption of the player per player ranged throughout that period
############################################################################################################
ggplot(sve_lige, aes(x = Nationality, y = Income_INFLACION)) + 
  labs(title=" Sum of Income WITH Inflation for Nationality", 
       subtitle="sum of avg Income  per player throught all seasons from 2000/2001 to day + Inflation for Nationality", 
       caption="Transfmarket.com")+
  xlab("Names of Nationality")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Income  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
############################################################################################################
