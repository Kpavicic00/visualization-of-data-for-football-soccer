#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

liga_PETICE <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GETDataClubs_with_seasons_BATCH.csv")
colnames(liga_PETICE) <- c("Order","Club","State","Competition","Expenditures",
                    "Arrivals","Income","Departures","Balance","Season",
                    "Inflacion_Income","Inflacion_Expenditures","Inflacion_Balance")
View(liga_PETICE)


liga <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GETDataClubs_with_seasons.csv")

colnames(liga) <- c("Order","Club","State","Competition","Expenditures",
                    "Arrivals","Income","Departures","Balance","Season",
                    "Inflacion_Income","Inflacion_Expenditures","Inflacion_Balance")
View(liga)
# /home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GETDataClubs_with_seasons_BATCH.csv
liga_PREMIER <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GETDataClubs_with_seasons_BATCH.csv")
colnames(liga_PREMIER) <- c("Order","Club","State","Competition","Expenditures",
                    "Arrivals","Income","Departures","Balance","Season",
                    "Inflacion_Income","Inflacion_Expenditures","Inflacion_Balance")



# tesni treci
#############################################################


g_seriaA <- ggplot(premierLiga,aes(x = Season, y = Balance_INFLACION )) +
  geom_line(alpha = 0.9) +
  geom_smooth(lwd = 2, se = FALSE) +
  scale_y_continuous("Average Balance  per player",labels = scales::comma) +
  scale_colour_manual(values = c("darkolivegreen")) +
  labs(title = "Average and Relative  Balance for Seria A  per player + INFLATION",caption="Transfmarket.com",color = " Names of Leagues\n") 

g_seriaA + theme_tufte() + 
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_text(face = "bold", size = 12),)

g_seriaA +theme( axis.title=element_text(size=17,face="bold"),
                 axis.text = element_text(face = "bold", size = 17,,color = "gray16"),
                 plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#############################################################













# tesni drugi 

###########################

ggplot(liga_PREMIER, aes(x = Club, y = Inflacion_Balance)) + 
  labs(title=" Sum of Balance WITH Inflation for Nationality", 
       subtitle="sum of avg Balance  per player throught all seasons from 2000/2001 to day + Inflation for Nationality", 
       caption="Transfmarket.com")+
  xlab("Names of Nationality")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg Balance  per player",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))
# testni prvi 
########################################
testni <- ggplot(premierLiga,aes(x=Expenditures, y=Club)) +
  geom_point(aes(col=Arrivals, size=Expenditures)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n Expend ",
       y="Names of Leagues",
       subtitle="\t Expend realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Arrivals") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)





# galktikosi 
##########################################

# Seria A
SeriaA <- liga %>%
  filter(Competition == "SerieA")
View(SeriaA)

#Bundesliga
Bundesliga <- liga %>%
  filter(Competition == "Bundesliga")
View(Bundesliga)

# Ligue 1

Ligue_1 <- liga %>%
  filter(Competition == "Ligue1")
View(Ligue_1)
##################
# La Liga
La_Liga <- liga %>%
  filter(Competition == "LaLiga")
View(La_Liga)

# Premiership
Premiership <- liga %>%
  filter(Competition == "PremierLeague")
View(Premiership)
#####################

# AtléticoMadrid
# drugi tip grafa
#breaks=c(-100000000,-75000000,-50000000,-25000000,0)


testni <- ggplot(Bundesliga,aes(x=Season, y=Club)) +
  geom_point(aes(col=Club, size=Departures) ) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_size("Departures",labels = scales::comma) +
  labs(title="\n\n Departures ",
       y="Name of Club",
       subtitle="\t Departures for players ",
       caption="Transfmarket.com",color = "Clubs") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)


# SeriaA   Bundesliga Ligue_1
#  "Inflacion_Income","Inflacion_Expenditures","Inflacion_Balance" "Arrivals","Departures"
ggplot(Ligue_1, aes(x = Club, y = Inflacion_Income )) +
  labs(title=" Sum of Income for each Club", 
       subtitle="sum of Income for each Club through all seasons", 
       caption="Transfmarket.com")+
  xlab("Name of clubs ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of Income ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))


##########################################


