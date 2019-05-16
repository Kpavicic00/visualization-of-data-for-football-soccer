#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)

liga <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GetBYyear.csv")


colnames(liga) <- c("Year","Expend","Income","Balance","number_of_Season",
                    "sum_of_Arrivlas","sum_of_Depatrues","avg_Expend_of_Arrivlas","avg_Income_of_Depatrues",
                    "avg_Balance_of_Depatrues","avg_Expend_Season","avg_Income_Season","avg_Balance_Season")
View(liga)

# 1. >>>>>>>>>>>>>>>>>> Expend , Income , Balance  
# relative 
#########################################################

g3 <- ggplot(liga, aes(Year)) +
  geom_line(aes(y = Expend, colour = "Expend"),lwd = 3) +
  geom_line(aes(y = Income, colour = "Income"),lwd = 3) +
  geom_line(aes(y = Balance, colour = "Balance"),lwd = 3) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Realative Expend <-> Income <-> Balance ",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# average  
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = Expend, colour = "Expend"),lwd = 3, se = F) +
  geom_smooth(aes(y = Income, colour = "Income"),lwd = 3, se = F) +
  geom_smooth(aes(y = Balance, colour = "Balance"),lwd = 3, se = F) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average  Expend <-> Income <-> Balance ",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################

# average  and relative 
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = Expend, colour = "Expend"),lwd = 3, se = F) +
  geom_smooth(aes(y = Income, colour = "Income"),lwd = 3, se = F) +
  geom_smooth(aes(y = Balance, colour = "Balance"),lwd = 3, se = F) +
  geom_line(aes(y = Expend, colour = "Expend"),lwd = 1) +
  geom_line(aes(y = Income, colour = "Income"),lwd = 1) +
  geom_line(aes(y = Balance, colour = "Balance"),lwd = 1) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average and Relative  Expend <-> Income <-> Balance ",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# 2. >>>>>>>>>>>>>>>>>> sum_of_Arrivlas , sum_of_Depatrues   
# relative 
#########################################################

g3 <- ggplot(liga, aes(Year)) +
  geom_line(aes(y = sum_of_Arrivlas, colour = "sum_of_Arrivlas"  ),lwd = 3) +
  geom_line(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3) +
  scale_colour_manual(values = c("orange4","#392168"))+
  scale_y_continuous(" Number of player ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Relative  sum of Arrivlas <-> sum of Depatrues",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# average  
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3, se = F) +
  geom_smooth(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3, se = F) +
  scale_colour_manual(values = c("orange4","#392168"))+
  scale_y_continuous(" Number of player ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average  sum of Arrivlas <-> sum of Depatrues",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################

# average  and relative 
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3, se = F) +
  geom_smooth(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3, se = F) +
  geom_line(aes(y = sum_of_Depatrues, colour = "sum_of_Depatrues"),lwd = 3) +
  scale_colour_manual(values = c("orange4","#392168"))+
  scale_y_continuous(" Number of player ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average and Relative sum of Arrivlas <-> sum of Depatrues",caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################

# 3. >>>>>>>>>>>>>>>>>> avg_Expend_of_Arrivlas , avg_Income_of_Depatrues, avg_Balance_of_Depatrues  
# relative 
#########################################################

g3 <- ggplot(liga, aes(Year)) +
  geom_line(aes(y = avg_Expend_of_Arrivlas, colour = "avg_Expend_of_Arrivlas"),lwd = 3) +
  geom_line(aes(y = avg_Income_of_Depatrues, colour = "avg_Income_of_Depatrues"),lwd = 3) +
  geom_line(aes(y = avg_Balance_of_Depatrues, colour = "avg_Balance_of_Depatrues"),lwd = 3) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Relative  for AVG Expend of Arrivals <==> AVG Income of Departures <==> AVG Balance of Departures",
       caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# average  
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = avg_Expend_of_Arrivlas, colour = "avg_Expend_of_Arrivlas"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Income_of_Depatrues, colour = "avg_Income_of_Depatrues"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Balance_of_Depatrues, colour = "avg_Balance_of_Depatrues"),lwd = 3, se = F) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average   for AVG Expend of Arrivals <==> AVG Income of Departures <==> AVG Balance of Departures"
       ,caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################

# average  and relative 
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = avg_Expend_of_Arrivlas, colour = "avg_Expend_of_Arrivlas"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Income_of_Depatrues, colour = "avg_Income_of_Depatrues"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Balance_of_Depatrues, colour = "avg_Balance_of_Depatrues"),lwd = 3, se = F) +
  geom_line(aes(y = avg_Expend_of_Arrivlas, colour = "avg_Expend_of_Arrivlas"),lwd = 1) +
  geom_line(aes(y = avg_Income_of_Depatrues, colour = "avg_Income_of_Depatrues"),lwd = 1) +
  geom_line(aes(y = avg_Balance_of_Depatrues, colour = "avg_Balance_of_Depatrues"),lwd = 1) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average and Relative for AVG Expend of Arrivals <==> AVG Income of Departures <==> AVG Balance of Departures"
       ,caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# 4. >>>>>>>>>>>>>>>>>> avg_Expend_Season , avg_Income_Season, avg_Balance_Season  
# relative 
#########################################################

g3 <- ggplot(liga, aes(Year)) +
  geom_line(aes(y = avg_Expend_Season, colour = "avg_Expend_Season"),lwd = 3) +
  geom_line(aes(y = avg_Income_Season, colour = "avg_Income_Season"),lwd = 3) +
  geom_line(aes(y = avg_Balance_Season, colour = "avg_Balance_Season"),lwd = 3) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Relative  for AVG Expend of Season <==> AVG Income of Season <==> AVG Balance of Season",
       caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################


# average  
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = avg_Expend_Season , colour = "avg_Expend_Season "),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Income_Season, colour = "avg_Income_Season"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Balance_Season, colour = "avg_Balance_Season"),lwd = 3, se = F) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average   for AVG Expend of Season <==> AVG Income of Season <==> AVG Balance of Season"
       ,caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################

# average  and relative 
#########################################################
g3 <- ggplot(liga, aes(Year)) +
  geom_smooth(aes(y = avg_Expend_Season , colour = "avg_Expend_Season "),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Income_Season, colour = "avg_Income_Season"),lwd = 3, se = F) +
  geom_smooth(aes(y = avg_Balance_Season, colour = "avg_Balance_Season"),lwd = 3, se = F) +
  geom_line(aes(y = avg_Expend_Season,colour = "avg_Expend_Season " ),lwd = 1) +
  geom_line(aes(y = avg_Income_Season, colour = "avg_Income_Season"),lwd = 1) +
  geom_line(aes(y = avg_Balance_Season, colour = "avg_Balance_Season"),lwd = 1) +
  scale_colour_manual(values = c("orange4", "cyan4","#392168"))+
  scale_y_continuous(" Investment ",labels = scales::comma)+
  scale_x_continuous(" Year ")+
  labs(title = "Average and Relative for AVG Expend of Season <==> AVG Income of Season <==> AVG Balance of Season"
       ,caption="Transfmarket.com",color = " Financial business\n")

g3 + theme( axis.title=element_text(size=17,face="bold",color = "gray16"),
            axis.text = element_text(face = "bold", size = 17,color = "gray16"),
            plot.title = element_text(size = 20, face = "italic",color = "gray16"))
#########################################################
