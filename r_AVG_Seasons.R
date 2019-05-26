#library

library(ggplot2)
library(maps)
library(ggridges)
library(dplyr)
library(ggthemes)
library(extrafont)



liga <- read.csv("/home/kristijan/github/FootballEvolcion/Datas/SaveData/save_csv_GetDataForLeauge_AVG_Seasons.csv")

colnames(liga) <- c("Name_of_Legue","Expend","Income","Balance","number_of_Season",
                           "sum_of_Arrivlas","sum_of_Depatrues","avg_Expend_of_Arrivlas","avg_Income_of_Depatrues",
                           "avg_Balance_of_Depatrues","avg_Expend_Season","avg_Income_Season","avg_Balance_Season")

View(liga_petice)



# 1.  =>>>>>>>>>>>>>>>>>> EXPEND <<<<<<<<<<<<<<<<<<<<<<<<<<=

# Expend through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=Expend, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=Expend)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n Expend ",
       y="Names of Leagues",
       subtitle="\t Expend realized by each league+ number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# Expend through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=Expend, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\nExpend ",
       y="Names of Leagues",
       subtitle="\t Expend realized by each league+ number of seasons",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# Expend through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = Expend )) +
  labs(title=" Sum of expendures for each League", 
       subtitle="sum of expendures throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of expendurs ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 2 . =>>>>>>>>>>>>>>>>>> INCOME <<<<<<<<<<<<<<<<<<<<<<<<<<=

# Income through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=Income, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=Income)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n Income ",
       y="Names of Leagues",
       subtitle="\t Income realized by each league + number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# Income through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=Income, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n Income ",
       y="Names of Leagues",
       subtitle="\t Income realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# Income through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = Income )) +
  labs(title=" Sum of Income for each League", 
       subtitle="sum of Income throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of Income ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################

# 3 . =>>>>>>>>>>>>>>>>>> BALANCE <<<<<<<<<<<<<<<<<<<<<<<<<<=

# Balance through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=Balance, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=Balance)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n Balance ",
       y="Names of Leagues",
       subtitle="\t Balance realized by each league + number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# Balance through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=Balance, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n Balance ",
       y="Names of Leagues",
       subtitle="\t Balance realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# Balance through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = Balance )) +
  labs(title=" Sum of Balance for each League", 
       subtitle="sum of Balance throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of Balance ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 4 . =>>>>>>>>>>>>>>>>>> sum_of_Arrivlas <<<<<<<<<<<<<<<<<<<<<<<<<<=

# sum_of_Arrivlas through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=sum_of_Arrivlas, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=sum_of_Arrivlas)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n Sum of Arrivlas ",
       y="Names of Leagues",
       subtitle="\t Sum of Arrivlas realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# sum_of_Arrivlas through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=sum_of_Arrivlas, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n sum_of_Arrivlas ",
       y="Names of Leagues",
       subtitle="\t sum_of_Arrivlas realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# sum_of_Arrivlas through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = sum_of_Arrivlas )) +
  labs(title=" Sum of  Arrivlas for each League", 
       subtitle="sum of Arrivlas throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of sum_of_Arrivlas ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 5 . =>>>>>>>>>>>>>>>>>> sum_of_Depatrues <<<<<<<<<<<<<<<<<<<<<<<<<<=

# sum_of_Depatrues through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=sum_of_Depatrues, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=sum_of_Depatrues)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n  Sum of Depatrues ",
       y="Names of Leagues",
       subtitle="\t  Sum of Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# sum_of_Depatrues through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=sum_of_Depatrues, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n Sum of Depatrues ",
       y="Names of Leagues",
       subtitle="\t Sum of Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# sum_of_Depatrues through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = sum_of_Depatrues )) +
  labs(title=" Sum of Depatrues for each League", 
       subtitle="sum of Depatrues throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of sum_of_Depatrues ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 6 . =>>>>>>>>>>>>>>>>>> avg_Expend_of_Arrivlas <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Expend_of_Arrivlas through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Expend_of_Arrivlas, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Expend_of_Arrivlas)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Expend_of_Arrivlas ",
       y="Names of Leagues",
       subtitle="\t avg_Expend_of_Arrivlas realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Expend_of_Arrivlas through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Expend_of_Arrivlas, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Expend_of_Arrivlas ",
       y="Names of Leagues",
       subtitle="\t avg_Expend_of_Arrivlas realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Expend_of_Arrivlas through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Expend_of_Arrivlas )) +
  labs(title=" Sum of avg_Expend_of_Arrivlas for each League", 
       subtitle="sum of avg_Expend_of_Arrivlas throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Expend_of_Arrivlas ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 7 . =>>>>>>>>>>>>>>>>>> avg_Income_of_Depatrues <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Income_of_Depatrues through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Income_of_Depatrues, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Income_of_Depatrues)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Income_of_Depatrues ",
       y="Names of Leagues",
       subtitle="\t avg_Income_of_Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Income_of_Depatrues through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Income_of_Depatrues, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Income_of_Depatrues ",
       y="Names of Leagues",
       subtitle="\t avg_Income_of_Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Income_of_Depatrues through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Income_of_Depatrues )) +
  labs(title=" Sum of avg_Income_of_Depatrues for each League", 
       subtitle="sum of avg_Income_of_Depatrues throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Income_of_Depatrues ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 8 . =>>>>>>>>>>>>>>>>>> avg_Balance_of_Depatrues <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Balance_of_Depatrues through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Balance_of_Depatrues, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Balance_of_Depatrues)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Balance_of_Depatrues ",
       y="Names of Leagues",
       subtitle="\t avg_Balance_of_Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Balance_of_Depatrues through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Balance_of_Depatrues, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Balance_of_Depatrues ",
       y="Names of Leagues",
       subtitle="\t avg_Balance_of_Depatrues realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Balance_of_Depatrues through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Balance_of_Depatrues )) +
  labs(title=" Sum of avg_Balance_of_Depatrues for each League", 
       subtitle="sum of avg_Balance_of_Depatrues throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Balance_of_Depatrues ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 9 . =>>>>>>>>>>>>>>>>>> avg_Expend_Season <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Expend_Season through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Expend_Season, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Expend_Season)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Expend_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Expend_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Expend_Season through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Expend_Season, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Expend_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Expend_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Expend_Season through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Expend_Season )) +
  labs(title=" Sum of avg_Expend_Season for each League", 
       subtitle="sum of avg_Expend_Season throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Expend_Season ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 10 . =>>>>>>>>>>>>>>>>>> avg_Income_Season <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Income_Season through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Income_Season, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Income_Season)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Income_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Income_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Income_Season through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Income_Season, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Income_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Income_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Income_Season through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Income_Season )) +
  labs(title=" Sum of avg_Income_Season for each League", 
       subtitle="sum of avg_Income_Season throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Income_Season ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################


# 11 . =>>>>>>>>>>>>>>>>>> avg_Balance_Season <<<<<<<<<<<<<<<<<<<<<<<<<<=

# avg_Balance_Season through league and season number => first type of plot
#############################################
testni <- ggplot(liga,aes(x=avg_Balance_Season, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=avg_Balance_Season)) + 
  geom_smooth(method="loess", se=F,col = "#123675") +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)+
  labs(title="\n\n avg_Balance_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Balance_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com",color = "Number of seasons") +
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))
plot(testni)

#############################################


# avg_Balance_Season through league and season number => SECOND type of plot
#############################################
ggplot(liga, aes(x=avg_Balance_Season, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8,col="#21471d") +
  scale_color_manual(values = "gray16")+ 
  geom_text(color="white", size=2) +
  labs(title="\n\n avg_Balance_Season ",
       y="Names of Leagues",
       subtitle="\t avg_Balance_Season realized by each league with the number of seasons ",
       caption="Transfmarket.com" )+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 24, face = "italic",color = "gray16"))

#############################################


# avg_Balance_Season through league and season number => THIRD type of plot
#############################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Balance_Season )) +
  labs(title=" Sum of avg_Balance_Season for each League", 
       subtitle="sum of avg_Balance_Season throught all seasons", 
       caption="Transfmarket.com")+
  xlab("Names of Leauges ")+
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  scale_y_continuous("sum of avg_Balance_Season ",labels = scales::comma) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),
        axis.title=element_text(size=10,face="bold",color = "gray16"),
        axis.text = element_text(face = "bold", size = 10,color = "gray16"),
        plot.title = element_text(size = 20, face = "italic",color = "gray16"))

#############################################