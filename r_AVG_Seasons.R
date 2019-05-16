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
                           "sum_of_Arrivlas ","sum_of_Depatrues","avg_Expend_of_Arrivlas","avg_Income_of_Depatrues",
                           "avg_Balance_of_Depatrues","avg_Expend_Season","avg_Income_Season","avg_Balance_Season")

View(liga)





# testiranje grafova 2 - > usmikat ga 
#################################
testni <- ggplot(liga,aes(x=Expend, y=Name_of_Legue)) +
  geom_point(aes(col=number_of_Season, size=Expend)) + 
  geom_smooth(method="loess", se=F) +
  scale_x_continuous(labels = scales::comma)+
  scale_size_continuous(labels = scales::comma)
  

plot(testni)

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)
##############


# testiranje grafova 1 - > usminkat ga 
#################################
# ovaj graf ostaje definitivno 
ggplot(liga, aes(x=Balance, y=Name_of_Legue, label=number_of_Season)) +
  geom_point(stat='identity',size=8) +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot")+
  scale_x_continuous(labels = scales::comma) 

#############################################
# 
############################################################################################################
ggplot(liga, aes(x = Name_of_Legue, y = avg_Balance_Season )) +
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
############################################################################################################