library(fplscrapR)
library(tidyverse)
library(extrafont)
#Load data
#df <- get_league(leagueid = 258563, leaguetype = "classic")


pd<-pd%>%ungroup()%>%mutate(round=round(as.numeric(round)))%>%
  mutate(name=ifelse(rank<6|player_name=="Hans Henrik Sievertsen",
         player_name," "),
         lpoints=ifelse(rank<6|player_name=="Hans Henrik Sievertsen",
                    points," "))%>%filter(round==2)
ggplot(pd,aes(x=reorder(rank,-rank),y=as.numeric(points)))+
    geom_bar(stat="identity",fill = "#74A089")+
   coord_flip()+theme_classic()+
  geom_text(aes(x=reorder(rank,-rank),y=50,label=name),colour="white",family="Comic Sans MS")+
  geom_text(aes(x=reorder(rank,-rank),y=points+5,label=lpoints),family="Comic Sans MS")+
  labs(title = '#EconTwitterPFP 20/21 standings after GW2',
       y="Points",x="Mini League Rank")+
  scale_x_discrete( expand = c(0, 0))+
  scale_y_continuous(limits=c(0,170), expand = c(0, 0))+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),
        text=element_text(family="Comic Sans MS"),
        panel.background = element_rect(fill =  'antiquewhite'),
        plot.background =  element_rect(fill =  'antiquewhite'))
  
  
  
  
