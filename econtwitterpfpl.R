library(fplscrapR)
library(tidyverse)
library(extrafont)
library(gghighlight)

################################# Update data ##########################################
#

  #Load data
  df <- get_league(leagueid = 258563, leaguetype = "classic")
  #setwd("C:\\Users\\hs17922\\Dropbox\\Private documents\\econtwitterfpl")
  # Modify the data
  new<-data.frame(player_name=df$standings$results$player_name,
                  points=df$standings$results$total, round=4)
  # Load old data
  old<-read_csv("fpldata.csv")
  ## Append
  combined<-rbind(old,new)
  # Save
  write_csv(combined,"fpldata.csv")

#################################  Create chart ###############################################
# Load
df<-read_csv("fpldata.csv")%>%
    arrange(round,points)%>%
    group_by(round)%>%
    mutate(rank=33-row_number())%>%
    group_by(player_name)%>%
    mutate(finalrank=last(rank),
           name=ifelse(finalrank<6,player_name,""),
           name=ifelse(player_name=="Hans Henrik Sievertsen", "Hans",name))
    
    

# Chart
p<-ggplot(df, aes(rank, group = player_name, 
         fill = as.factor(player_name), color = as.factor(player_name)))+
  geom_bar(stat="identity",aes(y = points,
                width = 0.9), alpha = 0.8, color = NA)+
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1,family="Comic Sans MS",size=4)+
  geom_text(aes(y = points, label =as.character(points)), vjust = 0.2, hjust = -.51,family="Comic Sans MS",size=4)+
  coord_flip(clip = "off", expand = FALSE)+
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_void()+
  theme(plot.title=element_text(size=16, hjust=0.5, colour="black", vjust=+2),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.margin = margin(2,2, 2, 4, "cm"),
        text=element_text(family="Comic Sans MS"),
        axis.line.y= element_line( size=.1, color="grey" ))+
  labs(title = "#EconTwitterPFL 20/21: GW{closest_state}")+  
  transition_states(round, transition_length = 4, state_length = 1,wrap = FALSE) +
  view_follow(fixed_x = TRUE) 

animate(p,  renderer = gifski_renderer(loop = FALSE))
anim_save("gw4.gif", p,  renderer = gifski_renderer(loop = FALSE))
  