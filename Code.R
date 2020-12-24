# Used Packages
library(tidyverse)#data
library(dplyr)#data
library(haven)#data
library(tidyr)#data
library(knitr)#data
library(kableExtra)#data
library(readr)#data
library(AICcmodavg)#nice plot summary tables
library(broom)#data
library(corrr)#model
library(brms)#model
library(modelr)#data
library(ggplot2)#data
library(sjPlot)

library(ggpubr)#data
### Data ###
# Import Data
games_stats <- read_csv("nba.games.stats.csv")
midgame<- read_csv("NBA.csv")
head(games_stats)

### Clean Data ###
## Midgame Data ##
# Create third quarter point column
midgame<-midgame%>%
  mutate(third_quarter_score = select(., Q1:Q3) %>% rowSums(na.rm = TRUE))

# take only season 2014 for data training purposes
midgame_2014<-midgame%>%
  filter(Season==14)%>%
  select(Date,
         Team,
         third_quarter_score)

## Game Stats Data ##
# Create point spread variable column
games_stats <- games_stats%>%
  mutate(PointSpread=TeamPoints-OpponentPoints)

games_stats <- games_stats%>%
  mutate(DateNum=as.numeric(gsub("-","",Date)))

games_stats <- games_stats%>%
  mutate_at(vars(WINorLOSS), .funs = 
              funs(case_when(.=="W"~1,
                             .=="L"~0)))

# Append chosen predictors to a new dataframe
stats_2014 <- 
  games_stats %>% 
  select(Team, 
         Opponent, 
         Home,
         Game, 
         Date, 
         WINorLOSS,
         PointSpread,
         TeamPoints, 
         OpponentPoints, 
         FieldGoalPercent = FieldGoals., 
         OppFieldGoalPercent = Opp.FieldGoals., 
         ThreePointPercent = X3PointShots., 
         OppThreePointPercent = Opp.3PointShots.,
         FreeThrowPercent = FreeThrows.,
         OppFreeThrowPercent = Opp.FreeThrows.,
         TotalRebounds,
         OppTotalRebound = Opp.TotalRebounds,
         Turnovers,
         OppTurnover = Opp.Turnovers)%>%
  filter(games_stats$DateNum>=20141028& games_stats$DateNum<=20150616) # filter home game only to avoid repetition


## Trying to adjust model
## From the lm() function with raw data, we find that there is a relationship
## between the dependent and independent vars. However, we will not have access 
## the predictors in prior to the game. Therefore, we will have to predict 
## each independent variable using the game stats we already have

# Add cumulative field goal average for each game played as a part of the data
stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgTeamPoints=(cumsum(TeamPoints)-TeamPoints)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppPoints=(cumsum(OpponentPoints)-OpponentPoints)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgFieldGoalPercent=(cumsum(FieldGoalPercent)-FieldGoalPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppFieldGoalPercent=(cumsum(OppFieldGoalPercent)-OppFieldGoalPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgThreePointPercent=(cumsum(ThreePointPercent)-ThreePointPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppThreePointPercent=(cumsum(OppThreePointPercent)-OppThreePointPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgFreeThrowPercent=(cumsum(FreeThrowPercent)-FreeThrowPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppFreeThrowPercent=(cumsum(OppFreeThrowPercent)-OppFreeThrowPercent)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgTotalRebounds=(cumsum(TotalRebounds)-TotalRebounds)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppTotalRebounds=(cumsum(OppTotalRebound)-OppTotalRebound)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgTurnovers=(cumsum(Turnovers)-Turnovers)/(Game-1))

stats_2014<-stats_2014%>%
  group_by(Team)%>%
  mutate(AvgOppTurnovers=(cumsum(OppTurnover)-OppTurnover)/(Game-1))

## Merge two data together ##
mid_2014<-merge(stats_2014, midgame_2014, 
                by.x = c("Date", "Team"),
                by.y = c("Date", "Team"))

mid_2014<-merge(mid_2014, midgame_2014,
                by.x = c("Date", "Opponent"),
                by.y = c("Date", "Team"))
# rename
mid_2014<-mid_2014%>%
  rename(TeamThirdQuarter=third_quarter_score.x,
         OppThirdQuarter=third_quarter_score.y)

# Add point spread third quarter
ThirdQuarterPointSpread<-mid_2014$TeamThirdQuarter-mid_2014$OppThirdQuarter
mid_2014<-cbind(mid_2014, ThirdQuarterPointSpread)

# select only second half season data for accuracy, 
# as we do not know a lot about the teams until midseason
mid_2014<- mid_2014%>%
  filter(Game>41)


# Data Preview
knitr::kable(head(stats_2014[1:10]), format="latex", booktabs=TRUE, 
             caption = "NBA Team Game Stats from 2014 to 2018 - 2014 data preview") %>% 
  kable_styling(latex_options="scale_down")

knitr::kable(head(stats_2014[11:17]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")

knitr::kable(head(stats_2014[18:24]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")

knitr::kable(head(stats_2014[25:30]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")

# Second dataset preview
knitr::kable(head(midgame_2014), 
             caption = "1996-2019 NBA Stats Complete With Player Stats - 2014 data preview")


### Graph

# Total Point Distribution

p1<-stats_2014%>%
  subset(Team == "ATL")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="ATL ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p2<-stats_2014%>%
  subset(Team == "BOS")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="BOS ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p3<-stats_2014%>%
  subset(Team == "BRK")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="BRK ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p4<-stats_2014%>%
  subset(Team == "CHO")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="CHO ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p5<-stats_2014%>%
  subset(Team == "CHI")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="CHI ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p6<-stats_2014%>%
  subset(Team == "CLE")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="CLE ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p7<-stats_2014%>%
  subset(Team == "DAL")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="DAL ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p8<-stats_2014%>%
  subset(Team == "DEN")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="DEN ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p9<-stats_2014%>%
  subset(Team == "DET")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="DET ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p10<-stats_2014%>%
  subset(Team == "GSW")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="GSW ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p11<-stats_2014%>%
  subset(Team == "HOU")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="HOU ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p12<-stats_2014%>%
  subset(Team == "IND")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="IND ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p13<-stats_2014%>%
  subset(Team == "LAC")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="LAC ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p14<-stats_2014%>%
  subset(Team == "LAL")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="LAL ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p15<-stats_2014%>%
  subset(Team == "MEM")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="MEM ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p16<-stats_2014%>%
  subset(Team == "MIA")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="MIA  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p17<-stats_2014%>%
  subset(Team == "MIL")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="MIL",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p18<-stats_2014%>%
  subset(Team == "MIN")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="MIN ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p19<-stats_2014%>%
  subset(Team == "NOP")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="NOP ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p20<-stats_2014%>%
  subset(Team == "NYK")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="NYK ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p21<-stats_2014%>%
  subset(Team == "OKC")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="OKC ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p22<-stats_2014%>%
  subset(Team == "ORL")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="ORL  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p23<-stats_2014%>%
  subset(Team == "PHI")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="PHI  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p24<-stats_2014%>%
  subset(Team == "PHO")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="PHO",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

p25<-stats_2014%>%
  subset(Team == "POR")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="POR  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p26<-stats_2014%>%
  subset(Team == "SAC")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="SAC  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p27<-stats_2014%>%
  subset(Team == "SAS")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="SAS  ",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p28<-stats_2014%>%
  subset(Team == "TOR")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="TOR",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p29<-stats_2014%>%
  subset(Team == "UTA")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  labs(title="UTA",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


p30<-stats_2014%>%
  subset(Team == "WAS")%>%
  ggplot(aes(x=TeamPoints))+
  geom_bar(color="blue", fill="white")+
  labs(title="WAS",
       x ="Team Points Distribution", y = "Frequency")+
  theme_light()+
  scale_x_continuous(breaks = seq(min(stats_2014$TeamPoints), 
                                  max(stats_2014$TeamPoints), 
                                  by = 10))+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,
          p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,
          ncol = 6, nrow = 5)


# Average Trend
ca1<-stats_2014%>%
  subset(Team == "ATL")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="ATL",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca2<-stats_2014%>%
  subset(Team == "BOS")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="BOS",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca3<-stats_2014%>%
  subset(Team == "BRK")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="BRK",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca4<-stats_2014%>%
  subset(Team == "CHI")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="CHI",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca5<-stats_2014%>%
  subset(Team == "CHO")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="CHO",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca6<-stats_2014%>%
  subset(Team == "CLE")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="CLE",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca7<-stats_2014%>%
  subset(Team == "DAL")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="DAL",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca8<-stats_2014%>%
  subset(Team == "DEN")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="DEN",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca9<-stats_2014%>%
  subset(Team == "DET")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="DET",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca10<-stats_2014%>%
  subset(Team == "GSW")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="GSW",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


ca11<-stats_2014%>%
  subset(Team == "HOU")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="HOU",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca12<-stats_2014%>%
  subset(Team == "IND")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="IND",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca13<-stats_2014%>%
  subset(Team == "LAC")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="LAC",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca14<-stats_2014%>%
  subset(Team == "LAL")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="LAL",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca15<-stats_2014%>%
  subset(Team == "MEM")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="MEM",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca16<-stats_2014%>%
  subset(Team == "MIA")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="MIA",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca17<-stats_2014%>%
  subset(Team == "MIL")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="MIL",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca18<-stats_2014%>%
  subset(Team == "MIN")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="MIN",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca19<-stats_2014%>%
  subset(Team == "NOP")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="NOP",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca20<-stats_2014%>%
  subset(Team == "NYK")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="NYK",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca21<-stats_2014%>%
  subset(Team == "OKC")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="OKC",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca22<-stats_2014%>%
  subset(Team == "ORL")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="ORL",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca23<-stats_2014%>%
  subset(Team == "PHI")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="PHI",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca24<-stats_2014%>%
  subset(Team == "PHO")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="PHO",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca25<-stats_2014%>%
  subset(Team == "POR")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="POR",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca26<-stats_2014%>%
  subset(Team == "SAC")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="SAC",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca27<-stats_2014%>%
  subset(Team == "SAS")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="SAS",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca28<-stats_2014%>%
  subset(Team == "TOR")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="TOR",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca29<-stats_2014%>%
  subset(Team == "UTA")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="UTA",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ca30<-stats_2014%>%
  subset(Team == "WAS")%>%
  ggplot(aes(x=Game, y= AvgTeamPoints))+ geom_path()+
  theme_light()+
  labs(title="WAS",
       x ="Game", y = "Average Team Points")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ggarrange(ca1,ca2,ca3,ca4,ca5,ca6,ca7,ca8,ca9,ca10,ca11,ca12,ca13,ca14,ca15,ca16,ca17,ca18,
          ca19,ca20,ca21,ca22,ca23,ca24,ca25,ca26,ca27,ca28,ca29,ca30,
          ncol = 6, nrow = 5)


### Model

library(pander) #for summary stats tables
## Use current Data to see if model fits
modelx<- lm(PointSpread~ Team+Opponent+
              ThirdQuarterPointSpread+
              FieldGoalPercent+OppFieldGoalPercent+
              ThreePointPercent+OppThreePointPercent+
              FreeThrowPercent+OppFreeThrowPercent+
              TotalRebounds+OppTotalRebound+
              Turnovers+OppTurnover, mid_2014)


pander(summary(modelx), caption = "Test Ideal Model Fit Summary")

### Now its time for our real model:)
cumsum_model<-lm(PointSpread~0+Team+Opponent+ThirdQuarterPointSpread+
                   AvgFieldGoalPercent+AvgOppFieldGoalPercent+
                   AvgThreePointPercent+AvgOppThreePointPercent+
                   AvgFreeThrowPercent+AvgOppFreeThrowPercent+
                   AvgTotalRebounds+AvgOppTotalRebounds+
                   AvgTurnovers+AvgOppTurnovers, mid_2014)

pander(summary(cumsum_model), caption = "Real Model Fit Summary")


### Resukts
# Check Linear 
x1<-mid_2014%>%
  ggplot(aes(x=ThirdQuarterPointSpread, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Third Quarter Point Spread vs. Point Spread",
       x ="Third Quarter Point Spread", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

x2<-mid_2014%>%
  ggplot(aes(x=AvgFieldGoalPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Field Goal Percent",
       x ="Average Field Goal Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x3<-mid_2014%>%
  ggplot(aes(x=AvgOppFieldGoalPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Avg Opponent Field Goal Percent",
       x ="Avg Opponent Field Goal Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

x4<-mid_2014%>%
  ggplot(aes(x=AvgThreePointPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Three Point Percent",
       x ="Average Three Point Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x5<-mid_2014%>%
  ggplot(aes(x=AvgOppThreePointPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Three Point Percent",
       x ="Average Opponent Three Point Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x6<-mid_2014%>%
  ggplot(aes(x=AvgFreeThrowPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Free Throw Percent",
       x ="Average Free Throw Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x7<-mid_2014%>%
  ggplot(aes(x=AvgOppFreeThrowPercent, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Opponent Free Throw Percent",
       x ="Average Opponent Free Throw Percent", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))



x8<-mid_2014%>%
  ggplot(aes(x=AvgTotalRebounds, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Total Rebounds",
       x ="Average Total Rebounds", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x9<-mid_2014%>%
  ggplot(aes(x=AvgOppTotalRebounds, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Opponent Total Rebounds",
       x ="Average Opponent Total Bebounds", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


x10<-mid_2014%>%
  ggplot(aes(x=AvgTurnovers, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Total Rebounds",
       x ="Average Total Rebounds", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))



x11<-mid_2014%>%
  ggplot(aes(x=AvgOppTurnovers, y=PointSpread))+geom_point()+
  theme_light()+
  labs(title="Average Opponent Total Rebounds",
       x ="Average Opponent Total Rebounds", y = "Point Spread")+
  theme(axis.text.x = element_text(face="bold",
                                   size=30),
        axis.text.y = element_text(face="bold", 
                                   size=30))+
  theme(plot.title = element_text(face = "bold", size=40))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


ggarrange(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,
          ncol = 3, nrow = 4)


# Regression diagnostics
par(mfrow = c(2, 2))
plot(cumsum_model)

# 2015 data model verification
# make the 2015 data the same as 2014 above
midgame<-midgame%>%
  mutate(third_quarter_score = select(., Q1:Q3) %>% rowSums(na.rm = TRUE))

midgame_2015<-midgame%>%
  filter(Season==15)%>%
  select(Date,
         Team,
         third_quarter_score)

games_stats <- games_stats%>%
  mutate(PointSpread=TeamPoints-OpponentPoints)

games_stats <- games_stats%>%
  mutate(DateNum=as.numeric(gsub("-","",Date)))

games_stats <- games_stats%>%
  mutate_at(vars(WINorLOSS), .funs = 
              funs(case_when(.=="W"~1,
                             .=="L"~0)))

stats_2015 <- 
  games_stats %>% 
  select(Team, 
         Opponent, 
         Home,
         Game, 
         Date, 
         WINorLOSS,
         PointSpread,
         TeamPoints, 
         OpponentPoints, 
         FieldGoalPercent = FieldGoals., 
         OppFieldGoalPercent = Opp.FieldGoals., 
         ThreePointPercent = X3PointShots., 
         OppThreePointPercent = Opp.3PointShots.,
         FreeThrowPercent = FreeThrows.,
         OppFreeThrowPercent = Opp.FreeThrows.,
         TotalRebounds,
         OppTotalRebound = Opp.TotalRebounds,
         Turnovers,
         OppTurnover = Opp.Turnovers)%>%
  filter(games_stats$DateNum>=20151027& games_stats$DateNum<=20160619)

# Add cumulative field goal average for each game played as a part of the data
stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgTeamPoints=(cumsum(TeamPoints)-TeamPoints)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppPoints=(cumsum(OpponentPoints)-OpponentPoints)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgFieldGoalPercent=(cumsum(FieldGoalPercent)-FieldGoalPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppFieldGoalPercent=(cumsum(OppFieldGoalPercent)-OppFieldGoalPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgThreePointPercent=(cumsum(ThreePointPercent)-ThreePointPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppThreePointPercent=(cumsum(OppThreePointPercent)-OppThreePointPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgFreeThrowPercent=(cumsum(FreeThrowPercent)-FreeThrowPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppFreeThrowPercent=(cumsum(OppFreeThrowPercent)-OppFreeThrowPercent)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgTotalRebounds=(cumsum(TotalRebounds)-TotalRebounds)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppTotalRebounds=(cumsum(OppTotalRebound)-OppTotalRebound)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgTurnovers=(cumsum(Turnovers)-Turnovers)/(Game-1))

stats_2015<-stats_2015%>%
  group_by(Team)%>%
  mutate(AvgOppTurnovers=(cumsum(OppTurnover)-OppTurnover)/(Game-1))



## Merge two data together ##
mid_2015<-merge(stats_2015, midgame_2015, 
                by.x = c("Date", "Team"),
                by.y = c("Date", "Team"))

mid_2015<-merge(mid_2015, midgame_2015,
                by.x = c("Date", "Opponent"),
                by.y = c("Date", "Team"))
# rename
mid_2015<-mid_2015%>%
  rename(TeamThirdQuarter=third_quarter_score.x,
         OppThirdQuarter=third_quarter_score.y)

# Add point spread third quarter
ThirdQuarterPointSpread<-mid_2015$TeamThirdQuarter-mid_2015$OppThirdQuarter
mid_2015<-cbind(mid_2015, ThirdQuarterPointSpread)

# select only second half season data 
mid_2015<- mid_2015%>%
  filter(Game>41)


pred_int<-data.frame(predict(cumsum_model, mid_2015, interval = "prediction"))

mid_2015<-cbind(mid_2015, "Predict"=pred_int)


mid_2015%>%
  ggplot(aes(x=Predict.fit, y=PointSpread))+ geom_point(shape=18)+
  theme_light()+
  labs(title="2015 Point Spread vs. Predict Point Spread",
       x ="Predicted Point Spread", y = "2015 Point Spread")+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.title=element_text(face="bold"))+
  stat_smooth(method = lm)+
  geom_line(aes(y = Predict.lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = Predict.upr), color = "red", linetype = "dashed")


# Randomly draw 10 sample games from 2015
library(data.table)
set.seed(10)

mid_2015_sample <- data.table(mid_2015)%>%
  select(Game, Team, Opponent,PointSpread, Predict.fit, Predict.lwr, Predict.upr)

knitr::kable(mid_2015_sample[sample(.N, 10)], caption = "Prediction of Point Spread with 10 randomly sampled games")
