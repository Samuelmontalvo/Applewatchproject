## Needed libraries
library(readxl)
library(tidyverse)
library(psych)
library(ggpubr)
library(irr)
library(tidyr)
library(blandr)
library(bmbstats)
library(BlandAltmanLeh)
library(kableExtra)



## Data subsets for recreational and football athletes
Recreational <- read_excel("Apple_Data_Condensed_Final_All.xlsx", 
                           sheet = "Recreational")

Football <- read_excel("Apple_Data_Condensed_Final_All.xlsx", 
                       sheet = "Football")

#descriptives for each subset
Descriptives <- subset(Recreational, select = c("Age","Height","Weight", "BMI"))
describe(Descriptives)

Descriptives <- subset(Football, select = c("Age","Height","Weight", "BMI"))
describe(Descriptives)

##Describe for HR long format
describe(Recreational) %>% kable()%>%
  kable_classic_2(full_width = F)

describe(Football) %>% kable()%>%
  kable_classic_2(full_width = F)

## Data subsets for recreational and football athletes hRmax
RecreationalHRmax <- read_excel("Apple_Data_Condensed_Final_All.xlsx", 
                                sheet = "RecreationalHRmax")

FootballHRmax <- read_excel("Apple_Data_Condensed_Final_All.xlsx", 
                            sheet = "FootballHRmax")

describe(RecreationalHRmax) %>% kable()%>%
  kable_classic_2(full_width = F)

describe(FootballHRmax) %>% kable()%>%
  kable_classic_2(full_width = F)


## PLOT HR BY STAGE 6

## REST FIRST
ECG_Rest_Recreational_all <- Recreational %>% select(ID,ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
                                                     ECG_Rest_5,ECG_Rest_6) %>% 
  gather(Stage, Hear_Rate, ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
         ECG_Rest_5,ECG_Rest_6) 

SPlot_ECG_Rest_Recreational_all <- ECG_Rest_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Rest_Recreational_all <- Recreational %>% select(ID,Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,Polar_Rest_4,
                                                       Polar_Rest_5,Polar_Rest_6) %>% 
  gather(Stage, Hear_Rate, Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,Polar_Rest_4,
         Polar_Rest_5,Polar_Rest_6) 

SPlot_Polar_Rest_Recreational_all <- Polar_Rest_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Rest_Recreational_all <- Recreational %>% select(ID,Apple6_Rest_1,Apple6_Rest_2,Apple6_Rest_3,Apple6_Rest_4,
                                                        Apple6_Rest_5,Apple6_Rest_6) %>% 
  gather(Stage, Hear_Rate, Apple6_Rest_1,Apple6_Rest_2,Apple6_Rest_3,Apple6_Rest_4,
         Apple6_Rest_5,Apple6_Rest_6) 

SPlot_Apple6_Rest_Recreational_all <- Apple6_Rest_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Rest_Recreational_all <- Recreational %>% select(ID,Apple7_Rest_1,Apple7_Rest_2,Apple7_Rest_3,Apple7_Rest_4,
                                                        Apple7_Rest_5,Apple7_Rest_6) %>% 
  gather(Stage, Hear_Rate, Apple7_Rest_1,Apple7_Rest_2,Apple7_Rest_3,Apple7_Rest_4,
         Apple7_Rest_5,Apple7_Rest_6) 

SPlot_Apple7_Rest_Recreational_all <- Apple7_Rest_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 rest stages
SPlot_Rest_Recreational_all <- ggarrange(SPlot_ECG_Rest_Recreational_all,SPlot_Polar_Rest_Recreational_all,
                                         SPlot_Apple6_Rest_Recreational_all,SPlot_Apple7_Rest_Recreational_all,
                                         ncol=1,nrow=4,
                                         labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                    "K","L","M","N","O"),
                                         label.y = 1.03)
ggsave("SPlot_Rest_Recreational_all.png")


## Low Stage
ECG_Low_Recreational_all <- Recreational %>% select(ID,ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
                                                    ECG_Low_5,ECG_Low_6) %>% 
  gather(Stage, Hear_Rate,ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
         ECG_Low_5,ECG_Low_6) 

SPlot_ECG_Low_Recreational_all <- ECG_Low_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Low_Recreational_all <- Recreational %>% select(ID,Polar_Low_1,Polar_Low_2,Polar_Low_3,Polar_Low_4,
                                                      Polar_Low_5,Polar_Low_6) %>% 
  gather(Stage, Hear_Rate,Polar_Low_1,Polar_Low_2,Polar_Low_3,Polar_Low_4,
         Polar_Low_5,Polar_Low_6) 

SPlot_Polar_Low_Recreational_all <- Polar_Low_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Low_Recreational_all <- Recreational %>% select(ID,Apple6_Low_1,Apple6_Low_2,Apple6_Low_3,Apple6_Low_4,
                                                       Apple6_Low_5,Apple6_Low_6) %>% 
  gather(Stage, Hear_Rate,Apple6_Low_1,Apple6_Low_2,Apple6_Low_3,Apple6_Low_4,
         Apple6_Low_5,Apple6_Low_6) 

SPlot_Apple6_Low_Recreational_all <- Apple6_Low_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Low_Recreational_all <- Recreational %>% select(ID,Apple7_Low_1,Apple7_Low_2,Apple7_Low_3,Apple7_Low_4,
                                                       Apple7_Low_5,Apple7_Low_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Low_1,Apple7_Low_2,Apple7_Low_3,Apple7_Low_4,
         Apple7_Low_5,Apple7_Low_6) 

SPlot_Apple7_Low_Recreational_all <- Apple7_Low_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 low stages recreational
SPlot_Low_Recreational_all <- ggarrange(SPlot_ECG_Low_Recreational_all,SPlot_Polar_Low_Recreational_all,
                                        SPlot_Apple6_Low_Recreational_all,SPlot_Apple7_Low_Recreational_all,
                                        ncol=1,nrow=4,
                                        labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                   "K","L","M","N","O"),
                                        label.y = 1.03)
ggsave("SPlot_Low_Recreational_all.png")


## Moderate Stage
ECG_Moderate_Recreational_all <- Recreational %>% select(ID,ECG_Moderate_1,ECG_Moderate_2,
                                                         ECG_Moderate_3,ECG_Moderate_4,
                                                         ECG_Moderate_5,ECG_Moderate_6) %>% 
  gather(Stage, Hear_Rate,ECG_Moderate_1,ECG_Moderate_2,
         ECG_Moderate_3,ECG_Moderate_4,
         ECG_Moderate_5,ECG_Moderate_6) 

SPlot_ECG_Moderate_Recreational_all <- ECG_Moderate_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Moderate_Recreational_all <- Recreational %>% select(ID,Polar_Moderate_1,Polar_Moderate_2,
                                                           Polar_Moderate_3,Polar_Moderate_4,
                                                           Polar_Moderate_5,Polar_Moderate_6) %>% 
  gather(Stage, Hear_Rate,Polar_Moderate_1,Polar_Moderate_2,
         Polar_Moderate_3,Polar_Moderate_4,
         Polar_Moderate_5,Polar_Moderate_6) 

SPlot_Polar_Moderate_Recreational_all <- Polar_Moderate_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Moderate_Recreational_all <- Recreational %>% select(ID,Apple6_Moderate_1,Apple6_Moderate_2,
                                                            Apple6_Moderate_3,Apple6_Moderate_4,
                                                            Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  gather(Stage, Hear_Rate, Apple6_Moderate_1,Apple6_Moderate_2,
         Apple6_Moderate_3,Apple6_Moderate_4,
         Apple6_Moderate_5,Apple6_Moderate_6) 

SPlot_Apple6_Moderate_Recreational_all <- Apple6_Moderate_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Moderate_Recreational_all <- Recreational %>% select(ID,Apple7_Moderate_1,Apple7_Moderate_2,
                                                            Apple7_Moderate_3,Apple7_Moderate_4,
                                                            Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Moderate_1,Apple7_Moderate_2,
         Apple7_Moderate_3,Apple7_Moderate_4,
         Apple7_Moderate_5,Apple7_Moderate_6) 

SPlot_Apple7_Moderate_Recreational_all <- Apple7_Moderate_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 Moderate stages recreational
SPlot_Moderate_Recreational_all <- ggarrange(SPlot_ECG_Moderate_Recreational_all,SPlot_Polar_Moderate_Recreational_all,
                                             SPlot_Apple6_Moderate_Recreational_all,SPlot_Apple7_Moderate_Recreational_all,
                                             ncol=1,nrow=4,
                                             labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                        "K","L","M","N","O"),
                                             label.y = 1.03)
ggsave("SPlot_Moderate_Recreational_all.png")

## High Stage
ECG_High_Recreational_all <- Recreational %>% select(ID,ECG_High_1,ECG_High_2,
                                                     ECG_High_3,ECG_High_4,
                                                     ECG_High_5,ECG_High_6) %>% 
  gather(Stage, Hear_Rate,ECG_High_1,ECG_High_2,
         ECG_High_3,ECG_High_4,
         ECG_High_5,ECG_High_6) 

SPlot_ECG_High_Recreational_all <- ECG_High_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_High_Recreational_all <- Recreational %>% select(ID,Polar_High_1,Polar_High_2,
                                                       Polar_High_3,Polar_High_4,
                                                       Polar_High_5,Polar_High_6) %>% 
  gather(Stage, Hear_Rate,Polar_High_1,Polar_High_2,
         Polar_High_3,Polar_High_4,
         Polar_High_5,Polar_High_6) 

SPlot_Polar_High_Recreational_all <- Polar_High_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_High_Recreational_all <- Recreational %>% select(ID,Apple6_High_1,Apple6_High_2,
                                                        Apple6_High_3,Apple6_High_4,
                                                        Apple6_High_5,Apple6_High_6) %>% 
  gather(Stage, Hear_Rate,Apple6_High_1,Apple6_High_2,
         Apple6_High_3,Apple6_High_4,
         Apple6_High_5,Apple6_High_6) 

SPlot_Apple6_High_Recreational_all <- Apple6_High_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_High_Recreational_all <- Recreational %>% select(ID,Apple7_High_1,Apple7_High_2,
                                                        Apple7_High_3,Apple7_High_4,
                                                        Apple7_High_5,Apple7_High_6) %>% 
  gather(Stage, Hear_Rate,Apple7_High_1,Apple7_High_2,
         Apple7_High_3,Apple7_High_4,
         Apple7_High_5,Apple7_High_6) 

SPlot_Apple7_High_Recreational_all <- Apple7_Moderate_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 High stages recreational
SPlot_High_Recreational_all <- ggarrange(SPlot_ECG_High_Recreational_all,SPlot_Polar_High_Recreational_all,
                                         SPlot_Apple6_High_Recreational_all,SPlot_Apple7_High_Recreational_all,
                                         ncol=1,nrow=4,
                                         labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                    "K","L","M","N","O"),
                                         label.y = 1.03)
ggsave("SPlot_High_Recreational_all.png")


## Post Stage
ECG_Post_Recreational_all <- Recreational %>% select(ID,ECG_Post_1,ECG_Post_2,
                                                     ECG_Post_3,ECG_Post_4,
                                                     ECG_Post_5,ECG_Post_6) %>% 
  gather(Stage, Hear_Rate,ECG_Post_1,ECG_Post_2,
         ECG_Post_3,ECG_Post_4,
         ECG_Post_5,ECG_Post_6) 

SPlot_ECG_Post_Recreational_all <- ECG_Post_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Post_Recreational_all <- Recreational %>% select(ID,Polar_Post_1,Polar_Post_2,
                                                       Polar_Post_3,Polar_Post_4,
                                                       Polar_Post_5,Polar_Post_6) %>% 
  gather(Stage, Hear_Rate,Polar_Post_1,Polar_Post_2,
         Polar_Post_3,Polar_Post_4,
         Polar_Post_5,Polar_Post_6) 

SPlot_Polar_Post_Recreational_all <- Polar_Post_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Post_Recreational_all <- Recreational %>% select(ID,Apple6_Post_1,Apple6_Post_2,
                                                        Apple6_Post_3,Apple6_Post_4,
                                                        Apple6_Post_5,Apple6_Post_6) %>% 
  gather(Stage, Hear_Rate,Apple6_Post_1,Apple6_Post_2,
         Apple6_Post_3,Apple6_Post_4,
         Apple6_Post_5,Apple6_Post_6) 

SPlot_Apple6_Post_Recreational_all <- Apple6_Post_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Post_Recreational_all <- Recreational %>% select(ID,Apple7_Post_1,Apple7_Post_2,
                                                        Apple7_Post_3,Apple7_Post_4,
                                                        Apple7_Post_5,Apple7_Post_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Post_1,Apple7_Post_2,
         Apple7_Post_3,Apple7_Post_4,
         Apple7_Post_5,Apple7_Post_6) 

SPlot_Apple7_Post_Recreational_all <- Apple7_Post_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 Post stages recreational
SPlot_Post_Recreational_all <- ggarrange(SPlot_ECG_Post_Recreational_all,SPlot_Polar_Post_Recreational_all,
                                         SPlot_Apple6_Post_Recreational_all,SPlot_Apple7_Post_Recreational_all,
                                         ncol=1,nrow=4,
                                         labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                    "K","L","M","N","O"),
                                         label.y = 1.03)
ggsave("SPlot_Post_Recreational_all.png")


## PLOT HR BY STAGE 6 FOOTBALL

## REST FIRST
ECG_Rest_Football_all <- Football %>% select(ID,ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
                                             ECG_Rest_5,ECG_Rest_6) %>% 
  gather(Stage, Hear_Rate, ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
         ECG_Rest_5,ECG_Rest_6) 

SPlot_ECG_Rest_Football_all <- ECG_Rest_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Rest_Football_all <- Football %>% select(ID,Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,Polar_Rest_4,
                                               Polar_Rest_5,Polar_Rest_6) %>% 
  gather(Stage, Hear_Rate, Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,Polar_Rest_4,
         Polar_Rest_5,Polar_Rest_6) 

SPlot_Polar_Rest_Football_all <- Polar_Rest_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Rest_Football_all <- Football %>% select(ID,Apple6_Rest_1,Apple6_Rest_2,Apple6_Rest_3,Apple6_Rest_4,
                                                Apple6_Rest_5,Apple6_Rest_6) %>% 
  gather(Stage, Hear_Rate, Apple6_Rest_1,Apple6_Rest_2,Apple6_Rest_3,Apple6_Rest_4,
         Apple6_Rest_5,Apple6_Rest_6) 

Apple6_Rest_Football_all$Hear_Rate <- as.numeric(Apple6_Rest_Football_all$Hear_Rate)

SPlot_Apple6_Rest_Football_all <- Apple6_Rest_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple7_Rest_Football_all <- Football %>% select(ID,Apple7_Rest_1,Apple7_Rest_2,Apple7_Rest_3,Apple7_Rest_4,
                                                Apple7_Rest_5,Apple7_Rest_6) %>% 
  gather(Stage, Hear_Rate, Apple7_Rest_1,Apple7_Rest_2,Apple7_Rest_3,Apple7_Rest_4,
         Apple7_Rest_5,Apple7_Rest_6) 

SPlot_Apple7_Rest_Football_all <- Apple7_Rest_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 rest stages
SPlot_Rest_Football_all <- ggarrange(SPlot_ECG_Rest_Football_all,SPlot_Polar_Rest_Football_all,
                                     SPlot_Apple6_Rest_Football_all,SPlot_Apple7_Rest_Football_all,
                                     ncol=1,nrow=4,
                                     labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                "K","L","M","N","O"),
                                     label.y = 1.03)
ggsave("SPlot_Rest_Football_all.png")


## Low Stage
ECG_Low_Football_all <- Football %>% select(ID,ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
                                            ECG_Low_5,ECG_Low_6) %>% 
  gather(Stage, Hear_Rate,ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
         ECG_Low_5,ECG_Low_6) 

SPlot_ECG_Low_Football_all <- ECG_Low_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Low_Football_all <- Football %>% select(ID,Polar_Low_1,Polar_Low_2,Polar_Low_3,Polar_Low_4,
                                              Polar_Low_5,Polar_Low_6) %>% 
  gather(Stage, Hear_Rate,Polar_Low_1,Polar_Low_2,Polar_Low_3,Polar_Low_4,
         Polar_Low_5,Polar_Low_6) 

SPlot_Polar_Low_Football_all <- Polar_Low_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Low_Football_all <- Football %>% select(ID,Apple6_Low_1,Apple6_Low_2,Apple6_Low_3,Apple6_Low_4,
                                               Apple6_Low_5,Apple6_Low_6) %>% 
  gather(Stage, Hear_Rate,Apple6_Low_1,Apple6_Low_2,Apple6_Low_3,Apple6_Low_4,
         Apple6_Low_5,Apple6_Low_6) 

SPlot_Apple6_Low_Football_all <- Apple6_Low_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Low_Football_all <- Football %>% select(ID,Apple7_Low_1,Apple7_Low_2,Apple7_Low_3,Apple7_Low_4,
                                               Apple7_Low_5,Apple7_Low_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Low_1,Apple7_Low_2,Apple7_Low_3,Apple7_Low_4,
         Apple7_Low_5,Apple7_Low_6) 

SPlot_Apple7_Low_Football_all <- Apple7_Low_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 low stages recreational
SPlot_Low_Football_all <- ggarrange(SPlot_ECG_Low_Football_all,SPlot_Polar_Low_Football_all,
                                    SPlot_Apple6_Low_Football_all,SPlot_Apple7_Low_Football_all,
                                    ncol=1,nrow=4,
                                    labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                               "K","L","M","N","O"),
                                    label.y = 1.03)
ggsave("SPlot_Low_Football_all.png")


## Moderate Stage
ECG_Moderate_Football_all <- Football %>% select(ID,ECG_Moderate_1,ECG_Moderate_2,
                                                 ECG_Moderate_3,ECG_Moderate_4,
                                                 ECG_Moderate_5,ECG_Moderate_6) %>% 
  gather(Stage, Hear_Rate,ECG_Moderate_1,ECG_Moderate_2,
         ECG_Moderate_3,ECG_Moderate_4,
         ECG_Moderate_5,ECG_Moderate_6) 

SPlot_ECG_Moderate_Football_all <- ECG_Moderate_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Moderate_Football_all <- Football %>% select(ID,Polar_Moderate_1,Polar_Moderate_2,
                                                   Polar_Moderate_3,Polar_Moderate_4,
                                                   Polar_Moderate_5,Polar_Moderate_6) %>% 
  gather(Stage, Hear_Rate,Polar_Moderate_1,Polar_Moderate_2,
         Polar_Moderate_3,Polar_Moderate_4,
         Polar_Moderate_5,Polar_Moderate_6) 

SPlot_Polar_Moderate_Football_all <- Polar_Moderate_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Moderate_Football_all <- Football %>% select(ID,Apple6_Moderate_1,Apple6_Moderate_2,
                                                    Apple6_Moderate_3,Apple6_Moderate_4,
                                                    Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  gather(Stage, Hear_Rate, Apple6_Moderate_1,Apple6_Moderate_2,
         Apple6_Moderate_3,Apple6_Moderate_4,
         Apple6_Moderate_5,Apple6_Moderate_6) 

SPlot_Apple6_Moderate_Football_all <- Apple6_Moderate_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_Moderate_Football_all <- Football %>% select(ID,Apple7_Moderate_1,Apple7_Moderate_2,
                                                    Apple7_Moderate_3,Apple7_Moderate_4,
                                                    Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Moderate_1,Apple7_Moderate_2,
         Apple7_Moderate_3,Apple7_Moderate_4,
         Apple7_Moderate_5,Apple7_Moderate_6) 

SPlot_Apple7_Moderate_Football_all <- Apple7_Moderate_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 Moderate stages recreational
SPlot_Moderate_Football_all <- ggarrange(SPlot_ECG_Moderate_Football_all,SPlot_Polar_Moderate_Football_all,
                                         SPlot_Apple6_Moderate_Football_all,SPlot_Apple7_Moderate_Football_all,
                                         ncol=1,nrow=4,
                                         labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                    "K","L","M","N","O"),
                                         label.y = 1.03)
ggsave("SPlot_Moderate_Football_all.png")

## High Stage
ECG_High_Football_all <- Football %>% select(ID,ECG_High_1,ECG_High_2,
                                             ECG_High_3,ECG_High_4,
                                             ECG_High_5,ECG_High_6) %>% 
  gather(Stage, Hear_Rate,ECG_High_1,ECG_High_2,
         ECG_High_3,ECG_High_4,
         ECG_High_5,ECG_High_6) 

SPlot_ECG_High_Football_all <- ECG_High_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_High_Football_all <- Football %>% select(ID,Polar_High_1,Polar_High_2,
                                               Polar_High_3,Polar_High_4,
                                               Polar_High_5,Polar_High_6) %>% 
  gather(Stage, Hear_Rate,Polar_High_1,Polar_High_2,
         Polar_High_3,Polar_High_4,
         Polar_High_5,Polar_High_6) 

SPlot_Polar_High_Football_all <- Polar_High_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_High_Football_all <- Football %>% select(ID,Apple6_High_1,Apple6_High_2,
                                                Apple6_High_3,Apple6_High_4,
                                                Apple6_High_5,Apple6_High_6) %>% 
  gather(Stage, Hear_Rate,Apple6_High_1,Apple6_High_2,
         Apple6_High_3,Apple6_High_4,
         Apple6_High_5,Apple6_High_6) 

SPlot_Apple6_High_Football_all <- Apple6_High_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()


Apple7_High_Football_all <- Football %>% select(ID,Apple7_High_1,Apple7_High_2,
                                                Apple7_High_3,Apple7_High_4,
                                                Apple7_High_5,Apple7_High_6) %>% 
  gather(Stage, Hear_Rate,Apple7_High_1,Apple7_High_2,
         Apple7_High_3,Apple7_High_4,
         Apple7_High_5,Apple7_High_6) 

SPlot_Apple7_High_Football_all <- Apple7_Moderate_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 High stages recreational
SPlot_High_Football_all <- ggarrange(SPlot_ECG_High_Football_all,SPlot_Polar_High_Football_all,
                                     SPlot_Apple6_High_Football_all,SPlot_Apple7_High_Football_all,
                                     ncol=1,nrow=4,
                                     labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                "K","L","M","N","O"),
                                     label.y = 1.03)
ggsave("SPlot_High_Football_all.png")


## Post Stage
ECG_Post_Football_all <- Football %>% select(ID,ECG_Post_1,ECG_Post_2,
                                             ECG_Post_3,ECG_Post_4,
                                             ECG_Post_5,ECG_Post_6) %>% 
  gather(Stage, Hear_Rate,ECG_Post_1,ECG_Post_2,
         ECG_Post_3,ECG_Post_4,
         ECG_Post_5,ECG_Post_6) 

SPlot_ECG_Post_Football_all <- ECG_Post_Recreational_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Polar_Post_Football_all <- Football %>% select(ID,Polar_Post_1,Polar_Post_2,
                                               Polar_Post_3,Polar_Post_4,
                                               Polar_Post_5,Polar_Post_6) %>% 
  gather(Stage, Hear_Rate,Polar_Post_1,Polar_Post_2,
         Polar_Post_3,Polar_Post_4,
         Polar_Post_5,Polar_Post_6) 

SPlot_Polar_Post_Football_all <- Polar_Post_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple6_Post_Football_all <- Football %>% select(ID,Apple6_Post_1,Apple6_Post_2,
                                                Apple6_Post_3,Apple6_Post_4,
                                                Apple6_Post_5,Apple6_Post_6) %>% 
  gather(Stage, Hear_Rate,Apple6_Post_1,Apple6_Post_2,
         Apple6_Post_3,Apple6_Post_4,
         Apple6_Post_5,Apple6_Post_6) 

SPlot_Apple6_Post_Football_all <- Apple6_Post_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() + 
  geom_point() + theme_bw()

Apple7_Post_Football_all <- Football %>% select(ID,Apple7_Post_1,Apple7_Post_2,
                                                Apple7_Post_3,Apple7_Post_4,
                                                Apple7_Post_5,Apple7_Post_6) %>% 
  gather(Stage, Hear_Rate,Apple7_Post_1,Apple7_Post_2,
         Apple7_Post_3,Apple7_Post_4,
         Apple7_Post_5,Apple7_Post_6) 

SPlot_Apple7_Post_Football_all <- Apple7_Post_Football_all %>% 
  ggplot(aes(x=Stage, y=Hear_Rate, group=ID, color=Stage)) + geom_line() +
  geom_point() + theme_bw()

## ALL 6 Post stages recreational
SPlot_Post_Football_all <- ggarrange(SPlot_ECG_Post_Football_all,SPlot_Polar_Post_Football_all,
                                     SPlot_Apple6_Post_Football_all,SPlot_Apple7_Post_Football_all,
                                     ncol=1,nrow=4,
                                     labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                                "K","L","M","N","O"),
                                     label.y = 1.03)
ggsave("SPlot_Post_Football_all.png")


########################### RELIABILITY ANALYSIS ###########################

######### ECG analysis Recreational first then football
#Recreational Rest
Rest_ECG_Recreational <- Recreational %>% select(ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
                                                 ECG_Rest_5,ECG_Rest_6)
icc(Rest_ECG_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Low
Low_ECG_Recreational <- Recreational %>% select(ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
                                                ECG_Low_5,ECG_Low_6)
icc(Low_ECG_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Moderate
Moderate_ECG_Recreational <- Recreational %>% select(ECG_Moderate_1,ECG_Moderate_2,
                                                     ECG_Moderate_3,ECG_Moderate_4,
                                                     ECG_Moderate_5,ECG_Moderate_6)
icc(Moderate_ECG_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational High
High_ECG_Recreational <- Recreational %>% select(ECG_High_1,ECG_High_2,ECG_High_3,
                                                 ECG_High_4,ECG_High_5,ECG_High_6)
icc(High_ECG_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Post
Post_ECG_Recreational <- Recreational %>% select(ECG_Post_1,ECG_Post_2,ECG_Post_3,
                                                 ECG_Post_4,ECG_Post_5,ECG_Post_6)
icc(Post_ECG_Recreational, model = "twoway", type = "consistency", unit = "average")

#Football Rest
Rest_ECG_Football <- Football %>% select(ECG_Rest_1,ECG_Rest_2,ECG_Rest_3,ECG_Rest_4,
                                         ECG_Rest_5,ECG_Rest_6)
icc(Rest_ECG_Football, model = "twoway", type = "consistency", unit = "average")
#Football Low
Low_ECG_Football <- Football %>% select(ECG_Low_1,ECG_Low_2,ECG_Low_3,ECG_Low_4,
                                        ECG_Low_5,ECG_Low_6)
icc(Low_ECG_Football, model = "twoway", type = "consistency", unit = "average")
#Football Moderate
Moderate_ECG_Football <- Football %>% select(ECG_Moderate_1,ECG_Moderate_2,
                                             ECG_Moderate_3,ECG_Moderate_4,
                                             ECG_Moderate_5,ECG_Moderate_6)
icc(Moderate_ECG_Football, model = "twoway", type = "consistency", unit = "average")
#Football High
High_ECG_Football <- Football %>% select(ECG_High_1,ECG_High_2,ECG_High_3,
                                         ECG_High_4,ECG_High_5,ECG_High_6)
icc(High_ECG_Football, model = "twoway", type = "consistency", unit = "average")
#Football Post
Post_ECG_Football <- Football %>% select(ECG_Post_1,ECG_Post_2,ECG_Post_3,
                                         ECG_Post_4,ECG_Post_5,ECG_Post_6)
icc(Post_ECG_Football, model = "twoway", type = "consistency", unit = "average")

###### Polar reliability
#Recreational Rest
Rest_Polar_Recreational <- Recreational %>% select(Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,
                                                   Polar_Rest_4,Polar_Rest_5,Polar_Rest_6)
icc(Rest_Polar_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Low
Low_Polar_Recreational <- Recreational %>% select(Polar_Low_1,Polar_Low_2,Polar_Low_3,
                                                  Polar_Low_4,Polar_Low_5,Polar_Low_6)
icc(Low_Polar_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Moderate
Moderate_Polar_Recreational <- Recreational %>% select(Polar_Moderate_1,Polar_Moderate_2,
                                                       Polar_Moderate_3,Polar_Moderate_4,
                                                       Polar_Moderate_5,Polar_Moderate_6)
icc(Moderate_Polar_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational High
High_Polar_Recreational <- Recreational %>% select(Polar_High_1,Polar_High_2,
                                                   Polar_High_3,Polar_High_4,
                                                   Polar_High_5,Polar_High_6)
icc(High_Polar_Recreational, model = "twoway", type = "consistency", unit = "average")
#Recreational Post
Post_Polar_Recreational <- Recreational %>% select(Polar_Post_1,Polar_Post_2,
                                                   Polar_Post_3,Polar_Post_4,
                                                   Polar_Post_5,Polar_Post_6)
icc(Post_Polar_Recreational, model = "twoway", type = "consistency", unit = "average")

#Football Rest
Rest_Polar_Football <- Football %>% select(Polar_Rest_1,Polar_Rest_2,Polar_Rest_3,
                                           Polar_Rest_4,Polar_Rest_5,Polar_Rest_6)
icc(Rest_Polar_Football, model = "twoway", type = "consistency", unit = "average")
#Football Low
Low_Polar_Football <- Football %>% select(Polar_Low_1,Polar_Low_2,Polar_Low_3,
                                          Polar_Low_4,Polar_Low_5,Polar_Low_6)
icc(Low_Polar_Football, model = "twoway", type = "consistency", unit = "average")
#Football Moderate
Moderate_Polar_Football <- Football %>% select(Polar_Moderate_1,Polar_Moderate_2,
                                               Polar_Moderate_3,Polar_Moderate_4,
                                               Polar_Moderate_5,Polar_Moderate_6)
icc(Moderate_Polar_Football, model = "twoway", type = "consistency", unit = "average")
#Football High
High_Polar_Football <- Football %>% select(Polar_High_1,Polar_High_2,
                                           Polar_High_3,Polar_High_4,
                                           Polar_High_5,Polar_High_6)
icc(High_Polar_Football, model = "twoway", type = "consistency", unit = "average")
#Post
Post_Polar_Football <- Football %>% select(Polar_Post_1,Polar_Post_2,
                                           Polar_Post_3,Polar_Post_4,
                                           Polar_Post_5,Polar_Post_6)
icc(Post_Polar_Football, model = "twoway", type = "consistency", unit = "average")

#####Apple Watch 6
#Recreational Rest
Rest_Apple6_Recreational <- Recreational %>% select(Apple6_Rest_1,Apple6_Rest_2, 
                                                    Apple6_Rest_3,Apple6_Rest_4,
                                                    Apple6_Rest_5,Apple6_Rest_6) 
icc(Rest_Apple6_Recreational, model = "twoway", type = "consistency", unit = "average")

#Recreational Low
Low_Apple6_Recreational <- Recreational %>% select(Apple6_Low_1,Apple6_Low_2,
                                                   Apple6_Low_3,Apple6_Low_4,
                                                   Apple6_Low_5,Apple6_Low_6)
icc(Low_Apple6_Recreational, model = "twoway", type = "consistency", unit = "average")

#Recreational Moderate
Moderate_Apple6_Recreational <- Recreational %>% select(Apple6_Moderate_1,Apple6_Moderate_2,
                                                        Apple6_Moderate_3,Apple6_Moderate_4,
                                                        Apple6_Moderate_5,Apple6_Moderate_6)
icc(Moderate_Apple6_Recreational, model = "twoway", type = "consistency", unit = "average")

#Recreational High
High_Apple6_Recreational <- Recreational %>% select(Apple6_High_3,Apple6_High_4,
                                                    Apple6_High_5,Apple6_High_6)
icc(High_Apple6_Recreational, model = "twoway", type = "consistency", unit = "average")

#Recreational Post
Post_Apple6_Recreational <- Recreational %>% select(Apple6_Post_3,Apple6_Post_4,
                                                    Apple6_Post_5,Apple6_Post_6)
icc(Post_Apple6_Recreational, model = "twoway", type = "consistency", unit = "average")

#Football Rest
Rest_Apple6_Football <- Football %>% select(Apple6_Rest_3,Apple6_Rest_4,
                                            Apple6_Rest_5,Apple6_Rest_6) 
icc(Rest_Apple6_Football, model = "twoway", type = "consistency", unit = "average")

#Football Low
Low_Apple6_Football <- Football %>% select(Apple6_Low_3,Apple6_Low_4,
                                           Apple6_Low_5,Apple6_Low_6)
icc(Low_Apple6_Football, model = "twoway", type = "consistency", unit = "average")

#Football Moderate
Moderate_Apple6_Football <- Football %>% select(Apple6_Moderate_3,Apple6_Moderate_4,
                                                Apple6_Moderate_5,Apple6_Moderate_6)
icc(Moderate_Apple6_Football, model = "twoway", type = "consistency", unit = "average")

#Football High
High_Apple6_Football <- Football %>% select(Apple6_High_3,Apple6_High_4,
                                            Apple6_High_5,Apple6_High_6)
icc(High_Apple6_Football, model = "twoway", type = "consistency", unit = "average")

#Football Post
Post_Apple6_Football <- Football %>% select(Apple6_Post_3,Apple6_Post_4,
                                            Apple6_Post_5,Apple6_Post_6)
icc(Post_Apple6_Football, model = "twoway", type = "consistency", unit = "average")

#####Apple Watch 7
#Recreational Rest
Rest_Apple7_Football <- Recreational %>% select(Apple7_Rest_1,Apple7_Rest_2,
                                                Apple7_Rest_3,Apple7_Rest_4,
                                                Apple7_Rest_5,Apple7_Rest_6)
icc(Rest_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Recreational Low
Low_Apple7_Football <- Recreational %>% select(Apple7_Low_3,
                                               Apple7_Low_4,Apple7_Low_5,Apple7_Low_6)
icc(Low_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Recreational Moderate
Moderate_Apple7_Football <- Recreational %>% select(Apple7_Moderate_3,Apple7_Moderate_4,
                                                    Apple7_Moderate_5,Apple7_Moderate_6)
icc(Moderate_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Recreational High
High_Apple7_Football <- Recreational %>% select(Apple7_High_3,Apple7_High_4,
                                                Apple7_High_5,Apple7_High_6)
icc(High_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Recreational Post
Post_Apple7_Football <- Recreational %>% select(Apple7_Post_3,Apple7_Post_4,
                                                Apple7_Post_5,Apple7_Post_6)
icc(Post_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Football Rest
Rest_Apple7_Football <- Football %>% select(Apple7_Rest_1,Apple7_Rest_2,
                                            Apple7_Rest_3,Apple7_Rest_4,
                                            Apple7_Rest_5,Apple7_Rest_6)
icc(Rest_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Football Low
Low_Apple7_Football <- Football %>% select(Apple7_Low_3,
                                           Apple7_Low_4,Apple7_Low_5,Apple7_Low_6)
icc(Low_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Football Moderate
Moderate_Apple7_Football <- Football %>% select(Apple7_Moderate_3,Apple7_Moderate_4,
                                                Apple7_Moderate_5,Apple7_Moderate_6)
icc(Moderate_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Football High
High_Apple7_Football <- Football %>% select(Apple7_High_3,Apple7_High_4,
                                            Apple7_High_5,Apple7_High_6)
icc(High_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

#Football Post
Post_Apple7_Football <- Football %>% select(Apple7_Post_3,Apple7_Post_4,
                                            Apple7_Post_5,Apple7_Post_6)
icc(Post_Apple7_Football, model = "twoway", type = "consistency", unit = "average")

############################# Validity ECG VS POLAR############################ 

####  ECG vs Polar Rest  Recreational First then Football
## Gather data then correlation, then ICC, then BA, then OLP

## REST ECG POLAR

##GATHER DATA
ECG_Rest_Recreational <- Recreational %>% select(ID,ECG_Rest_3,ECG_Rest_4,
                                                 ECG_Rest_5,ECG_Rest_6) %>% 
  gather(ID,ECG_Rest,ECG_Rest_3,ECG_Rest_4,
         ECG_Rest_5,ECG_Rest_6) %>% 
  select(ECG_Rest)

Polar_Rest_Recreational <- Recreational %>% select(ID,Polar_Rest_3,
                                                   Polar_Rest_4,Polar_Rest_5,Polar_Rest_6) %>% 
  gather(ID,Polar_Rest,Polar_Rest_3,
         Polar_Rest_4,Polar_Rest_5,Polar_Rest_6) %>% 
  select(Polar_Rest)

#New data frame
ECGvsPolar_Rest_Recreational <- cbind(Polar_Rest = Polar_Rest_Recreational, ECG_Rest = ECG_Rest_Recreational)

#Correlation
cor.test(ECGvsPolar_Rest_Recreational$ECG_Rest,ECGvsPolar_Rest_Recreational$Polar_Rest,
         method = "pearson")

Cor_ECGvsPolar_Rest_Recreational <- ECGvsPolar_Rest_Recreational %>% 
  ggplot(aes(x=Polar_Rest, y=ECG_Rest)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Rest_Recreational, model = "twoway", type = "consistency", unit = "average")


## BLAND ALTMAN basic
BA_ECGvsPolar_Rest_Recreational <- bland.altman.plot(ECGvsPolar_Rest_Recreational$ECG_Rest,
                                                     ECGvsPolar_Rest_Recreational$Polar_Rest,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Rest_Recreational


## FOOTBALL
##GATHER DATA
ECG_Rest_Football <- Football %>% select(ID,ECG_Rest_3,ECG_Rest_4,
                                         ECG_Rest_5,ECG_Rest_6) %>% 
  gather(ID,ECG_Rest,ECG_Rest_3,ECG_Rest_4,
         ECG_Rest_5,ECG_Rest_6) %>% 
  select(ECG_Rest)

Polar_Rest_Football <- Football %>% select(ID,Polar_Rest_3,
                                           Polar_Rest_4,Polar_Rest_5,Polar_Rest_6) %>% 
  gather(ID,Polar_Rest,Polar_Rest_3,
         Polar_Rest_4,Polar_Rest_5,Polar_Rest_6) %>% 
  select(Polar_Rest)

#New data frame
ECGvsPolar_Rest_Football <- cbind(Polar_Rest = Polar_Rest_Football, ECG_Rest = ECG_Rest_Football)

#Correlation
cor.test(ECGvsPolar_Rest_Football$ECG_Rest,ECGvsPolar_Rest_Football$Polar_Rest,
         method = "pearson")

Cor_ECGvsPolar_Rest_Football <- ECGvsPolar_Rest_Football %>% 
  ggplot(aes(x=Polar_Rest, y=ECG_Rest)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Rest_Football, model = "twoway", type = "consistency", unit = "average")


## BLAND ALTMAN basic
BA_ECGvsPolar_Rest_Football <- bland.altman.plot(ECGvsPolar_Rest_Football$ECG_Rest,
                                                 ECGvsPolar_Rest_Football$Polar_Rest,
                                                 graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Rest_Football

####  ECG vs Polar Low

##GATHER DATA
ECG_Low_Recreational <- Recreational %>% select(ECG_Low_3,ECG_Low_4,
                                                ECG_Low_5,ECG_Low_6) %>% 
  gather(ID,ECG_Low,ECG_Low_3,ECG_Low_4,
         ECG_Low_5,ECG_Low_6) %>% 
  select(ECG_Low)

Polar_Low_Recreational <- Recreational %>% select(Polar_Low_3,
                                                  Polar_Low_4,Polar_Low_5,Polar_Low_6) %>% 
  gather(ID,Polar_Low,Polar_Low_3,
         Polar_Low_4,Polar_Low_5,Polar_Low_6) %>% 
  select(Polar_Low)

#New data frame
ECGvsPolar_Low_Recreational <- cbind(Polar_Low = Polar_Low_Recreational, ECG_Low = ECG_Low_Recreational)

#Correlation
cor.test(ECGvsPolar_Low_Recreational$ECG_Low,ECGvsPolar_Low_Recreational$Polar_Low,
         method = "pearson")

Cor_ECGvsPolar_Low_Recreational <- ECGvsPolar_Low_Recreational %>% 
  ggplot(aes(x=Polar_Low, y=ECG_Low)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Low_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Low_Recreational <- bland.altman.plot(ECGvsPolar_Low_Recreational$ECG_Low,
                                                    ECGvsPolar_Low_Recreational$Polar_Low,
                                                    graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Low_Recreational


## Football
##GATHER DATA
ECG_Low_Football <- Football %>% select(ECG_Low_3,ECG_Low_4,
                                        ECG_Low_5,ECG_Low_6) %>% 
  gather(ID,ECG_Low,ECG_Low_3,ECG_Low_4,
         ECG_Low_5,ECG_Low_6) %>% 
  select(ECG_Low)

Polar_Low_Football <- Football %>% select(Polar_Low_3,
                                          Polar_Low_4,Polar_Low_5,Polar_Low_6) %>% 
  gather(ID,Polar_Low,Polar_Low_3,
         Polar_Low_4,Polar_Low_5,Polar_Low_6) %>% 
  select(Polar_Low)

#New data frame
ECGvsPolar_Low_Football <- cbind(Polar_Low = Polar_Low_Football, ECG_Low = ECG_Low_Football)

#Correlation
cor.test(ECGvsPolar_Low_Football$ECG_Low,ECGvsPolar_Low_Football$Polar_Low,
         method = "pearson")

Cor_ECGvsPolar_Low_Football <- ECGvsPolar_Low_Football %>% 
  ggplot(aes(x=Polar_Low, y=ECG_Low)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Low_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Low_Football <- bland.altman.plot(ECGvsPolar_Low_Recreational$ECG_Low,
                                                ECGvsPolar_Low_Recreational$Polar_Low,
                                                graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Low_Football


####  ECG vs Polar Moderate
##GATHER DATA
ECG_Moderate_Recreational <- Recreational %>% select(ECG_Moderate_3,ECG_Moderate_4,
                                                     ECG_Moderate_5,ECG_Moderate_6) %>% 
  gather(ID,ECG_Moderate,ECG_Moderate_3,ECG_Moderate_4,
         ECG_Moderate_5,ECG_Moderate_6) %>% 
  select(ECG_Moderate)

Polar_Moderate_Recreational <- Recreational %>% select(Polar_Moderate_3,Polar_Moderate_4,
                                                       Polar_Moderate_5,Polar_Moderate_6) %>% 
  gather(ID,Polar_Moderate,Polar_Moderate_3,Polar_Moderate_4,
         Polar_Moderate_5,Polar_Moderate_6) %>% 
  select(Polar_Moderate)

#New data frame
ECGvsPolar_Moderate_Recreational <- cbind(Polar_Moderate = Polar_Moderate_Recreational, 
                                          ECG_Moderate = ECG_Moderate_Recreational)

#Correlation
cor.test(ECGvsPolar_Moderate_Recreational$ECG_Moderate,ECGvsPolar_Moderate_Recreational$Polar_Moderate,
         method = "pearson")

Cor_ECGvsPolar_Moderate_Recreational <- ECGvsPolar_Moderate_Recreational %>% 
  ggplot(aes(x=Polar_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Moderate_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Moderate_Recreational <- bland.altman.plot(ECGvsPolar_Moderate_Recreational$ECG_Moderate,
                                                         ECGvsPolar_Moderate_Recreational$Polar_Moderate,
                                                         graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Moderate_Recreational

##GATHER DATA
ECG_Moderate_Football <- Football %>% select(ECG_Moderate_3,ECG_Moderate_4,
                                             ECG_Moderate_5,ECG_Moderate_6) %>% 
  gather(ID,ECG_Moderate,ECG_Moderate_3,ECG_Moderate_4,
         ECG_Moderate_5,ECG_Moderate_6) %>% 
  select(ECG_Moderate)

Polar_Moderate_Football <- Football %>% select(Polar_Moderate_3,Polar_Moderate_4,
                                               Polar_Moderate_5,Polar_Moderate_6) %>% 
  gather(ID,Polar_Moderate,Polar_Moderate_3,Polar_Moderate_4,
         Polar_Moderate_5,Polar_Moderate_6) %>% 
  select(Polar_Moderate)

#New data frame
ECGvsPolar_Moderate_Football <- cbind(Polar_Moderate = Polar_Moderate_Football, 
                                      ECG_Moderate = ECG_Moderate_Football)

#Correlation
cor.test(ECGvsPolar_Moderate_Football$ECG_Moderate,ECGvsPolar_Moderate_Football$Polar_Moderate,
         method = "pearson")

Cor_ECGvsPolar_ModerateFootball <- ECGvsPolar_Moderate_Football %>% 
  ggplot(aes(x=Polar_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_Moderate_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Moderate_Football <- bland.altman.plot(ECGvsPolar_Moderate_Recreational$ECG_Moderate,
                                                     ECGvsPolar_Moderate_Recreational$Polar_Moderate,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Moderate_Football

####  ECG vs Polar High
##GATHER DATA
ECG_High_Recreational <- Recreational %>% select(ECG_High_3,ECG_High_4,
                                                 ECG_High_5,ECG_High_6) %>% 
  gather(ID,ECG_High,ECG_High_3,ECG_High_4,
         ECG_High_5,ECG_High_6) %>% 
  select(ECG_High)

Polar_High_Recreational <- Recreational %>% select( Polar_High_3,Polar_High_4,
                                                    Polar_High_5,Polar_High_6) %>% 
  gather(ID,Polar_High, Polar_High_3,Polar_High_4,
         Polar_High_5,Polar_High_6) %>% 
  select(Polar_High)

#New data frame
ECGvsPolar_High_Recreational <- cbind(Polar_High = Polar_High_Recreational, 
                                      ECG_High = ECG_High_Recreational)

#Correlation
cor.test(ECGvsPolar_High_Recreational$ECG_High,ECGvsPolar_High_Recreational$Polar_High,
         method = "pearson")

Cor_ECGvsPolar_High_Recreational <- ECGvsPolar_High_Recreational %>% 
  ggplot(aes(x=Polar_High, y=ECG_High)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_High_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_High_Recreational <- bland.altman.plot(ECGvsPolar_High_Recreational$ECG_High,
                                                     ECGvsPolar_High_Recreational$Polar_High,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_High_Recreational

##GATHER DATA
ECG_High_Football <- Football %>% select(ECG_High_3,ECG_High_4,
                                         ECG_High_5,ECG_High_6) %>% 
  gather(ID,ECG_High,ECG_High_3,ECG_High_4,
         ECG_High_5,ECG_High_6) %>% 
  select(ECG_High)

Polar_High_Football <- Football %>% select( Polar_High_3,Polar_High_4,
                                            Polar_High_5,Polar_High_6) %>% 
  gather(ID,Polar_High, Polar_High_3,Polar_High_4,
         Polar_High_5,Polar_High_6) %>% 
  select(Polar_High)

#New data frame
ECGvsPolar_High_Football <- cbind(Polar_High = Polar_High_Football, 
                                  ECG_High = ECG_High_Football)

#Correlation
cor.test(ECGvsPolar_High_Football$ECG_High,ECGvsPolar_High_Football$Polar_High,
         method = "pearson")

Cor_ECGvsPolar_High_Football <- ECGvsPolar_High_Football %>% 
  ggplot(aes(x=Polar_High, y=ECG_High)) + geom_point() + theme_bw()

#ICC
icc(ECGvsPolar_High_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_High_Football <- bland.altman.plot(ECGvsPolar_High_Recreational$ECG_High,
                                                 ECGvsPolar_High_Recreational$Polar_High,
                                                 graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_High_Football

####  ECG vs Polar Post
##GATHER DATA
ECG_Post_Recreational <- Recreational %>% select(ECG_Post_3,ECG_Post_4,
                                                 ECG_Post_5,ECG_Post_6) %>% 
  gather(ID,ECG_Post,ECG_Post_3,ECG_Post_4,
         ECG_Post_5,ECG_Post_6) %>% 
  select(ECG_Post)

Polar_Post_Recreational <- Recreational %>% select(Polar_Post_3,Polar_Post_4,
                                                   Polar_Post_5,Polar_Post_6) %>% 
  gather(ID,Polar_Post,Polar_Post_3,Polar_Post_4,
         Polar_Post_5,Polar_Post_6) %>% 
  select(Polar_Post)

#New data frame
ECGvsPolar_Post_Recreational <- cbind(Polar_Post = Polar_Post_Recreational, 
                                      ECG_Post = ECG_Post_Recreational)

#Correlation
cor.test(ECGvsPolar_Post_Recreational$ECG_Post,ECGvsPolar_Post_Recreational$Polar_Post,
         method = "pearson")

Cor_ECGvsPolar_Post_Recreational <- ECGvsPolar_Post_Recreational %>% 
  ggplot(aes(x=Polar_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsPolar_Post_Recreational

#ICC
icc(ECGvsPolar_Post_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Post_Recreational <- bland.altman.plot(ECGvsPolar_Post$ECG_Post,
                                                     ECGvsPolar_Post$Polar_Post,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Post_Recreational

##GATHER DATA
ECG_Post_Football <- Football %>% select(ECG_Post_3,ECG_Post_4,
                                         ECG_Post_5,ECG_Post_6) %>% 
  gather(ID,ECG_Post,ECG_Post_3,ECG_Post_4,
         ECG_Post_5,ECG_Post_6) %>% 
  select(ECG_Post)

Polar_Post_Football <- Football %>% select(Polar_Post_3,Polar_Post_4,
                                           Polar_Post_5,Polar_Post_6) %>% 
  gather(ID,Polar_Post,Polar_Post_3,Polar_Post_4,
         Polar_Post_5,Polar_Post_6) %>% 
  select(Polar_Post)

#New data frame
ECGvsPolar_Post_Football <- cbind(Polar_Post = Polar_Post_Football, 
                                  ECG_Post = ECG_Post_Football)

#Correlation
cor.test(ECGvsPolar_Post_Football$ECG_Post,ECGvsPolar_Post_Football$Polar_Post,
         method = "pearson")

Cor_ECGvsPolar_Post_Football <- ECGvsPolar_Post_Football %>% 
  ggplot(aes(x=Polar_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsPolar_Post_Football

#ICC
icc(ECGvsPolar_Post_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsPolar_Post_Football <- bland.altman.plot(ECGvsPolar_Post$ECG_Post,
                                                 ECGvsPolar_Post$Polar_Post,
                                                 graph.sys="ggplot2") + theme_classic()
BA_ECGvsPolar_Post_Football


############################# Validity ECG VS APPLE 6 #############

####  ECG vs  APPLE 6
##GATHER DATA
APPLE6_Rest_Recreational <- Recreational %>% select(ID, Apple6_Rest_3,Apple6_Rest_4,
                                                    Apple6_Rest_5,Apple6_Rest_6) %>% 
  gather(ID,APPLE6_Rest,Apple6_Rest_3,Apple6_Rest_4,
         Apple6_Rest_5,Apple6_Rest_6) %>% 
  select(APPLE6_Rest)

#New data frame
ECGvsAPPLE6_Rest_Recreational <- cbind(APPLE6_Rest = APPLE6_Rest_Recreational, ECG_Rest = ECG_Rest_Recreational)

#Correlation
cor.test(ECGvsAPPLE6_Rest_Recreational$ECG_Rest,ECGvsAPPLE6_Rest_Recreational$APPLE6_Rest,
         method = "pearson")

Cor_ECGvsAPPLE6_Rest_Recreational <- ECGvsAPPLE6_Rest_Recreational %>% 
  ggplot(aes(x=APPLE6_Rest, y=ECG_Rest)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Rest_Recreational

#ICC
icc(ECGvsAPPLE6_Rest_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Rest_Recreational <- bland.altman.plot(ECGvsAPPLE6_Rest_Recreational$ECG_Rest,
                                                      ECGvsAPPLE6_Rest_Recreational$APPLE6_Rest,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Rest_Recreational
##GATHER DATA
APPLE6_Rest_Football <- Football %>% select(ID, Apple6_Rest_3,Apple6_Rest_4,
                                            Apple6_Rest_5,Apple6_Rest_6) %>% 
  gather(ID,Apple6_Rest,Apple6_Rest_3,Apple6_Rest_4,
         Apple6_Rest_5,Apple6_Rest_6) %>% 
  select(Apple6_Rest)

#New data frame
ECGvsAPPLE6_Rest_Football <- cbind(Apple6_Rest = APPLE6_Rest_Football, ECG_Rest = ECG_Rest_Football)

#Correlation
cor.test(ECGvsAPPLE6_Rest_Football $ECG_Rest,ECGvsAPPLE6_Rest_Football $Apple6_Rest,
         method = "pearson")

Cor_ECGvsAPPLE6_Rest_Football <- ECGvsAPPLE6_Rest_Football %>% 
  ggplot(aes(x=Apple6_Rest, y=ECG_Rest)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Rest_Football

#ICC
icc(ECGvsAPPLE6_Rest_Football , model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Rest_Football  <- bland.altman.plot(ECGvsAPPLE6_Rest_Football $ECG_Rest,
                                                   ECGvsAPPLE6_Rest_Football $Apple6_Rest,
                                                   graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Rest_Football 

####  ECG vs  APPLE 6 Low
##GATHER DATA
APPLE6_Low_Recreational <- Recreational %>% select(Apple6_Low_3,Apple6_Low_4,
                                                   Apple6_Low_5,Apple6_Low_6) %>% 
  gather(ID,APPLE6_Low,Apple6_Low_3,Apple6_Low_4,
         Apple6_Low_5,Apple6_Low_6) %>% 
  select(APPLE6_Low)

#New data frame
ECGvsAPPLE6_Low_Recreational <- cbind(APPLE6_Low = APPLE6_Low_Recreational, ECG_Low = ECG_Low_Recreational)

#Correlation
cor.test(ECGvsAPPLE6_Low_Recreational$ECG_Low,APPLE6_Low_Recreational$APPLE6_Low,
         method = "pearson")

Cor_ECGvsAPPLE6_Low_Recreational <- ECGvsAPPLE6_Low_Recreational %>% 
  ggplot(aes(x=APPLE6_Low, y=ECG_Low)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Low_Recreational

#ICC
icc(ECGvsAPPLE6_Low_Recreational , model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Low_Recreational <- bland.altman.plot(ECGvsAPPLE6_Low_Recreational$ECG_Low,
                                                     ECGvsAPPLE6_Low_Recreational$APPLE6_Low,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Low_Recreational

##GATHER DATA
APPLE6_Low_Football <- Football %>% select(Apple6_Low_3,Apple6_Low_4,
                                           Apple6_Low_5,Apple6_Low_6) %>% 
  gather(ID,APPLE6_Low,Apple6_Low_3,Apple6_Low_4,
         Apple6_Low_5,Apple6_Low_6) %>% 
  select(APPLE6_Low)

#New data frame
ECGvsAPPLE6_Low_Football <- cbind(APPLE6_Low = APPLE6_Low_Football, ECG_Low = ECG_Low_Football)

#Correlation
cor.test(ECGvsAPPLE6_Low_Football$ECG_Low,APPLE6_Low_Football$APPLE6_Low,
         method = "pearson")

Cor_ECGvsAPPLE6_Low_Football <- ECGvsAPPLE6_Low_Football %>% 
  ggplot(aes(x=APPLE6_Low, y=ECG_Low)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Low_Football

#ICC
icc(ECGvsAPPLE6_Low_Football , model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Low_Football <- bland.altman.plot(ECGvsAPPLE6_Low_Football$ECG_Low,
                                                 ECGvsAPPLE6_Low_Football$APPLE6_Low,
                                                 graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Low_Football

####  ECG vs  APPLE 6 Moderate
##GATHER DATA

APPLE6_Moderate_Recreational <- Recreational %>% select(Apple6_Moderate_3,Apple6_Moderate_4,
                                                        Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  gather(ID,APPLE6_Moderate,Apple6_Moderate_3,Apple6_Moderate_4,
         Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  select(APPLE6_Moderate)

#New data frame
ECGvsApple6_Moderate_Recreational <- cbind(APPLE6_Moderate = APPLE6_Moderate_Recreational, 
                                           ECG_Moderate = ECG_Moderate_Recreational)

#Correlation
cor.test(ECGvsApple6_Moderate_Recreational$ECG_Moderate,ECGvsApple6_Moderate_Recreational$APPLE6_Moderate,
         method = "pearson")

Cor_ECGvsAPPLE6_Moderate_Recreational <- ECGvsApple6_Moderate_Recreational %>% 
  ggplot(aes(x=APPLE6_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Moderate_Recreational

#ICC
icc(ECGvsApple6_Moderate_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Moderate_Recreational <- bland.altman.plot(ECGvsApple6_Moderate_Recreational$ECG_Moderate,
                                                          ECGvsApple6_Moderate_Recreational$APPLE6_Moderate,
                                                          graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Moderate_Recreational
##GATHER DATA

APPLE6_Moderate_Football <- Football %>% select(Apple6_Moderate_3,Apple6_Moderate_4,
                                                Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  gather(ID,APPLE6_Moderate,Apple6_Moderate_3,Apple6_Moderate_4,
         Apple6_Moderate_5,Apple6_Moderate_6) %>% 
  select(APPLE6_Moderate)

#New data frame
ECGvsAPPLE6_Moderate_Football <- cbind(APPLE6_Moderate = APPLE6_Moderate_Football, 
                                       ECG_Moderate = ECG_Moderate_Football)

#Correlation
cor.test(ECGvsAPPLE6_Moderate_Football$ECG_Moderate,ECGvsAPPLE6_Moderate_Football$APPLE6_Moderate,
         method = "pearson")

Cor_ECGvsAPPLE6_Moderate_Football <- ECGvsAPPLE6_Moderate_Football %>% 
  ggplot(aes(x=APPLE6_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Moderate_Football

#ICC
icc(ECGvsAPPLE6_Moderate_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Moderate_Football <- bland.altman.plot(ECGvsAPPLE6_Moderate_Football$ECG_Moderate,
                                                      ECGvsPolar6_Moderate_Football$APPLE6_Moderate,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Moderate_Football

####  ECG vs  APPLE 6 High
##GATHER DATA
Apple6_High_Recreational <- Recreational %>% select(Apple6_High_3,Apple6_High_4,
                                                    Apple6_High_5,Apple6_High_6) %>% 
  gather(ID,Apple6_High, Apple6_High_3,Apple6_High_4,
         Apple6_High_5,Apple6_High_6) %>% 
  select(Apple6_High)

#New data frame
ECGvsAPPLE6_High_Recreational <- cbind(Apple6_High = Apple6_High_Recreational, 
                                       ECG_High = ECG_High_Recreational)

#Correlation
cor.test(ECGvsAPPLE6_High_Recreational$ECG_High,ECGvsAPPLE6_High_Recreational$Apple6_High,
         method = "pearson")

Cor_ECGvsAPPLE6_High_Recreational <- ECGvsAPPLE_High_Recreational %>% 
  ggplot(aes(x=Apple6_High, y=ECG_High)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_High_Recreational

#ICC
icc(ECGvsAPPLE_High_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_High_Recreational <- bland.altman.plot(ECGvsAPPLE_High_Recreational$ECG_High,
                                                      ECGvsAPPLE_High_Recreational$Apple6_High,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_High_Recreational

##GATHER DATA
Apple6_High_Football <- Football %>% select(Apple6_High_3,Apple6_High_4,
                                            Apple6_High_5,Apple6_High_6) %>% 
  gather(ID,Apple6_High, Apple6_High_3,Apple6_High_4,
         Apple6_High_5,Apple6_High_6) %>% 
  select(Apple6_High)

#New data frame
ECGvsAPPLE6_High_Football <- cbind(Apple6_High = Apple6_High_Football, 
                                   ECG_High = ECG_High_Football)

#Correlation
cor.test(ECGvsAPPLE6_High_Football$ECG_High,ECGvsAPPLE6_High_Football$Apple6_High,
         method = "pearson")

Cor_ECGvsAPPLE6_High_Football <- ECGvsAPPLE6_High_Football %>% 
  ggplot(aes(x=Apple6_High, y=ECG_High)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_High_Football

#ICC
icc(ECGvsAPPLE6_High_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_High_Football <- bland.altman.plot(ECGvsAPPLE6_High_Football$ECG_High,
                                                  ECGvsAPPLE_High_Football$Apple6_High,
                                                  graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_High_Football

####  ECG vs  APPLE 6 Post
##GATHER DATA
APPLE6_Post_Recreational <- Recreational %>% select(Apple6_Post_3,Apple6_Post_4,
                                                    Apple6_Post_5,Apple6_Post_6) %>% 
  gather(ID,APPLE6_Post,Apple6_Post_3,Apple6_Post_4,
         Apple6_Post_5,Apple6_Post_6) %>% 
  select(APPLE6_Post)

#New data frame
ECGvsAPPLE6_Post_Recreational <- cbind(APPLE6_Post = APPLE6_Post_Recreational , 
                                       ECG_Post = ECG_Post_Recreational )

#Correlation
cor.test(ECGvsAPPLE6_Post_Recreational$ECG_Post,ECGvsAPPLE6_Post_Recreational$APPLE6_Post,
         method = "pearson")

Cor_ECGvsAPPLE6_Post_Recreational <- ECGvsAPPLE6_Post_Recreational %>% 
  ggplot(aes(x=APPLE6_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Post_Recreational

#ICC
icc(ECGvsAPPLE6_Post_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Post_Recreational <- bland.altman.plot(ECGvsAPPLE6_Post_Recreational$ECG_Post,
                                                      ECGvsAPPLE6_Post_Recreational$APPLE6_Post,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Post_Recreational

##GATHER DATA
APPLE6_Post_Football <- Football %>% select(Apple6_Post_3,Apple6_Post_4,
                                            Apple6_Post_5,Apple6_Post_6) %>% 
  gather(ID,APPLE6_Post,Apple6_Post_3,Apple6_Post_4,
         Apple6_Post_5,Apple6_Post_6) %>% 
  select(APPLE6_Post)

#New data frame
ECGvsAPPLE6_Post_Football <- cbind(APPLE6_Post = APPLE6_Post_Football, 
                                   ECG_Post = ECG_Post_Football)

#Correlation
cor.test(ECGvsAPPLE6_Post_Football$ECG_Post,ECGvsAPPLE6_Post_Football$APPLE6_Post,
         method = "pearson")

Cor_ECGvsAPPLE6_Post_Football <- ECGvsAPPLE6_Post_Football %>% 
  ggplot(aes(x=APPLE6_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE6_Post_Football

#ICC
icc(ECGvsAPPLE6_Post_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE6_Post_Football <- bland.altman.plot(ECGvsAPPLE6_Post_Football$ECG_Post,
                                                  ECGvsAPPLE6_Post_Football$APPLE6_Post,
                                                  graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE6_Post_Football

############################# Validity ECG VS APPLE 7 #############

####  ECG vs  APPLE 7
##GATHER DATA
APPLE7_Rest_Recreational <- Recreational %>% select(ID, Apple7_Rest_3,Apple7_Rest_4,
                                                    Apple7_Rest_5,Apple7_Rest_6) %>% 
  gather(ID,APPLE7_Rest,Apple7_Rest_3,Apple7_Rest_4,
         Apple7_Rest_5,Apple7_Rest_6) %>% 
  select(APPLE7_Rest)

#New data frame
ECGvsAPPLE7_Rest_Recreational <- cbind(APPLE7_Rest = APPLE7_Rest_Recreational, ECG_Rest = ECG_Rest_Recreational)

#Correlation
cor.test(ECGvsAPPLE7_Rest_Recreational$ECG_Rest,ECGvsAPPLE7_Rest_Recreational$APPLE7_Rest,
         method = "pearson")

Cor_ECGvsAPPLE7_Rest_Recreational <- ECGvsAPPLE7_Rest_Recreational %>% 
  ggplot(aes(x=APPLE7_Rest, y=ECG_Rest)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Rest_Recreational

#ICC
icc(ECGvsAPPLE7_Rest_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Rest_Recreational <- bland.altman.plot(ECGvsAPPLE7_Rest_Recreational$ECG_Rest,
                                                      ECGvsAPPLE7_Rest_Recreational$APPLE7_Rest,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Rest_Recreational

##GATHER DATA
APPLE7_Rest_Football <- Football %>% select(ID, Apple7_Rest_3,Apple7_Rest_4,
                                            Apple7_Rest_5,Apple7_Rest_6) %>% 
  gather(ID,APPLE7_Rest,Apple7_Rest_3,Apple7_Rest_4,
         Apple7_Rest_5,Apple7_Rest_6) %>% 
  select(APPLE7_Rest)

#New data frame
ECGvsAPPLE7_Rest_Football <- cbind(APPLE7_Rest = APPLE7_Rest_Football, ECG_Rest = ECG_Rest_Football)

#Correlation
cor.test(ECGvsAPPLE7_Rest_Football$ECG_Rest,ECGvsAPPLE7_Rest_Football$APPLE7_Rest,
         method = "pearson")

Cor_ECGvsAPPLE7_Rest_Football <- ECGvsAPPLE7_Rest_Football %>% 
  ggplot(aes(x=APPLE7_Rest, y=ECG_Rest)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Rest_Football

#ICC
icc(ECGvsAPPLE7_Rest_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Rest_Football <- bland.altman.plot(ECGvsAPPLE7_Rest_Football$ECG_Rest,
                                                  ECGvsAPPLE7_Rest_Football$APPLE7_Rest,
                                                  graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Rest_Football

####  ECG vs  APPLE 7 Low
##GATHER DATA
APPLE7_Low_Recreational <- Recreational %>% select(Apple7_Low_3,Apple7_Low_4,
                                                   Apple7_Low_5,Apple7_Low_6) %>% 
  gather(ID,APPLE7_Low,Apple7_Low_3,Apple7_Low_4,
         Apple7_Low_5,Apple7_Low_6) %>% 
  select(APPLE7_Low)

#New data frame
ECGvsAPPLE7_Low_Recreational <- cbind(APPLE7_Low = APPLE7_Low_Recreational, ECG_Low = ECG_Low_Recreational)

#Correlation
cor.test(ECGvsAPPLE7_Low_Recreational$ECG_Low,APPLE7_Low_Recreational$APPLE7_Low,
         method = "pearson")

Cor_ECGvsAPPLE7_Low_Recreational <- ECGvsAPPLE7_Low_Recreational %>% 
  ggplot(aes(x=APPLE7_Low, y=ECG_Low)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Low_Recreational

#ICC
icc(ECGvsAPPLE7_Low_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Low_Recreational <- bland.altman.plot(ECGvsAPPLE7_Low$ECG_Low,
                                                     ECGvsAPPLE7_Low$APPLE7_Low,
                                                     graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Low_Recreational

##GATHER DATA
APPLE7_Low_Football <- Football %>% select(Apple7_Low_3,Apple7_Low_4,
                                           Apple7_Low_5,Apple7_Low_6) %>% 
  gather(ID,APPLE7_Low,Apple7_Low_3,Apple7_Low_4,
         Apple7_Low_5,Apple7_Low_6) %>% 
  select(APPLE7_Low)

#New data frame
ECGvsAPPLE7_Low_Football <- cbind(APPLE7_Low = APPLE7_Low_Football, ECG_Low = ECG_Low_Football)

#Correlation
cor.test(ECGvsAPPLE7_Low_Football$ECG_Low,APPLE7_Low_Football$APPLE7_Low,
         method = "pearson")

Cor_ECGvsAPPLE7_Low_Football <- ECGvsAPPLE7_Low_Football %>% 
  ggplot(aes(x=APPLE7_Low, y=ECG_Low)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Low_Football

#ICC
icc(ECGvsAPPLE7_Low_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Low_Football <- bland.altman.plot(ECGvsAPPLE7_Low_Football$ECG_Low,
                                                 ECGvsAPPLE7_Low_Football$APPLE7_Low,
                                                 graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Low_Football

####  ECG vs  APPLE 6 Moderate
##GATHER DATA

APPLE7_Moderate_Recreational <- Recreational %>% select(Apple7_Moderate_3,Apple7_Moderate_4,
                                                        Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  gather(ID,APPLE7_Moderate,Apple7_Moderate_3,Apple7_Moderate_4,
         Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  select(APPLE7_Moderate)

#New data frame
ECGvsAPPLE7_Moderate_Recreational <- cbind(APPLE7_Moderate = APPLE7_Moderate_Recreational, 
                                           ECG_Moderate = ECG_Moderate_Recreational)

#Correlation
cor.test(ECGvsAPPLE7_Moderate_Recreational$ECG_Moderate,ECGvsAPPLE7_Moderate_Recreational$APPLE7_Moderate,
         method = "pearson")

Cor_ECGvsAPPLE7_Moderate_Recreational <- ECGvsAPPLE7_Moderate_Recreational %>% 
  ggplot(aes(x=APPLE7_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Moderate_Recreational

#ICC
icc(ECGvsAPPLE7_Moderate_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Moderate_Recreational <- bland.altman.plot(ECGvsAPPLE7_Moderate_Recreational$ECG_Moderate,
                                                          ECGvsAPPLE7_Moderate_Recreational$APPLE7_Moderate,
                                                          graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Moderate_Recreational

##GATHER DATA

APPLE7_Moderate_Football <- Football %>% select(Apple7_Moderate_3,Apple7_Moderate_4,
                                                Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  gather(ID,APPLE7_Moderate,Apple7_Moderate_3,Apple7_Moderate_4,
         Apple7_Moderate_5,Apple7_Moderate_6) %>% 
  select(APPLE7_Moderate)

#New data frame
ECGvsAPPLE7_Moderate_Football <- cbind(APPLE7_Moderate = APPLE7_Moderate_Football, 
                                       ECG_Moderate = ECG_Moderate_Football)

#Correlation
cor.test(ECGvsAPPLE7_Moderate_Football$ECG_Moderate,ECGvsAPPLE7_Moderate_Football$APPLE7_Moderate,
         method = "pearson")

Cor_ECGvsAPPLE7_Moderate_Football <- ECGvsAPPLE7_Moderate_Football %>% 
  ggplot(aes(x=APPLE7_Moderate, y=ECG_Moderate)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Moderate_Football

#ICC
icc(ECGvsAPPLE7_Moderate_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Moderate_Football <- bland.altman.plot(ECGvsAPPLE7_Moderate_Football$ECG_Moderate,
                                                      ECGvsAPPLE7_Moderate_Football$APPLE7_Moderate,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Moderate_Football

####  ECG vs  APPLE 7 High
##GATHER DATA
Apple7_High_Recreational <- Recreational %>% select(Apple7_High_3,Apple7_High_4,
                                                    Apple7_High_5,Apple7_High_6) %>% 
  gather(ID,Apple7_High,Apple7_High_3,Apple7_High_4,
         Apple7_High_5,Apple7_High_6) %>% 
  select(Apple7_High)

#New data frame
ECGvsAPPLE7_High_Recreational <- cbind(Apple7_High = Apple7_High_Recreational, 
                                       ECG_High = ECG_High_Recreational)

#Correlation
cor.test(ECGvsAPPLE7_High_Recreational$ECG_High,ECGvsAPPLE7_High_Recreational$Apple7_High,
         method = "pearson")

Cor_ECGvsAPPLE7_High_Recreational <- ECGvsAPPLE7_High_Recreational %>% 
  ggplot(aes(x=Apple7_High, y=ECG_High)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_High_Recreational

#ICC
icc(ECGvsAPPLE7_High_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_High_Recreational <- bland.altman.plot(ECGvsAPPLE7_High_Recreational$ECG_High,
                                                      ECGvsAPPLE7_High_Recreational$Apple7_High,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_High_Recreational

####  ECG vs  APPLE 7 High
##GATHER DAT

Apple7_High_Football <- Football %>% select(Apple7_High_3,Apple7_High_4,
                                            Apple7_High_5,Apple7_High_6) %>% 
  gather(ID,Apple7_High,Apple7_High_3,Apple7_High_4,
         Apple7_High_5,Apple7_High_6) %>% 
  select(Apple7_High)

#New data frame
ECGvsAPPLE7_High_Football <- cbind(Apple7_High = Apple7_High_Football, 
                                   ECG_High = ECG_High_Football)

#Correlation
cor.test(ECGvsAPPLE7_High_Football$ECG_High,ECGvsAPPLE7_High_Football$Apple7_High,
         method = "pearson")

Cor_ECGvsAPPLE7_High_Football <- ECGvsAPPLE7_High_Football %>% 
  ggplot(aes(x=Apple7_High, y=ECG_High)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_High_Football

#ICC
icc(ECGvsAPPLE7_High_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_High_Football <- bland.altman.plot(ECGvsAPPLE7_High_Football$ECG_High,
                                                  ECGvsAPPLE7_High_Football$Apple7_High,
                                                  graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_High_Football

####  ECG vs  APPLE 7 Post

##GATHER DATA
APPLE7_Post_Recreational <- Recreational %>% select(Apple7_Post_3,Apple7_Post_4,
                                                    Apple7_Post_5,Apple7_Post_6) %>% 
  gather(ID,APPLE7_Post,Apple7_Post_3,Apple7_Post_4,
         Apple7_Post_5,Apple7_Post_6) %>% 
  select(APPLE7_Post)

#New data frame
ECGvsAPPLE7_Post_Recreational <- cbind(APPLE7_Post = APPLE7_Post_Recreational, 
                                       ECG_Post = ECG_Post_Recreational)

#Correlation
cor.test(ECGvsAPPLE7_Post_Recreational$ECG_Post,ECGvsAPPLE7_Post_Recreational$APPLE7_Post,
         method = "pearson")

Cor_ECGvsAPPLE7_Post_Recreational <- ECGvsAPPLE7_Post_Recreational %>% 
  ggplot(aes(x=APPLE7_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Post_Recreational

#ICC
icc(ECGvsAPPLE7_Post_Recreational, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Post_Recreational <- bland.altman.plot(ECGvsAPPLE7_Post_Recreational$ECG_Post,
                                                      ECGvsAPPLE7_Post_Recreational$APPLE7_Post,
                                                      graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Post_Recreational

##GATHER DATA
APPLE7_Post_Football <- Football %>% select(Apple7_Post_3,Apple7_Post_4,
                                            Apple7_Post_5,Apple7_Post_6) %>% 
  gather(ID,APPLE7_Post,Apple7_Post_3,Apple7_Post_4,
         Apple7_Post_5,Apple7_Post_6) %>% 
  select(APPLE7_Post)

#New data frame
ECGvsAPPLE7_Post_Football <- cbind(APPLE7_Post = APPLE7_Post_Football, 
                                   ECG_Post = ECG_Post_Football)

#Correlation
cor.test(ECGvsAPPLE7_Post_Football$ECG_Post,ECGvsAPPLE7_Post_Football$APPLE7_Post,
         method = "pearson")

Cor_ECGvsAPPLE7_Post_Football <- ECGvsAPPLE7_Post_Football %>% 
  ggplot(aes(x=APPLE7_Post, y=ECG_Post)) + geom_point() + theme_bw()
Cor_ECGvsAPPLE7_Post_Football

#ICC
icc(ECGvsAPPLE7_Post_Football, model = "twoway", type = "consistency", unit = "average")

## BLAND ALTMAN basic
BA_ECGvsAPPLE7_Post_Football <- bland.altman.plot(ECGvsAPPLE7_Post_Recreational$ECG_Post,
                                                  ECGvsAPPLE7_Post_Recreational$APPLE7_Post,
                                                  graph.sys="ggplot2") + theme_classic()
BA_ECGvsAPPLE7_Post_Football

## PLOT OF BLAND ALTMAN
BA_recreational <- ggarrange(BA_ECGvsPolar_Rest_Recreational,BA_ECGvsPolar_Low_Recreational, 
                             BA_ECGvsPolar_Moderate_Recreational,
                             BA_ECGvsPolar_High_Recreational, BA_ECGvsPolar_Post_Recreational,
                             BA_ECGvsAPPLE6_Rest_Recreational,
                             BA_ECGvsAPPLE6_Low_Recreational,BA_ECGvsAPPLE6_Moderate_Recreational,
                             BA_ECGvsAPPLE6_High_Recreational,
                             BA_ECGvsAPPLE6_Post_Recreational,BA_ECGvsAPPLE7_Rest_Recreational,
                             BA_ECGvsAPPLE7_Low_Recreational,
                             BA_ECGvsAPPLE7_Moderate_Recreational,BA_ECGvsAPPLE7_High_Recreational,
                             BA_ECGvsAPPLE7_Post_Recreational,
                             ncol=5,nrow=3,
                             labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                        "K","L","M","N","O"),
                             label.y = 1.03)
ggsave("BA_recreational.png")


## PLOT OF BLAND ALTMAN
BA_Football <- ggarrange(BA_ECGvsPolar_Rest_Football,BA_ECGvsPolar_Low_Football, 
                         BA_ECGvsPolar_Moderate_Football,
                         BA_ECGvsPolar_High_Football, BA_ECGvsPolar_Post_Football, 
                         BA_ECGvsAPPLE6_Rest_Football,
                         BA_ECGvsAPPLE6_Low_Football,BA_ECGvsAPPLE6_Moderate_Football,
                         BA_ECGvsAPPLE6_High_Football,
                         BA_ECGvsAPPLE6_Post_Football,BA_ECGvsAPPLE7_Rest_Football,
                         BA_ECGvsAPPLE7_Low_Football,
                         BA_ECGvsAPPLE7_Moderate_Football,BA_ECGvsAPPLE7_High_Football,
                         BA_ECGvsAPPLE7_Post_Football,
                         ncol=5,nrow=3,
                         labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                    "K","L","M","N","O"),
                         label.y = 1.03)
ggsave("BA_Football.png")

## PLOT CORRELATIONS
Cor_recreational <- ggarrange(Cor_ECGvsPolar_Rest_Recreational,Cor_ECGvsPolar_Low_Recreational, 
                              Cor_ECGvsPolar_Moderate_Recreational,
                              Cor_ECGvsPolar_High_Recreational, Cor_ECGvsPolar_Post_Recreational,
                              Cor_ECGvsAPPLE6_Rest_Recreational,
                              Cor_ECGvsAPPLE6_Low_Recreational,Cor_ECGvsAPPLE6_Moderate_Recreational,
                              Cor_ECGvsAPPLE6_High_Recreational,
                              Cor_ECGvsAPPLE6_Post_Recreational,Cor_ECGvsAPPLE7_Rest_Recreational,
                              Cor_ECGvsAPPLE7_Low_Recreational,
                              Cor_ECGvsAPPLE7_Moderate_Recreational,Cor_ECGvsAPPLE7_High_Recreational,
                              Cor_ECGvsAPPLE7_Post_Recreational,
                              ncol=5,nrow=3,
                              labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                         "K","L","M","N","O"),
                              label.y = 1.03)
ggsave("Cor_recreational.png")


## PLOT OF BLAND ALTMAN
Cor_Football <- ggarrange(Cor_ECGvsPolar_Rest_Football,Cor_ECGvsPolar_Low_Football, 
                          Cor_ECGvsPolar_Moderate_Football,
                          Cor_ECGvsPolar_High_Football, Cor_ECGvsPolar_Post_Football, 
                          Cor_ECGvsAPPLE6_Rest_Football,
                          Cor_ECGvsAPPLE6_Low_Football,Cor_ECGvsAPPLE6_Moderate_Football,
                          Cor_ECGvsAPPLE6_High_Football,
                          Cor_ECGvsAPPLE6_Post_Football,Cor_ECGvsAPPLE7_Rest_Football,
                          Cor_ECGvsAPPLE7_Low_Football,
                          Cor_ECGvsAPPLE7_Moderate_Football,Cor_ECGvsAPPLE7_High_Football,
                          Cor_ECGvsAPPLE7_Post_Football,
                          ncol=5,nrow=3,
                          labels = c("A","B", "C", "D","E","F","G","H","I","J",
                                     "K","L","M","N","O"),
                          label.y = 1.03)
ggsave("Cor_Football.png")






