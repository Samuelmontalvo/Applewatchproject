## Needed libraries
library(dplyr)
library(readxl)
Appleproject <- read_excel("Appleproject.xlsx", 
                           sheet = "Master_Data")
View(Appleproject)

attach(Appleproject)

Appleproject <- Appleproject %>% mutate(BMI = Weight/(Height^2))

## Convert Stage from numeric to factor variable
Stage <- as.factor(Stage)

## Subset for the Stage perioids on Running test after VO2

All <- subset(Appleproject, select = c("Apple","Polar"))

Stage_2 <- subset(Appleproject, Stage == "2", 
               select = c("Apple","Polar"))

Stage_3 <- subset(Appleproject, Stage == "3", 
                  select = c("Apple","Polar"))

library(psych)

## ICC psych package
ICC(All)
ICC(Stage_2)
ICC(Stage_3)


## ICC irr package
library(irr)
icc(Stage_2, model = "twoway", 
  type = "agreement", unit = "average")

## Descriptives
Sex <- as.factor(Sex)

Descriptives <- subset(Appleproject, Stage == "2", 
                  select = c("Age","Sex","Height","Weight", "BMI","VO2max"))

describe(Descriptives)
describeBy(Descriptives,Descriptives$Sex)


library(blandr)
library(ggplot2)
#standard BA plot
blandr.draw(Stage_3$Apple,Stage_3$Polar, 
            plotTitle = "Bland-Altman plot of Apple Watch 6 and Polar H-10", 
            ciDisplay = FALSE,
            ciShading = FALSE) +
  theme_classic()
ggsave("BA_S3.png")

## plot with proportional bias
blandr.draw(Stage_3$Apple,Stage_3$Polar, 
            plotTitle = "Bland-Altman plot of Apple Watch 6 and Polar H-10", 
            ciDisplay = FALSE,
            ciShading = FALSE, plotProportionalBias = TRUE) +
  theme_classic()
ggsave("BA_proportionalbias_S3.png")