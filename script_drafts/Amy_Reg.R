##############################
###Simple Linear Regression###
##############################


#Load Libraries 
library("tidyverse")
library("ggplot2")
library("readxl") 

#Import Data
GR <- read_excel("Green_Roof.xlsx")

#For this analysis we want to know how roof age (the explanatory variable)
#influences the response variable plant cover (Moss, Suc)
#lets start with Moss, we will need to filter out all other cover types

Cut<- GR %>% filter(Spp == "Moss")


#Check Normality of Moss using the function shapiro.test()
#get Moss as close to normality as possible (reciprocal, 
#logarithm, cube root, square root, and square)

shapiro.test(log10(Cut$Moss))
hist(log10(Cut$Moss))

###Run Regresstion###
#Here moss will be the responce variable and age will be the 
#explanitory variable. lm is the function for simple regresstion

lm(formula = log10(Moss) ~ Age, data = Cut)

#this code works but it only gives us the Coefficients. to get more details
#lets name the function and use the summary function

Reg<-lm(formula = log10(Moss) ~ Age, data = Cut)
summary(Reg)

#Now lets make a graph of our regresstion using GGplot 2
ggplot(Cut, aes(x=Age, y=Cover)) +
  geom_point(size=2)+
  geom_smooth(method=lm , color="red", se=FALSE)+
  theme_set(theme_bw())

###Challenge###
#create a graph with 2 regression lines, moss and suc
#write the r2 value on the graph
 
ggplot(GR, aes(x=Age, y=Cover, color=Spp)) +
  geom_point(size=2)+
  geom_smooth(method=lm , se=FALSE)+
  theme_set(theme_bw())



r2 <- summary(model)$r.squared

ggplot(GR, aes(x = Age, y = Cover, color = Spp)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = r2_labels, aes(x = Age, y = Cover, label = label, color = Spp), inherit.aes = FALSE, hjust = 1) +
  theme_bw()

