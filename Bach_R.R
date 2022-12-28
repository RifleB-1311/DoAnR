library(ggplot2)
library("readxl")
library(plotrix)

setwd("D:/01-HUET/02-Download")
Bachdata= read_excel("Hotel.xlsx")
dfbach= na.omit(Bachdata)
df= data.frame(dfbach)

Bach<-df$Poor


#Trung binh  
mean<-mean(Bach)
mean

#Yeu vi
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
yeuvi<-getmode(Bach)
yeuvi

#Trung vi
median<- median(Bach)
median
#Phuong sai
varence<-var(Bach)
varence

#Do lech chuan
std<-sd(Bach)
std

#Sai so chuan
sai_so = sd(fert)/sqrt(length(fert))
sai_so

#gans gia tri
Bach1<-df$Poor
Bach2=df$Terrible
Bach3=df$VeryGood
Bach4=df$Average
Bach5=c(sum(Bach1),sum(Bach2),sum(Bach3),sum(Bach4))
Bach6=c(Bach2,Bach3)
color=c("lightblue","red","yellow","purple")
#ve Bieu do 
pie(Bach5,labels = c("poor","Terrible","VeryGood","Average"),main = "Bi???u d??? tròn d??? li???u khách s???n",col = color)
boxplot(Bach4,Bach3,labels = c("Average","VeryGood"),main="Bi???u d??? d??? li???u khách s???n t???i Avarage và VeryGood",col = "lightgreen",horizontal = TRUE,xlab="count",ylab="index")
barplot(Bach5,names.arg = c("poor","Terrible","VeryGood","Average"),main="Bi???u d??? bar c???a d??? li???u khách s???n",col="lightgreen")

