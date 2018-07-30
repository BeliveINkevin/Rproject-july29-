setwd("C:/Documents/My Excel") # MUST USE "/" for the wd to work, not "\"
install.packages("RSQLite")
library(RSQLite)
db<-dbConnect(SQLite(),dbname="Abalone.db")
df<-dbGetQuery(db,'SELECT * from Abalone LIMIT 1500 ')
colnames(df)=c("sex","length","diameter","height","total.weight",
                        "meat.weight","viscera.weight","shell.weight","age")
head(df)
abalone=df
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cowplot")
library(dplyr)
library(ggplot2) 
library(cowplot) #Use the cowplot packages to plot multiple graphs in 1 page
male=dplyr::filter(abalone,sex=='M')
female=dplyr::filter(abalone,sex=='F')
infant=dplyr::filter(abalone,sex=='I')

summary(male)
summary(female)
summary(infant)

#graphing count vs length
lm<-ggplot(male,aes(x=length))+geom_bar()
lf<-ggplot(female,aes(x=length))+geom_bar()
li<-ggplot(infant,aes(x=length))+geom_bar()
lt<-ggplot(abalone,aes(x=length))+geom_bar()
plot_grid(lm,lf,li,lt,labels="AUTO")

malemodel<-lm(age~length+diameter+height+total.weight+meat.weight+viscera.weight+shell.weight,data=male)
summary(malemodel)
femalemodel<-lm(age~length+diameter+height+total.weight+meat.weight+viscera.weight+shell.weight,data=female)
summary(femalemodel)
totalmodel<-lm(age~length+diameter+height+total.weight+meat.weight+viscera.weight+shell.weight,data=abalone)
summary(totalmodel)

#seeing if there is a relationship b/t length of shell and age x=M,y=F,z=I,a=entire dataset
x<-lm(age~length,data=male)
summary(x)
y<-lm(age~length,data=female)
summary(y)
z<-lm(age~length,data=infant)
summary(z)
a<-lm(age~length,data=abalone)
summary(a)


