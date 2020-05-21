getwd()
setwd("C:\\Users\\Home\\Desktop\\Dataset")

data<-read.csv("DirectMarketing.csv")

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("car")

library(dplyr)     #For DataManipulation
library(ggplot2)   #For DataVisulaisation
library(car)       #For Linear Regression

head(data)

### Data Exploration ###

#Age:Catgorical Variable
plot(data$Age,data$AmountSpent,clr="red")
data$Age1<-ifelse(data$Age!="Young","middle-old",as.character(data$Age))
data$Age1<-as.factor(data$Age1)
summary(data$Age1)
plot(data$Age1,data$AmountSpent,clr="red")


#Gender : Catgorical Variable
plot(data$Gender,data$AmountSpent,clr="red")

#OwnHome :Catgorical Variable
plot(data$OwnHome,data$AmountSpent,clr="red")


#Married :Catgorical Variable
plot(data$Married,data$AmountSpent,clr="red")

#Location :Catgorical Variable
plot(data$Location,data$AmountSpent,clr="red")

#Salary :continoues Variable
plot(data$Salary,data$AmountSpent)


#childern :continoues convert into Catgorical Variable
summary(data$Children)
data$Children<-as.factor(data$Children)   #to convert into catgorical
plot(data$Children,data$AmountSpent)
data$Children1<-ifelse(data$Children==3| data$Children==2,"3-2",as.character(data$Children)) ##combining childern 2 nd 3
data$Children1<-as.factor(data$Children1)  #to convert into Catgorical
summary(data$Children1)
plot(data$Children1,data$AmountSpent,clr="red")


#History :Catgorical Variable
summary(data$History)
#imputing Missing value 
tapply(data$AmountSpent,data$History,mean)
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"])
#creating NAs as Missing (so tat we can build model)
data$History1<-ifelse(is.na(data$History),"Missing",as.factor(data$History))
data$History1<-as.factor(data$History1)
summary(data$History1)
data$History1<-factor(data$History1,labels =c("High","Low","Medium","Missing"))
summary(data$History1)

                       
                       
#Catalogs :contnious convert into Catgorical
summary(data$Catalogs)
data$Catalogs<-as.factor(data$Catalogs)
summary(data$Catalogs)
plot(data$Catalogs,data$AmountSpent,clr="red")


data1<-data[,-c(1,7,8)]


#########################Linear Model Building ################################
mod1<-lm(AmountSpent~.,data=data1)
summary(mod1)

step(mod,direction="both")  ##SWR (Step wise Regression)


mod2<-lm(AmountSpent~Gender+Location +Salary+Catalogs+Children1+History1,data=data1)
summary(mod2)

#Removing insignificant Variables
#Gendermale nd History1Missing

mod3<-lm(AmountSpent~Location +Salary+Catalogs+Children1,data=data1)
summary(mod3)



##### Creating Dummy Variable for Age1 nd History1

data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)


data1$Missing_d<-ifelse(data1$History1=="Missing",1,0)
data1$Low_d<-ifelse(data1$History1=="Low",1,0)
data1$Med_d<-ifelse(data1$History1=="Medium",1,0)
data1$High_d<-ifelse(data1$History1=="High",1,0)


mod3<-lm(AmountSpent~Male_d+Location+Salary+Catalogs+Children1+Med_d+Low_d,data=data1)
summary(mod3)

##Male_d is still insignificant :drop Male_d


mod4<-lm(AmountSpent~Location+Salary+Catalogs+Children1+Med_d+Low_d,data=data1)
summary(mod4)



########################## LINEAR REGRESSION Assumptions ###########################

   ####Normality :Residuals op Symmetry###
hist(mod4$residuals)
qqPlot(mod4$residuals)

#Residuals op must be symmetry(mirror image) called Normalitey
#in above plot residuals at end Becoming Symmetry


  ### Mulitcoolinearity ###########
vif(mod4)

#Values are less then 10
#So no Multicollinearity problem but i have Normality issues

 ###Constant Variance checking #####
plot(mod4$fitted.values,mod4$residuals)
 #Observed tat plot is in funnel shape ,soit does not satisfies the Asuumptions of LinearRegression
 #so Log transformation is Done

  ### Apply log transformation ###
mod5<-lm(log(AmountSpent)~Location+Salary+Catalogs+Children1+Med_d+Low_d,data=data1)

summary(mod5)          #All are significant
hist(mod5$residuals)   #Symmetry :Mirror image :Normalaity
qqPlot(mod5$residuals) #symmetry :two data points on same line :Normality
vif(mod5)              #No multicollinearity problem
plot(mod5$fitted.values,mod5$residuals)  #plot is Homoscedasity vit little funnel shape


### Apply sqrt transformation ###

mod6<-lm(sqrt(AmountSpent)~Location+Salary+Catalogs+Children1+Med_d+Low_d,data=data1)

summary(mod6)          #All are significant (signs are chnanges to positive)
hist(mod6$residuals)   #Symmetry :Mirror image :Normalaity
qqPlot(mod6$residuals) #symmetry :two data points on same line :Normality
vif(mod6)              #No multicollinearity problem
plot(mod6$fitted.values,mod5$residuals)  #plot is perfectley Homoscedasity 


###### FIT CHART :Actual value vs Predicted value for mod6 #####

Actual<-sqrt(data1$AmountSpent)
Predicted<-mod6$fitted.values

dat<-data.frame(Predicted,Actual)
head(dat,3)

Residuals =Predicted-Actual
head(Residuals,3)

p<-ggplot(dat,aes(x=row(dat)[,2],y=Predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=Actual),colour="black")


#write.csv(dat,"C:\\Users\\Home\\Desktop\\Dataset\\outputDirectmarketing.csv",row.names=FALSE)
