rm(list=ls())
NYC <- read.csv("C:/Users/kitan/Downloads/NYPD2017.csv")#,na.strings = c("","NA"))
NYC<-na.omit(NYC)
#str(NYC)
#summary(NYC)
#NYC1<-NYC[!is.na(NYC$BOROUGH),]
#NYC1<-NYC[!is.na(NYC$ZIP.CODE),]
#install.packages('randomForest')
library(randomForest)
NYC$TIME<-gsub(":.*","",NYC$TIME)
#View(NYC)

NYC$TIME<-as.numeric(NYC$TIME)
typeof(NYC$TIME)

str(NYC$TIME)

NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 0 & NYC$TIME <= 3, 1)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 4 & NYC$TIME <= 7, 2)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 12 & NYC$TIME <=15, 4)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 16 & NYC$TIME <= 19, 5)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 20 & NYC$TIME <=23, 6)

NYC$TIME<- replace(NYC$TIME, NYC$TIME == 8, 3)

NYC$TIME<- replace(NYC$TIME, NYC$TIME == 9, 3)
NYC$TIME<- replace(NYC$TIME, NYC$TIME == 10, 3)
NYC$TIME<- replace(NYC$TIME, NYC$TIME == 11, 3)
?mdy

NYC$DATE <- as.Date(NYC$DATE,"%m/%d/%Y")


NYC$MONTHS <- strftime(NYC$DATE,"%m")
#NYC$DATE<-as.Date(NYC$DATE,"%d-%m-%Y")

#NYC$MONTHS<-strftime(NYC$DATE,"%m")
str(NYC$MONTHS)
summary(NYC$KILLED)
NYC$MONTHS<-as.integer(NYC$MONTHS)
NYC$KILLED <- NYC$NUMBER.OF.PERSONS.KILLED + NYC$NUMBER.OF.PEDESTRIANS.KILLED + NYC$NUMBER.OF.CYCLIST.KILLED + NYC$NUMBER.OF.MOTORIST.KILLED
NYC$INJURED <- NYC$NUMBER.OF.PERSONS.INJURED + NYC$NUMBER.OF.PEDESTRIANS.INJURED + NYC$NUMBER.OF.CYCLIST.INJURED + NYC$NUMBER.OF.MOTORIST.INJURED
#NYC$KILLED<- replace(NYC$TIME, NYC$TIME >= 20 & NYC$TIME <=23, 6)
NYC$INJUREDORNOT<- replace(NYC$INJUREDORNOT, NYC$INJURED >0, 1)
NYC$INJUREDORNOT<- replace(NYC$INJUREDORNOT, NYC$INJURED ==0, 0)
#NYC$KILLEDORNOT[NYC$KILLED>0]=1
#NYC$KILLEDORNOT[NYC$INJURED==0]=0
#NYC$TIME<-as.integer(NYC$timezone)
NYC$CONTRIBUTING.FACTOR.VEHICLE.1<-as.integer(NYC$CONTRIBUTING.FACTOR.VEHICLE.1)
NYC$CONTRIBUTING.FACTOR.VEHICLE.2<-as.integer(NYC$CONTRIBUTING.FACTOR.VEHICLE.2)

NYC$CONTRIBUTING.FACTOR.VEHICLE.3<-as.integer(NYC$CONTRIBUTING.FACTOR.VEHICLE.3)

NYC$CONTRIBUTING.FACTOR.VEHICLE.4<-as.integer(NYC$CONTRIBUTING.FACTOR.VEHICLE.4)
NYC$CONTRIBUTING.FACTOR.VEHICLE.5<-as.integer(NYC$CONTRIBUTING.FACTOR.VEHICLE.5)
NYC$OFF.STREET.NAME<-as.integer(NYC$OFF.STREET.NAME)
NYC$ON.STREET.NAME<-as.integer(NYC$ON.STREET.NAME)

NYC$CROSS.STREET.NAME<-as.integer(NYC$CROSS.STREET.NAME)
NYC$LOCATION<-as.integer(NYC$LOCATION)
#NYC$DATE<-as.integer(NYC$DATE)
NYC$TIME<-as.integer(NYC$TIME)
NYC$BOROUGH<-as.integer(NYC$BOROUGH)
NYC$LONGITUDE<-as.integer(NYC$LONGITUDE)
NYC$LATITUDE<-as.integer(NYC$LATITUDE)
#NYC$months<-as.integer(NYC$months)
NYC$VEHICLE.TYPE.CODE.1<-as.integer(NYC$VEHICLE.TYPE.CODE.1)
NYC$VEHICLE.TYPE.CODE.2<-as.integer(NYC$VEHICLE.TYPE.CODE.2)
NYC$VEHICLE.TYPE.CODE.3<-as.integer(NYC$VEHICLE.TYPE.CODE.3)
NYC$VEHICLE.TYPE.CODE.4<-as.integer(NYC$VEHICLE.TYPE.CODE.4)
NYC$VEHICLE.TYPE.CODE.5<-as.integer(NYC$VEHICLE.TYPE.CODE.5)
NYC$INJUREDORNOT<-as.integer(NYC$INJUREDORNOT)
index <- seq (1,nrow(NYC),by=5)
test<-NYC[index,]
training<-NYC[-index,]
training<-training[,c(-1,-11,-12,-13,-14,-16,-17,-18,-15,-24,-32)]
test<-test[,c(-1,-11,-12,-13,-14,-16,-17,-18,-15,-24,-32)]
table(training$INJUREDORNOT)
?rpart()
#Grow the tree 
library(rpart)
mytree<-rpart( factor(INJUREDORNOT)~.,data=training)

mytree
?factor

# use the table to interpret the resutls.

table(training$INJUREDORNOT,training$CONTRIBUTING.FACTOR.VEHICLE.2)


# plot the tree
#?rpart()
## default plotting is not very good.
plot(mytree)
text(mytree)

## a better plot
library(rpart.plot)
prp(mytree)


# much fancier graph
fancyRpartPlot(mytree)


### Predict the score and error rate###
prediction<-predict(mytree,test,type="class")
prediction
table(actual=test[,22],prediction)
wrong<- (test[,22]!=prediction)
rate<-sum(wrong)/length(wrong)
rate

# which funtion gives index of the cool name

