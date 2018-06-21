library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

MyData <- read.csv(file="Desktop/DfTRoadSafety_Accidents_2014.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

if (TRUE){
for (i in 1:nrow(MyData)){
  temp <- ""
  a <- MyData$Date[i]
  a<-paste0(a,substr(a,3,5))
  a<-paste0(a,"/")
  a<-paste0(a,substr(a,1,2))
  temp<-substr(a,7,16)
  MyData$Date[i] <- temp
}
}

if (TRUE){
for (i in 1:nrow(MyData))
  if(MyData$Accident_Severity[i]=="1"){
    MyData$Accident_Severity[i]="fatal"
  } else if(MyData$Accident_Severity[i]=="2") {
    MyData$Accident_Severity[i]="serious"
  } else {
    MyData$Accident_Severity[i]="slight"
  }
}

write.csv(MyData, file="Desktop/cleanData2014.csv")

  
