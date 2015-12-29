#setwd("~/project");
# setwd("G:/911calls")

install.package("rpart.plot")
library("rpart")
library("rpart.plot")
#install.packages('dplyr')
library(dplyr)

#install.packages('lubridate')
library('lubridate')

calls2011<-read.csv("911calls2011.csv",header=TRUE,sep=",", strip.white=TRUE,skipNul=TRUE);
calls2012<-read.csv("911calls2012.csv",header=TRUE,sep=",", strip.white=TRUE,skipNul=TRUE);
calls2013<-read.csv("911calls2013.csv",header=TRUE,sep=",", strip.white=TRUE,skipNul=TRUE);
calls2014<-read.csv("911calls2014.csv",header=TRUE,sep=",", strip.white=TRUE,skipNul=TRUE);

crimes<-read.csv("crimeTypes.csv",header=TRUE,sep=",", strip.white=TRUE,skipNul=TRUE);
colnames(crimes)=c('TypeText','crime');

actions<-read.csv("actions.csv",header=TRUE,sep=",", strip.white=TRUE);
colnames(actions)=c('DispositionText','jobDone');

data<-rbind(calls2011, calls2012, calls2013, calls2014);
originalData <- data;
crimesData<-merge(data, crimes, by='TypeText');
crimesData<-merge(crimesData, actions, by='DispositionText');
crimesData<-crimesData[crimesData$crime=='s',]
filteredData<-crimesData;
crimesData$hour<-as.POSIXlt(strptime(crimesData$TimeCreate, "%m/%d/%Y %H:%M"))$hour
crimesData$quarter<-quarters(as.Date(as.character(crimesData$TimeCreate), format("%m/%d/%Y")))
crimesData$TimeCreate <- as.Date(as.character(crimesData$TimeCreate), format("%m/%d/%Y"))

#get month of the year when it happened

crimesData$month <- as.POSIXlt(crimesData$TimeCreate)$mon;
crimesData$year <- as.POSIXlt(crimesData$TimeCreate)$year+1900;
totalData<-crimesData;
#count the number of crimes monthly
actionsCount <- crimesData %>% group_by(year, month) %>% tally()
#crimesData <- merge(crimesData, actionsCount, by='TimeCreate')

#create data for each year
#create data of the format (count, year, day)
crimesTSData <- unique(actionsCount[, which(names(actionsCount) %in% c("month", "n","year"))])
crimesTSData <- na.omit(crimesTSData)
data<-crimesTSData[with(crimesTSData, order(year,month)),]

#make n as zero for missing days


#//predict next year 

newData<-c();
for(j in 1:4){
  for(i in 0:11){
    crow<-c("month"=NA,"year"=NA,"n"=NA)
    if(j==1 & length(data$year[data$year==2011])!=0 & 
       length(data$month[data$month==i])==0){
      crow<-c("month"=i,"year"=2011,"n"=0)
    }else if(j==2 & length(data$year[data$year==2012])!=0 & 
             length(data$month[data$month==i])==0){
      crow<-c("month"=i,"year"=2012,"n"=0)
    }else if(j==3 & length(data$year[data$year==2013])!=0 & 
             length(data$month[data$month==i])==0){
      crow<-c("month"=i,"year"=2013,"n"=0)
    }else if(j==4 & length(data$year[data$year==2014])!=0 & 
             length(data$month[data$month==i])==0){
      crow<-c("month"=i,"year"=2014,"n"=0)
    }
    
    newData<-rbind(newData,crow);
  }
}

data<-rbind(data,newData)
crimesTSData <- unique(data[, which(names(data) %in% c("month", "n","year"))])
crimesTSData <- na.omit(crimesTSData)
data<-crimesTSData[with(crimesTSData, order(year,month)),]
par(mfrow=c(1,1))
#//create data for the form of timeseries
tsData<-c(rep(0,12*4));
#fill zero in place where day is not available
tsData<-data[,which(names(data) %in% c("n"))];

tsData<-ts(tsData, start=2011, frequency=12)
plot(tsData,type="l")
plot(decompose(tsData),type="l")
#quarter that has more Crime rate - Q1
actionsCountQuarter <- crimesData %>% group_by(year, quarter) %>% tally()
tsDataQurtr<-actionsCountQuarter[,which(names(actionsCountQuarter) %in% c("n"))];
tsDataQurtr<-ts(tsDataQurtr, start=2011, frequency=4)
plot(tsDataQurtr,type="h")

#hours when crime rate is more
actionsCountYear <- crimesData %>% group_by(hour) %>% tally()
actionsCountYear<-actionsCountYear[with(actionsCountYear,order(-n)),];
head(actionsCountYear)

#percent of crime rate near UNO compared to all
crimeRateZIP <- crimesData %>% group_by(Zip) %>% tally()
uno<-crimeRateZIP[crimeRateZIP$Zip==70122,];
uno<-na.omit(uno)
crimeRateNO<-data.frame("n"=c(sum(crimeRateZIP$n)))
crimeRateUNO<-uno$n/crimeRateNO*100
crimeRateUNO

#how many criminals were escaped in all the four years
leftCriminals<-totalData[totalData$jobDone=='n',]
perCriminalsEsc<-nrow(leftCriminals)/nrow(totalData)*100
perCriminalsEsc

#predict the crime rate for the year 2015
par(mfrow=c(2,1))
acf(tsData)
pacf(tsData)
#pacf tailing off quickly and ACF is not. Hence (p,d,q) = (0,unknown,1)
#applying differencing to check and make the series stationary to pass to arima model
tsData1<-diff(tsData)
m<-length(tsData1)
par(mfrow=c(1,1))
plot(1:m,tsData1,type="l")

#Differencing made the series stationary so d=1
par(mfrow=c(2,1))
acf(tsData1)
pacf(tsData1)

#applying arima model with (p,d,q)=(0,1,1)
tsData.fit <- arima (tsData,
                     order=c(1,1,1), 
                     seasonal = list(order=c(0,1,1),period=12),
                     include.mean=FALSE)
tsData.fit

#predicting crime rate for the year 2015
tsData.predict <- predict (tsData.fit, n.ahead=12)

par(mfrow=c(1,1))
plot (tsData,xlim=c(2013,2015.5))
lines(tsData.predict$pred,col="blue")
lines(tsData.predict$pred+tsData.predict$se,col="red")
lines(tsData.predict$pred-tsData.predict$se,col="red")


#year that has more crime rate
par(mfrow=c(1,1))
yearlyData<-c();
yearlyData <- data %>% group_by(year) %>% tally()
crimesCount<-yearlyData$n;
#plot(yearlyData$year, yearlyData$n, type="h",ylab="Number of Crimes")
barplot(yearlyData$n, beside=TRUE, main="Crime rate", ylab="Number of Crimes", xlab="Year",axes=TRUE)


#percentage change in the crime rate over the given years
percentageData<-NA;
yearlyData$perChange<-100*(crimesCount/lag(crimesCount, 1)-1)
percentageData<-yearlyData;
percentageData$year<-paste(percentageData$year,"-",lag(percentageData$year,1))
percentageData<-na.omit(percentageData)
percentageData$n<-NULL
percentageData

#crime types and their rate altogeather in one year
originalData$TypeText[originalData$TypeText=='FUGITIVE ATTTACHMENT']<-'FUGITIVE ATTACHMENT'
categoryData<- originalData %>% group_by(TypeText) %>% tally()
mostCommitedCrimes<-categoryData[with(categoryData,order(-n)),];
#top 10 crimes and group rest of them togeather
top10Crimes<-mostCommitedCrimes[1:10,]
remCrimes<-mostCommitedCrimes[11:238,]
remCrimesTotal<-data.frame("TypeText"=c('remainingCrimes'),"n"=c(sum(remCrimes$n)))
colnames(remCrimesTotal)<-c("TypeText","n")
pieChartData<-rbind(top10Crimes,remCrimesTotal);
pie(pieChartData$n, labels=pieChartData$TypeText, main="crime rate of all the four years")

#top 10 areas with crime rate
areasData<- originalData %>% group_by(Zip) %>% tally()
areasData<-na.omit(areasData)
areasData<-areasData[with(areasData,order(-n)),]
top10Areas<-areasData[1:10,]
top10Areas

#which area needs more cop support
incCrimeAreas<- totalData %>% group_by(Zip, year) %>% tally()
incCrimeAreas<-na.omit(incCrimeAreas)
incCrimeAreas <- unique(incCrimeAreas[, which(names(incCrimeAreas) %in% c("Zip", "n","year"))])
incCrimeAreas<-incCrimeAreas[2:nrow(incCrimeAreas),]
allAreas<-unique(incCrimeAreas$Zip)

#which area had increasing crime rate over the years
test<-c();
for(i in 1:length(allAreas)){
  currentArea<-incCrimeAreas[incCrimeAreas$Zip==allAreas[i],]
  currentArea$n1<-100*(currentArea$n/lag(currentArea$n, 1)-1)
  print(currentArea)
  currentArea$n2<-100*(currentArea$n/lag(currentArea$n, 2)-1)
  currentArea$n3<-100*(currentArea$n/lag(currentArea$n, 3)-1)
  currentArea<-currentArea[currentArea$n3>=0,]
  currentArea<-na.omit(currentArea)
  test[length(test)+1]<-currentArea$Zip;
}
