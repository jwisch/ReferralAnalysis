library(lubridate)
library(plyr)
library(ggplot2)
library(Hmisc)

#load data
referrals1617 <- read.csv(file="C:/Users/julie.wisch/Documents/ReferralPlot/Referral1617.csv", header=TRUE, sep=",")
referrals1718 <- read.csv(file="C:/Users/julie.wisch/Documents/ReferralPlot/Referral1718.csv", header=TRUE, sep=",")
moon <- read.csv(file="C:/Users/julie.wisch/Documents/ReferralPlot/moon.csv", header=TRUE, sep=",")
temperature <- read.csv(file="C:/Users/julie.wisch/Documents/ReferralPlot/TempData.csv", header=TRUE, sep=",")
weather <- read.csv(file="C:/Users/julie.wisch/Documents/ReferralPlot/OtherWeather.csv", header=TRUE, sep=",")

#rename column names to make dataframes match
colnames(referrals1718)[colnames(referrals1718)=="Select.reason."] <- "Reason"


#counts the number of referrals (sorted by reason, on a given date)
referralfreq1617<-count(referrals1617, c("Reason", "Date"))
referralfreq1718<-count(referrals1718, c("Reason", "Date"))

#sorting stuff into smaller dataframes that just have what I want
BehavioralViolations1617 <- subset(referralfreq1617, Reason == "Behavioral violation")                  
BehavioralViolations1718 <- subset(referralfreq1718, Reason == "Behavioral violation")                  
CuL1617 <- subset(referralfreq1617, Reason == "Truancy- classroom")                  
CuL1718 <- subset(referralfreq1718, Reason == "Truancy- classroom") 
moonsub <- moon[, c("date", "phaseid")]
tempsub <- temperature[, c("DATE", "TOBS")]
weathersub <- weather[, c("DATE", "AvgWind", "Precip", "Thunder")]

colnames(moonsub)[colnames(moonsub)=="date"] <- "Date"
colnames(tempsub)[colnames(tempsub)=="DATE"] <- "Date"
colnames(weathersub)[colnames(weathersub)=="DATE"] <- "Date"


Behavior <- rbind(BehavioralViolations1617, BehavioralViolations1718)
CuL <- rbind(CuL1617, CuL1718)

###############Data Histograms



par(mfrow=c(2,2),new = TRUE)
hist(Behavior$freq, xlab = "Frequency", ylab = "Behavioral Violations", main = "")
hist(CuL$freq, xlab = "Frequency", ylab = "Classroom Truancies", main = "")
boxplot(Behavior$freq, xlab = "Frequency", ylab = "Behavioral Violations")
boxplot(CuL$freq, xlab = "Frequency", ylab = "Classroom Truancies")

############################
###Trying to get a nice data frame


df<-merge(x = CuL, y = Behavior, by = "Date", all = TRUE)
df<-merge(x = df, y = moonsub, by = "Date", all =TRUE)
df<-merge(x = df, y = tempsub, by = "Date", all = TRUE)
df<-merge(x = df, y = weathersub, by = "Date", all = TRUE)

#Getting the day of the week
df$Date<- as.Date(df$Date, format = "%m/%d/%Y")
day <- weekdays(df$Date)
df<-cbind(df, day)

df_dropbaddates <- subset(df, Date > as.Date("2016-08-21") )
df_dropbaddates <- subset(df_dropbaddates, Date < as.Date("2018-04-09") )

library(data.table)
df_ordered<-setorder(df_dropbaddates, Date)
#filling in moon phase
for(i in 1:2){
  df_ordered$phaseid[i]<-3 }

for(i in 3: nrow(df_ordered)){ 
  if (is.na(df_ordered$phaseid[i]) == TRUE){
    df_ordered$phaseid[i]<-df_ordered$phaseid[i-1]
  }
}

#removes the duplicate rows...not sure why that was happening with merge but this seems to fix it
df <- df_ordered[!duplicated(df_ordered$Date),]




#replacing NA's in the frequency counts with 0's
df[c("freq.x", "freq.y")][is.na(df[c("freq.x", "freq.y")])] <- 0

total <- as.numeric(df$freq.x) + as.numeric(df$freq.y)
df<-cbind(df, total)



#dealing with missing data
#imputing mean for temperature and average wind because I don't have any better ideas
#assuming all days with missing data for precip and thunder were days when that didn't occur
df$Reason.x<-impute(df$Reason.x, "Truancy- classroom")
df$Reason.y<-impute(df$Reason.y, "Behavioral violation")
df$TOBS<-impute(df$TOBS, mean)
df$AvgWind<-impute(df$AvgWind, mean)
df$Precip<-impute(df$Precip, 0)
df$Thunder<-impute(df$Thunder, 0)

#making categorical variables factors for regression
df$phaseid <- as.factor(df$phaseid)
df$TOBS <- as.numeric(df$TOBS)
df$AvgWind <- as.numeric(df$AvgWind)
df$Precip <- as.numeric(df$Precip)
df$Thunder <- as.factor(df$Thunder)


#creating day by day dataframes
df_Monday<-subset(df, df$day == "Monday")
df_Tuesday<-subset(df, df$day == "Tuesday")
df_Wednesday<-subset(df, df$day == "Wednesday")
df_Thursday<-subset(df, df$day == "Thursday")
df_Friday<-subset(df, df$day == "Friday")

#plotting behavioral violations on a date-by-date basis
ggplot(df, aes(Date, freq.y, colour=factor(phaseid))) + geom_point() + 
   xlab("") + ylab("Daily Violations")

#plotting creep-u-leaps on a date-by-date basis
plot<-ggplot(df, aes(Date, freq.x, colour=factor(phaseid))) + geom_point() + 
  xlab("") + ylab("Daily Truancies")
plot + stat_smooth(method=lm, fullrange=FALSE)


#checking normality of distribution of referrals - it's not normal
#looks more like a poisson distribution
plot(density(df$total), main="Density Plot: Total", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(df$total), 2)))  # density plot for 'speed'


reg_behavior<-glm(formula = freq.x ~ phaseid + TOBS + AvgWind + Precip + Thunder+ day, data = df, na.action = na.exclude, family = poisson())
summary(reg_behavior)

reg_truancy<-glm(formula = freq.y ~ phaseid + TOBS + AvgWind + Precip + Thunder + day, data = df, family = poisson())
summary(reg_truancy)

reg_total<-glm(formula = total ~ phaseid + TOBS + AvgWind + Precip + Thunder + day, data = df, family = poisson())
summary(reg_total)

#reg_total<-glm(formula = total ~ TOBS + AvgWind, data = df, family = poisson())

#Testing for effect size
library (effsize)
phaseflag<-rep(0, length(df$total))
df<-cbind(df, phaseflag)

for(i in 1: nrow(df)){ 
  if(df$phaseid[i] == 3){
    df$phaseflag[i] <- 1 #1 means it's a full moon
  }
#0 means it's not
}
cohen.d(df$total, df$phaseflag)



r1<-glm(formula = total ~ TOBS + phaseid + day, data = df, family = poisson()) 

#The purpose of this function is to create graphs containing the predicted number of referrals on a day by day basis
#Predictions are based on the r1 glm (considering temperature, lunar phase, and day of the week)
DailyGraphs<-function(df, DayoftheWeek){

##Multiple Predictors
##https://stackoverflow.com/questions/20451927/plotting-one-predictor-of-a-model-that-has-several-predictors-with-ggplot


#need to adjust next row so that the lines go all the way across rather than chunked out like they are
model.dat <- data.frame(Treat = rep(levels(df$phaseid), each = 250), TOBS=rep(seq(0.1, 100, 0.1)), Day = rep(DayoftheWeek))

#function to make a prediction based on lunar phase and varying observed temperature to predict what the number of referrals will be on a given day
makingthelines<-function(lunarphase){
model.dat1 <- data.frame(Treat = rep(lunarphase, each = 1000), TOBS=rep(seq(0.1, 100, 0.1)), Day = rep(DayoftheWeek))
colnames(model.dat1)<-c("phaseid", "TOBS", "day")
model.dat1$phaseid<-as.factor(model.dat1$phaseid)
#model.dat1<-cbind(model.dat1, rep(DayoftheWeek))
prediction.dat1 <- data.frame(model.dat1, Pred=predict(r1, model.dat1), Day = rep(DayoftheWeek))
return(prediction.dat1)}


#loop to create the four lines based on the varying lunar phase
for(i in 1:4){
  name<- paste("prediction.line", i, sep = "")
  assign(name, makingthelines(i))
}



prediction.line<-rbind(prediction.line1, prediction.line2, prediction.line3, prediction.line4)

colnames(model.dat)<-c("phaseid", "TOBS", "day")
prediction.dat <- data.frame(model.dat, Pred=predict(r1, model.dat))
head(prediction.dat)
p<-ggplot(df,aes(x=TOBS,y=total,color=phaseid))+geom_point()+
  geom_line(data=prediction.line,aes(x=TOBS,y=Pred,color=phaseid)) 
return(p)} #function returns the graph without any labels

p<-DailyGraphs(df_Monday, "Monday")
p<-DailyGraphs(df_Tuesday, "Tuesday")
p<-DailyGraphs(df_Wednesday, "Wednesday")
p<-DailyGraphs(df_Thursday, "Thursday")
p<-DailyGraphs(df_Friday, "Friday")
p + labs(x = "Temperature", y = "Disciplinary Referrals") + ggtitle("                     Monday")
p + labs(x = "Temperature", y = "Disciplinary Referrals") + ggtitle("                     Tuesday")
p + labs(x = "Temperature", y = "Disciplinary Referrals") + ggtitle("                   Wednesday")
p + labs(x = "Temperature", y = "Disciplinary Referrals") + ggtitle("                     Thursday")
p + labs(x = "Temperature", y = "Disciplinary Referrals") + ggtitle("                     Friday")
