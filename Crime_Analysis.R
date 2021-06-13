#ui.R
#getwd()
# installed.packages()

#setwd("C:/Users/chira/OneDrive/Desktop/Crime")
# encounter_inline= read.csv("searchResults-20180521_encounters_inline.csv", header = TRUE, check.names = F, as.is=TRUE)
library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggrepel)
library(mltools)
library(caret)
library(ggmap)
library(tmap)
library(tidyverse)
library(maptools)
library(RColorBrewer)
library(data.table)
library(mltools)
library(rpart)
library(randomForest)
require(caTools)


getwd()
setwd("C:/Users/chira/OneDrive/Desktop/Crime")
dataset= read.csv("Crime_Data.csv", header = TRUE, check.names = F, as.is=TRUE)

# map <- get_map(location = 'India', zoom = 4)
# ggmap(map)
# roadmap <- get_map("India", maptype='roadmap', zoom = 10)
# 
# subset= dataset[dataset$`Crime Subcategory`=="CAR PROWL",]
# 
# freq= data.frame(table(subset$`Occurred Date`))
# 
# dataset$`Occurred Date`
# freq$Freq


#class(dataset$`Occurred Date`)
#class(freq$Var1)
# freq$Var1 = as.Date(freq$Var1,  format = "%d/%m/%Y")
# 
# ggplot(data= freq, aes(x=freq$Var1, y=freq$Freq)) +
#   geom_line(color = "#00AFBB", size = 1)+
#   geom_smooth()+
#   facet_wrap(~ dataset$`Crime Subcategory`)

#################################
#class(dataset$`Occurred Date`)
myvars <- c("Occurred Date", "Crime Subcategory")
newdata <- dataset[myvars]
#removing blank values
newdata = newdata[complete.cases(newdata), ]
newdata = newdata[!(newdata$`Crime Subcategory`==""), ]
#changing format to date for subset
newdata$`Occurred Date` = mdy(newdata$`Occurred Date`)
#newdata$`Occurred Date` = as.Date(newdata$`Occurred Date`, format= "%m/%d/%Y")
newdata <- subset(newdata, newdata$`Occurred Date` > as.Date("2010-01-01"))
#grouping by crimesubcategory and date
x= newdata %>% group_by(newdata$`Occurred Date`, newdata$`Crime Subcategory`) %>% tally()
names(x) <- c("Occurred_Date", "Crime_Subcategory","count")
x <- x[order(x$Crime_Subcategory),]
#x= x[complete.cases(x), ]
#x= x[!(x$Crime_Subcategory==""), ]

#class(x$Occurred_Date)
#x$Occurred_Date = as.Date(x$Occurred_Date, format= "%m/%d/%Y")
#x <- subset(x, x$Occurred_Date > as.Date("2004-01-01") )
#x <- x[order(x$Crime_Subcategory),]

ggplot(data= x, aes(x=x$Occurred_Date, y=x$count)) +
  geom_line(color = "#00AFBB", size = 0.5)+
  geom_smooth()+
  scale_x_date() +
  facet_wrap(~x$Crime_Subcategory, scales = "free")


#################################
####Crimes by day of the year####
newdata$DofY = yday(newdata$`Occurred Date`)
#x$DofY = yday(x$Occurred_Date)
x1= newdata[c("Crime Subcategory", "DofY")]
x1 = x1 %>% group_by(x1$`Crime Subcategory`, x1$DofY) %>% tally()
names(x1) <- c("Crime_Subcategory","Day of the Year","count")
x1 <- x1[order(x1$Crime_Subcategory),]
ggplot(data= x1, aes(x=x1$`Day of the Year`, y=x1$count)) +
  geom_line(color = "#00AFBB", size = 1)+
  geom_smooth()+
  #scale_x_date() +
  facet_wrap(~x1$Crime_Subcategory, scales = "free")

#################################
newdata$DofW <- weekdays(newdata$`Occurred Date`)
#x$DofW <- weekdays(x$Occurred_Date)
x2= newdata[c("Crime Subcategory", "DofW")]
x2 = x2 %>% group_by(x2$`Crime Subcategory`, x2$DofW) %>% tally()
names(x2) <- c("Crime_Subcategory","Day of the Week","count")
x2 <- x2[order(x2$Crime_Subcategory),]
ggplot(data= x2, aes(x=x2$`Day of the Week`, y=x2$count)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #scale_x_date() +
  facet_wrap(~x2$Crime_Subcategory, scales = "free")


####################################################################################
#Plotting on Map
#remove.packages("ggmap")
#devtools::install_github("dkahle/ggmap")


register_google(key = "AIzaSyA_M28Nrkkb2q-e206cfC4jbYVd4OGP_TE")
map.seattle <- get_map("Seattle")
ggmap(map.seattle)


#geo_reply = geocode("LAKEWOOD/SEWARD PARK, Seattle", output='all', messaging=TRUE, override_limit=TRUE)
#answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
# answer$status <- geo_reply$status
# answer$lat <- geo_reply$results[[1]]$geometry$location$lat
# answer$long <- geo_reply$results[[1]]$geometry$location$lng
# if (length(geo_reply$results[[1]]$types) > 0){
#   answer$accuracy <- geo_reply$results[[1]]$types[[1]]
# }
# answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
# answer$formatted_address <- geo_reply$results[[1]]$formatted_address



####### Finding Unique neighbourhoods and geocoding them ############
unique_n = unique(dataset$Neighborhood)
unique_n = data.frame(unique_n)
unique_n$unique_n = paste0(unique_n$unique_n, ", Seattle")

#temp_info <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)

i=1
for (i in 1:length(unique_n$unique_n)){
  nhood = unique_n$unique_n[i]
  geo_reply = geocode(nhood, output='all', messaging=TRUE, override_limit=TRUE)
  unique_n$status[i] <- geo_reply$status
  unique_n$lat[i] <- geo_reply$results[[1]]$geometry$location$lat
  unique_n$long[i] <- geo_reply$results[[1]]$geometry$location$lng
  if (length(geo_reply$results[[1]]$types) > 0){
    unique_n$accuracy[i] <- geo_reply$results[[1]]$types[[1]]
  }
  unique_n$address_type[i] <- paste(geo_reply$results[[1]]$types, collapse=',')
  unique_n$formatted_address[i] <- geo_reply$results[[1]]$formatted_address
}

################################################################################
#n_test = data.frame(dataset$Neighborhood)
#names(n_test) <- c("unique_n")
#n_test$unique_n = paste0(n_test$unique_n, ", Seattle")

################# Merging Geocoded data with dataset #############################
dataset$Neighborhood = paste0(dataset$Neighborhood, ", Seattle")
names(unique_n)[1]<-"Neighborhood"
m1= merge(dataset,unique_n,by=c("Neighborhood"))

names(m1)[14]<-"lon"

Seattle.map <- get_map("Seattle", zoom = 11)
#ggmap(Seattle.map)+
#  geom_point(data=m1,
#           aes(x=lon,y=lat,color="red"),
#           size=2,alpha=.7)

#### Maps divided by different categories Subcategory ##################################
#Sub= unique(dataset$`Crime Subcategory`)[1:3]

SubPersonal= c("AGGRAVATED ASSAULT-DV","AGGRAVATED ASSAULT","SEX OFFENSE-OTHER")
SubProperty= c("THEFT-SHOPLIFT","BURGLARY-RESIDENTIAL","MOTOR VEHICLE THEFT")
SubStatutory= c("NARCOTIC","DUI","MOTOR VEHICLE THEFT")

m6 = data.frame(dataset$`Crime Subcategory`)
m6 = m6 %>% group_by(m6$dataset..Crime.Subcategory.)  %>% tally()

m3= dataset %>% filter(dataset$`Crime Subcategory` %in%  SubStatutory)
m4= merge(m3,unique_n,by=c("Neighborhood"))
m5 = m4 %>% group_by(m4$Neighborhood, m4$`Crime Subcategory`,m4$lat, m4$lon) %>% tally()
names(m5) <- c("Neighborhood","Crime_Subcategory","lat","lon","count")



#To export image 
#png("image1.jpeg", width = 1200, height = 1200)
#dev.off()
ggmap(Seattle.map)+
  geom_point(data = m5,
             aes(x = lon, y = lat, size = count , color = count, alpha = 7/10)) +
  scale_colour_gradient(high="red",low='blue')+
  xlab('longitude')+ylab('latitude')+
  facet_wrap(~Crime_Subcategory)
#### Map for one Subcategory ############################


#### 911 Calls ##########################################
dataset911= read.csv("Seattle_Police_Department_911_Incident_Response.csv", header = TRUE, check.names = F, as.is = TRUE)
dataset911= dataset911[,-c(16,17,18,19)]
dataset911[complete.cases(dataset911), ]
dataset911$Event_Clearance_Group = dataset911$`Event Clearance Group`
dataDanger= read.csv("DN.csv", header = TRUE, check.names = F, as.is = TRUE)
dataDanger$label <-paste(dataDanger$Rank, dataDanger$Location, sep="-")

col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"

dataset911$ymd = mdy_hms(dataset911$`Event Clearance Date`)
dataset911$year = year(dataset911$ymd)

unique(dataset911$`Event Clearance Group`)
#dataset911Sub <- dataset911 %>% filter(year>=2014)
######### Plotting density graph for selected crime call type ####################################### 
dataset911Sub <- filter(dataset911, dataset911$`Event Clearance Group` %in% c('ROBBERY', 'DISTURBANCES', 
                                                'SUSPICIOUS CIRCUMSTANCES', 'MOTOR VEHICLE COLLISION INVESTIGATION',
                                                'HOMICIDE','NARCOTICS COMPLAINTS'))
dataset911Sub <- dataset911Sub %>% filter(year>=2014)

ggmap(Seattle.map)+
  stat_density2d(
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha =..level..),
    size = 0.2, bins = 30, data = dataset911Sub, geom = "polygon") +
  geom_density2d(data = dataset911Sub, aes(x = Longitude, y = Latitude), size = 0.3) +
  facet_wrap(~ Event_Clearance_Group)

#####################################################################################################

############## Plotting Dangerous Areas in Seattle ##################################################
dset911 <- dataset911 %>% filter(year>=2010)
dset911Dangerous <-filter(dset911, dset911$`Event Clearance Group` %in% c('TRESPASS', 'ASSAULTS', 'SUSPICIOUS CIRCUMSTANCES', 
                                                      'BURGLARY', 'PROWLER', 'ASSAULTS', 'PROPERTY DAMAGE', 
                                                      'ARREST', 'NARCOTICS COMPLAINTS','THREATS', 'HARASSMENT', 'WEAPONS CALLS',
                                                      'PROSTITUTION' , 'ROBBERY', 'FAILURE TO REGISTER (SEX OFFENDER)', 'LEWD CONDUCT', 
                                                      'HOMICIDE'))

ggmap(Seattle.map)+
  #stat_density2d(
  #  aes(x = Longitude, y = Latitude, fill = ..level.., alpha =..level..),
  #  size = 0.2, bins = 30, data = dset911, geom = "polygon") +
  #geom_density2d(data = dset911, aes(x = Longitude, y = Latitude), size = 0.3) +
  geom_point(aes(x = x, y = y, stroke = 2), colour=col4, data = dataDanger, size =2.5) + 
  geom_label_repel(
    aes(x, y, label = label),
    data=dataDanger,
    family = 'Times', 
    size = 4, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') 

#####################################################################################################

############################### -- 911 Calls -- #####################################################


#### Visualizing police beats ###########################


area <- readShapePoly("spdbeat_WGS84.shp")
colors <- brewer.pal(9, "BuGn")

area.points <- fortify(area)

ggmap(Seattle.map) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

#### Visualizing police beats ###########################
m2= m1[c("Crime Subcategory", "Neighborhood","lat","lon")]
m2 = m2 %>% group_by(m2$Neighborhood, m2$`Crime Subcategory`,m2$lat, m2$lon) %>% tally()
names(m2) <- c("Neighborhood","Crime_Subcategory","lat","lon","count")
m2 <- x2[order(x2$Crime_Subcategory),]

m3= m3 %>% filter(Crime_Subcategory=="HOMICIDE")
m2.sub <- m2 %>% filter(m2, m2$Crime_Subcategory=="HOMICIDE")
m2.sub <- filter(m2, m2$Crime_Subcategory %in% c('CAR PROWL','ARSON','HOMICIDE'))
colnames(m2)

ggmap(Seattle.map)+
  geom_point(data = m2,
                 aes(x = lon, y = lat, color = m2$count)) +
  facet_wrap(~Crime_Subcategory)
  
################################  PREDICTION  ####################################################
#install.packages('mltools')

# Cleaning the dataset
datasetTest = read.csv("Crime_Data.csv", header = TRUE, check.names = F, as.is=TRUE)
datasetTest = datasetTest[!(datasetTest$`Crime Subcategory`==""), ]
datasetTest = datasetTest[!(datasetTest$Precinct==""), ]
datasetTest = datasetTest[!(datasetTest$Precinct=="UNKNOWN"), ]
datasetTest[(datasetTest$Precinct=="UNKNOWN"), ]

datasetTest$OccurredDateTime = paste(datasetTest$`Occurred Date`, " ", datasetTest$Occurred_Time, sep = "")
datasetTest$OccurredDateTime= mdy_hm(datasetTest$OccurredDateTime)


#class(datasetTest$Occurred_Time)
datasetTest= datasetTest[complete.cases(datasetTest), ]
datasetTest= subset(datasetTest, datasetTest$Neighborhood != "UNKNOWN")

#dset <- datasetTest %>% filter(year>=2010)
#datasetTest$`Occurred Date` = as.Date(datasetTest$`Occurred Date`, format= "%m/%d/%Y")

datasetTrain = datasetTest
dsetTrain <- datasetTrain %>% filter(year(datasetTrain$OccurredDateTime)>=2010)
dsetTrain$day = day(dsetTrain$OccurredDateTime)
dsetTrain$WofY = week(dsetTrain$OccurredDateTime)
dsetTrain$month = month(dsetTrain$OccurredDateTime)
dsetTrain$year = year(dsetTrain$OccurredDateTime)
dsetTrain$hour = hour(dsetTrain$OccurredDateTime)
dsetTrain$minute = minute(dsetTrain$OccurredDateTime)
dsetTrain$DofW = wday(dsetTrain$OccurredDateTime)
dsetTrain$DofY = yday(dsetTrain$OccurredDateTime)

unique(dsetTrain$Precinct)
str(dsetTrain)

unique(dsetTrain$`Crime Subcategory`)
unique(dsetTrain$Precinct)
unique(dsetTrain$`Primary Offense Description`)

#dsetTrain[dsetTrain$Precinct=='UNKNOWN'||dsetTrain$Precinct=='UNKNOWN']

dsetTrain$Precinct = as.factor(dsetTrain$Precinct)
dsetTrain$Beat = as.factor(dsetTrain$Beat)
dsetTrain$Sector = as.factor(dsetTrain$Sector)
dsetTrain$Neighborhood = as.factor(dsetTrain$Neighborhood)

tData <- dsetTrain[c(8,11)]

trainData <- one_hot(as.data.table(tData))
#trainData = tData

#trainData$day = dsetTrain$day
trainData$month = dsetTrain$month
trainData$WofY = dsetTrain$WofY
trainData$year = dsetTrain$year
#trainData$hour = dsetTrain$hour
trainData$DofW = dsetTrain$DofW
trainData$DofY = dsetTrain$DofY
trainData$Crime_Subcategory = dsetTrain$`Crime Subcategory`
#y = dsetTrain$`Crime Subcategory`

set.seed(101) 
sample = sample.split(trainData$Crime_Subcategory, SplitRatio = 0.80)
train = subset(trainData, sample == TRUE)
test  = subset(trainData, sample == FALSE)

colnames(train) <- make.names(names(train))
colnames(test) <- make.names(names(test))
train$Crime_Subcategory = as.factor(train$Crime_Subcategory)
test$Crime_Subcategory = as.factor(test$Crime_Subcategory)
#gsub("\\.\\.+", "_", colnames(train))
#gsub(" ", "_", names(ctm2))
#names(yourdf) <- make.names(names(train))


str(train, list.len=ncol(train))
str(test, list.len=ncol(test))
class(train$Crime_Subcategory)

SubPersonal= c("AGGRAVATED ASSAULT-DV","AGGRAVATED ASSAULT","SEX OFFENSE-OTHER", "ARSON",
              "RAPE","FAMILY OFFENSE-NONVIOLENT","WEAPON","PROSTITUTION","PORNOGRAPHY","SEX OFFENSE-OTHER",
              "DISORDERLY CONDUCT","HOMICIDE")

SubProperty= c("THEFT-SHOPLIFT","BURGLARY-RESIDENTIAL", "ROBBERY-RESIDENTIAL","THEFT-BUILDING"
               ,"ROBBERY-COMMERCIAL","THEFT-BICYCLE","ROBBERY-STREET","BURGLARY-COMMERCIAL",
               "THEFT-ALL OTHER","LOITERING")

SubStatutory= c("NARCOTIC","DUI","MOTOR VEHICLE THEFT","BURGLARY-COMMERCIAL-SECURE PARKING",
                "BURGLARY-RESIDENTIAL-SECURE PARKING","CAR PROWL","TRESPASS","LIQUOR LAW VIOLATION"
                ,"GAMBLE")

xIndex = which(train$Crime_Subcategory %in% SubPersonal)
train$Crime_Category = ""
train[xIndex,]$Crime_Category = "Personal_Crime"
xIndex1 = which(train$Crime_Subcategory %in% SubProperty)
train[xIndex1,]$Crime_Category = "Property_Crime"
xIndex2 = which(train$Crime_Subcategory %in% SubStatutory)
train[xIndex2,]$Crime_Category = "Statutory_Crime"

xIndex = which(test$Crime_Subcategory %in% SubPersonal)
test$Crime_Category = ""
test[xIndex,]$Crime_Category = "Personal_Crime"
xIndex1 = which(test$Crime_Subcategory %in% SubProperty)
test[xIndex1,]$Crime_Category = "Property_Crime"
xIndex2 = which(test$Crime_Subcategory %in% SubStatutory)
test[xIndex2,]$Crime_Category = "Statutory_Crime"

trainCategory <- train[, -69]
trainCategory$Crime_Category <- as.factor(trainCategory$Crime_Category)
class(trainCategory$Crime_Category)

testCategory <- test[, -69]
testCategory$Crime_Category <- as.factor(testCategory$Crime_Category)
class(testCategory$Crime_Category)

modfit.rf <- randomForest(Crime_Category ~. , data=trainCategory)
modfit.rf.All <- randomForest(Crime_Subcategory ~. , data=train)
modfit.rf.sampled <- randomForest(Crime_Subcategory ~. , data=trainSampled)

modfit.rf.pred <- predict(modfit.rf, testCategory[,-69])
modfit.rf.pred.All <- predict(modfit.rf.All, test[,-69])
modfit.rf.pred.sampled <- predict(modfit.rf.sampled, testSampled[,-69])
#caret::confusionMatrix(testCategory$Crime_Category, modfit.rf.pred, positive="1", mode="everything")

y_actual= testCategory$Crime_Category
y_pred = as.vector(modfit.rf.pred)
F1_Score(y_pred, y_actual)
mean(y_pred == y_actual)

y_actualAll= test$Crime_Subcategory
y_predAll = as.vector(modfit.rf.pred.All)
F1_Score(y_predAll, y_actualAll)
mean(y_predAll == y_actualAll)

y_actual_s= testSampled$Crime_Subcategory
y_pred_s = as.vector(modfit.rf.pred.sampled)
F1_Score(y_pred_s, y_actual_s)
mean(y_pred_s == y_actual_s)

print(modfit.rf)
print(modfit.rf.All)
varImpPlot(modfit.rf)
plot(modfit.rf)

# y_test= predict(modfit.rf, test[,-145])

# confusionMatrix(test[,145], y_test1$y_test)
# accuracy(test[,145], y_test1$y_test)
# class(y_test)
# y_test1 = data.frame(y_test)
#rf_model_1<-train(Crime_Subcategory~., data=train, method="rf")
#unique(train$Crime_Subcategory)


# ctrl <- trainControl(method = "repeatedcv", 
#                      number = 5, 
#                      repeats = 5, 
#                      verboseIter = FALSE)
# 
# set.seed(42)
# model_rf_under <- caret::train(Crime_Subcategory ~ .,
#                                data = smote_train,
#                                method = "rf",
#                                preProcess = c("scale", "center"),
#                                trControl = ctrl)
# 
# model_rf_normal <- caret::train(Crime_Subcategory ~ .,
#                                data = train,
#                                method = "rf",
#                                trControl = ctrl)

########### SAMPLING Data ######################################################
#install.packages("DMwR")
library(ROSE)
library(DMwR)
#data_balanced_under <- ovun.sample(Crime_Subcategory ~ ., data = train, method = "under",N=1000)$data
#table(train$Crime_Subcategory)

trainData$Crime_Subcategory = as.factor(trainData$Crime_Subcategory)
smote_train <- SMOTE(Crime_Subcategory ~ ., data  = trainData, perc.over = 500, perc.under = 300000)
table(train$Crime_Subcategory)
table(smote_train$Crime_Subcategory)

set.seed(101) 
sampleS = sample.split(smote_train$Crime_Subcategory, SplitRatio = 0.80)
trainSampled = subset(smote_train, sampleS == TRUE)
testSampled  = subset(smote_train, sampleS == FALSE)

colnames(trainSampled) <- make.names(names(trainSampled))
colnames(testSampled) <- make.names(names(testSampled))
trainSampled$Crime_Subcategory = as.factor(trainSampled$Crime_Subcategory)
testSampled$Crime_Subcategory = as.factor(testSampled$Crime_Subcategory)


#data_balanced_under = downSample(train, train$Crime_Subcategory)
#table(data_balanced_under$Crime_Subcategory)


#y_test= predict(model_rf_under, test[,-69])
#caret::confusionMatrix(y, y_test, positive="1", mode="everything")

#data(hacide)
# imbalance on training set
#table(hacide.train$cls)

# balanced data set with both over and under sampling
# data.balanced.ou <- ovun.sample(cls~., data=hacide.train,
#                                 N=nrow(hacide.train), p=0.5, 
#                                 seed=1, method="both")$data

######### KNN  #######################################################
#str(train)
#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(3333)
#knn_fit <- train(Crime_Subcategory~., data = train, method = "knn",
#                 trControl=trctrl,
#                 preProcess = c("center", "scale"),
#                 tuneLength = 10)

library(class)

x= train$Crime_Subcategory
y= test$Crime_Subcategory

class(train$Crime_Subcategory)

knn.5 <-  knn(train[,-69], test[,-69], x, k=5)
knn.20 <-  knn(train[,-69], test[,-69], x, k=20)
knn.40 <-  knn(train[,-69], test[,-69], x, k=40)
knn.80 <-  knn(train[,-69], test[,-69], x, k=80)
#Evaluation
mean(y == knn.5)
mean(y == knn.20)
mean(y == knn.40)
mean(y == knn.80)

F1_Score(y, knn.5)
F1_Score(y, knn.20)
F1_Score(y, knn.40)
F1_Score(y, knn.80)


#For 3 classes
x= trainCategory$Crime_Category
y= testCategory$Crime_Category

Knn.5 <-  knn(trainCategory[,-69], testCategory[,-69], x, k=5)
Knn.20 <-  knn(trainCategory[,-69], testCategory[,-69], x, k=20)
Knn.40 <-  knn(trainCategory[,-69], testCategory[,-69], x, k=40)
Knn.80 <-  knn(trainCategory[,-69], testCategory[,-69], x, k=80)
#Evaluation
mean(y == Knn.5)
F1_Score(y, Knn.5)
mean(y == Knn.20)
F1_Score(y, Knn.20)
mean(y == Knn.40)
F1_Score(y, Knn.40)
mean(y == Knn.80)
F1_Score(y, Knn.80)

#Sampled
x= trainSampled$Crime_Subcategory
y= testSampled$Crime_Subcategory

knn.5s <-  knn(trainSampled[,-69], testSampled[,-69], x, k=5)
knn.20s <-  knn(trainSampled[,-69], testSampled[,-69], x, k=20)

mean(y == knn.5s)
F1_Score(y, knn.5s)
mean(y == knn.20s)
F1_Score(y, knn.20s)


# library(MLmetrics)
# library(ModelMetrics)
# y1= data.frame(Knn.5)
# MultiLogLoss(y, y1)
# MultiLogLoss(y_true = y, y_pred = attr(Knn.5, "probabilities"))
# mlogLoss(y, Knn.5)
# length(Knn.5)
# class(Knn.5)
# caret::confusionMatrix(y, knn.20, positive="1", mode="everything")
# 
# MultiLogLoss(y, Knn.5)
# MultiLogLoss(y_true = testCategory$Crime_Category, y_pred = attr(Knn.5, "probabilities"))
# 
# MultiLogLoss <- function(y_pred, y_true) {
#   if (is.matrix(y_true) == FALSE) {
#     y_true <- model.matrix(~ 0 + ., data.frame(as.character(y_true)))
#   }
#   eps <- 1e-15
#   N <- nrow(y_pred)
#   y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
#   MultiLogLoss <- (-1 / N) * sum(y_true * log(y_pred))
#   return(MultiLogLoss)
# }
# 
# 
# library(caret)
# caret::confusionMatrix(y, knn.20, positive="1", mode="everything")

######### -- KNN -- #############################################

######### Naive Bayes ###########################################
library(e1071)

Naive_Bayes_Model=naiveBayes(Crime_Category ~., data=trainCategory)
Naive_Bayes_Model0=naiveBayes(Crime_Subcategory ~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,testCategory[,-69])
NB_Predictions0=predict(Naive_Bayes_Model0,test[,-69])

y= testCategory$Crime_Category
caret::confusionMatrix(y, NB_Predictions, positive="1", mode="everything")
F1_Score(y, NB_Predictions)

y= test$Crime_Subcategory
length(y)
caret::confusionMatrix(y, NB_Predictions0, mode="everything")
F1_Score(y, NB_Predictions0)


#####################################################################
