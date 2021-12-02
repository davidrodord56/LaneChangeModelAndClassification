### ANALYSIS and MODEL based on NGSIM
#Code implemented by B.E.E. Erick David Rodriguez for PhD program in Intelligent systems at Universidad de las Americas Puebla
#Advisor of Course DSI-5211 Statistical Learning Dr. Gibran Etcheverry
#Based on Work by companies for the Federal Highway Administration: https://ops.fhwa.dot.gov/trafficanalysistools/ngsim.htm
#Dataset from: https://data.transportation.gov/Automobiles/Next-Generation-Simulation-NGSIM-Vehicle-Trajector/8ect-6jqj

#First we load the libraries required for the model
{
library('tidyverse')
library(data.table)
library(cluster)
library(factoextra)
library(pracma)
library(ggplot2)
library(tibble)
library(dplyr)
# library(plyr)
library(MASS)
library(caret)
library(gmodels)
library(tfplot)
library(ggpubr)
library("GGally")
library(devtools)
library(rgl)
library(reshape)
library(adabag)
library(caret)
library(gganimate)
library(transformr)}

#Load Functions
#MS to date changes date from miliseconds from 1970 to a Readable Date format
ms_to_date = function(ms, t0="1970-01-01", timezone) {
        ## @ms: a numeric vector of milliseconds (big integers of 13 digits)
        ## @t0: a string of the format "yyyy-mm-dd", specifying the date that
        ##      corresponds to 0 millisecond
        ## @timezone: a string specifying a timezone that can be recognized by R
        ## return: a POSIXct vector representing calendar dates and times
        sec = ms / 1000
        as.POSIXct(sec, origin=t0,timezone = timezone)
}


#LOAD DATASETS
##THE original dataset includes, some regions like Lankershim, peachtree and others, we will use i80 highway

##### Uncomment these lines for retrieving full dataset
{
# data_prueba = fread("Next_Generation_Simulation__NGSIM__Vehicle_Trajectories_and_Supporting_Data_FULL.csv")
#
## GET only the i80 dataset
# {
# Locations = levels(as.factor(data_prueba$Location))
# Locations_corrected = gsub("-", "", Locations)
# b=0
# for (i in Locations){
#   b=b+1
#   assign(paste0('data_',Locations_corrected[b]),subset(data_prueba,Location == Locations[b]))
# }
# remove(b,i)
# data = data_i80 ##Change this for another region
# remove(data_lankershim,data_peachtree,data_prueba,data_us101)
# remove(Locations,Locations_corrected)
# }
##################
}
#####

## IF data is prepared load only i80 dataset
{
data_i80 = fread('data_i80.csv')
data_i80$V1 = NULL
data=data_i80[with(data_i80, order(Global_Time)),]
data$newDate=ms_to_date(data$Global_Time - 2*60*60*1000, timezone="Americas/Los_Angeles")
remove(data_i80)
}

#For each Lane get end and start, as relative positions to the frames
{
lanesmax =vector()
lanesmin=vector()

for ( i in 1:length(levels(as.factor(data$Lane_ID))))
{
  data_prueba=subset(data, Lane_ID==i)
  lanesmax=c(lanesmax,max(data_prueba$Local_X))
  lanesmin=c(lanesmin,min(data_prueba$Local_X))
  data_prueba=0
}
        remove(data_prueba,i)
}

#Get the number of Cars in the dataset
## Number of Cars
{
diff_cars= n_distinct(levels(as.factor(data$Vehicle_ID)))
print(paste('There are',diff_cars, 'different Cars in the dataset'))
}

## plot the histograms of the Velocity and accelaeration of the cars and of lane
{
        hist(data$v_Acc*.305, main= "Histogram of accelerations in dataset", freq = TRUE, xlim = c(-5,5), breaks = 50) #From feet second square to meters

        hist(data$v_Vel*1.09, main= 'Velocity in km/h', freq =FALSE) # from Feet second to km/h
}

##Histogram of Velocity and accelaration
{
mu= data %>%  group_by(Lane_ID) %>% summarise(grp.meanV = mean(v_Vel*1.09), grp.medianV = median(v_Vel*1.09), grp.meanA=mean(v_Acc*.305))
#mu <- plyr::ddply(data, "Lane_ID", summarise, grp.mean=mean(v_Vel*1.09))
ggplot(data, aes(x = v_Vel*1.09,colour = as.factor(Lane_ID))) +
  geom_density(adjust = 4, size = 2) +
  geom_vline(data=mu, aes(xintercept=grp.meanV, color=as.factor(Lane_ID)),
             linetype="dashed", size =2) +
  scale_color_brewer(palette="Dark2")
}
{

ggplot(data, aes(x = v_Acc*.305,colour = as.factor(Lane_ID))) +
  geom_density(adjust = 10, size = 2) +
  # geom_vline(data=mu, aes(xintercept=grp.mean, color=as.factor(Lane_ID)),
  #            linetype="dashed", size =2) +
  scale_color_brewer(palette="Dark2")
}

## Subsetting for hours from 16:00 to 16:15
{
data = data[which( data$newDate < as.POSIXlt('2005-04-13 16:20:55'), arr.ind=TRUE)]
}

##Repeat histograms for subset from 16 to 16:15
##Histograma Velocidades por carril 16:00 16:15
{
mu <- plyr::ddply(data, "Lane_ID", summarise, grp.mean=mean(v_Vel*1.09))
ggplot(data, aes(x = v_Vel*1.09,colour = as.factor(Lane_ID))) +
  geom_density(adjust = 4, size = 2) +
    scale_color_brewer(palette="Dark2")
}
##Histograma Aceleraciones por carril 16:00 16:15
{
ggplot(data, aes(x = v_Acc*.305,colour = as.factor(Lane_ID))) +
  geom_density(adjust = 10, size = 2) +
  # geom_vline(data=mu, aes(xintercept=grp.mean, color=as.factor(Lane_ID)),
  #            linetype="dashed", size =2) +
  scale_color_brewer(palette="Dark2")
}


### PLot a single Car
## PLot Single Car change Variable carn if there is a specif car to follow
{
cars = unique(data$Vehicle_ID)
carn=sample(cars,1)
carn= 394
car=data[which(data$Vehicle_ID==carn)]
limits = c(0,95,0,1800)
plot(car$Local_X,car$Local_Y,col =as.factor(car$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
lines(car$Local_X,car$Local_Y)
abline(v= c(lanesmin))
print(carn)
remove(car,carn,cars)}

#Number of Cars from 16:00 to 16:15
{
diff_cars= n_distinct(levels(as.factor(data$Vehicle_ID)))
print(paste('There are',diff_cars, 'different Cars in the dataset from 16:00 to 16:15'))
  remove(diff_cars)
}

###################### SLOW Plot Lanes and Vehicles
{
plot(data$Local_X,data$Local_Y,col =as.factor(data$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
abline(v= c(lanesmin))

plot(data$Local_X,data$Local_Y,col =as.factor(data$Vehicle_ID), xlim = limits[1:2], ylim = limits[3:4])
}


##Summaries for cars
{
summaryCars= data %>%  group_by(Vehicle_ID) %>% summarise(n = n(), Caminito=list(Lane_ID),
                                                          firstLane= first(Lane_ID),
                                                          lastLane= last(Lane_ID),
                                                          start = first(newDate),
                                                          end = last(newDate),
                                                          speedAVG = mean(v_Vel*.305),
                                                          accAVG= mean(v_Acc*1.09))
summaryCars$nChanges = as.integer(0)
for(i in 1:length(summaryCars[[3]]))
{
  summaryCars[[3]][[i]] = summaryCars[[3]][[i]][which(c(1,diff(summaryCars[[3]][[i]])) != 0)]
  summaryCars[["nChanges"]][i] = as.integer(as.integer(length(summaryCars[[3]][[i]]))-1)
}
}

##Summary data and important results
{
summary(subset(summaryCars, select = -c(Caminito)))
summary(as.factor(summaryCars$nChanges))
sum(summary(as.factor(data$Lane_ID)))
summary(as.factor(summaryCars$lastLane))
summaryCars$totalT = as.numeric(summaryCars$end - summaryCars$start)
summary(summaryCars$totalT)
subsummary =  summaryCars %>%  group_by(nChanges) %>% summarise(n = n(), avgTime = mean(totalT),speedAVG=mean(speedAVG))
}
{
summaryLanes= data %>%  group_by(Lane_ID) %>% summarise(n = n(),Vehicles = list(Vehicle_ID),
                                                        speedAVG = mean(v_Vel*1.09),
                                                        accAVG= mean(v_Acc*.305),
                                                        speedSum = sum(v_Vel*1.09))

for(i in 1:length(summaryLanes[[3]]))
{
  summaryLanes[["nVehicles"]][i] = length(summaryLanes[[3]][[i]])
  summaryLanes[["diffVehicles"]][i] = as.integer(n_distinct(summaryLanes[[3]][[i]]))

}
   summaryLanes$TMS = summaryLanes$speedSum / summaryLanes$nVehicles
}

### Plot Summaries
{
plot(summaryLanes$Lane_ID,summaryLanes$speedAVG, main = "Avg Speed per lane")
lines(summaryLanes$Lane_ID,summaryLanes$speedAVG, lwd = 5 , col = 'red')

plot(summaryLanes$Lane_ID,summaryLanes$accAVG, main = "Avg Acceleration per lane")
lines(summaryLanes$Lane_ID,summaryLanes$accAVG, lwd = 5 , col = 'blue')


ggplot(data=summaryCars,aes(x=nChanges, y= speedAVG)) +geom_point()

ggplot(data=summaryCars, aes(x=speedAVG,colour = as.factor(nChanges))) +
  geom_density(adjust = 4, size = 2) +
  scale_color_brewer(palette="Dark2")+
  ggtitle("Density of SpeedAVG, group by Lane")


ggplot(data=summaryCars,aes(x=nChanges, y= accAVG)) + geom_point()

ggplot(data=summaryCars, aes(x=accAVG,colour = as.factor(nChanges))) +
  geom_density(adjust = 4, size = 2) +
  scale_color_brewer(palette="Dark2")+
  ggtitle("Density of AccAVG, group by Lane")

ggplot(data=summaryCars,aes(x=accAVG, y= speedAVG, col = as.factor(nChanges))) +geom_point()

plot(summaryLanes$Lane_ID,summaryLanes$nVehicles, main="Number of cars at any time (per Frame accumulative) " )
lines(summaryLanes$Lane_ID,summaryLanes$nVehicles)


plot(summaryLanes$Lane_ID,summaryLanes$diffVehicles, main = "Number of different cars passing any time per lane")
lines(summaryLanes$Lane_ID,summaryLanes$diffVehicles )


plot(summaryLanes$Lane_ID,summaryLanes$TMS,ylim=c(0,60) , main = "TMS of lanes")
lines(summaryLanes$Lane_ID,summaryLanes$TMS )

plot(subsummary$nChanges, subsummary$n, main = "Number of cars per number of changes of lane")
lines(subsummary$nChanges, subsummary$n)


plot(subsummary$nChanges, subsummary$avgTime, main = "Average time required to travel highway depending on number of lane changes")
lines(subsummary$nChanges, subsummary$avgTime)

}

###Summaries for Minutes
{
data1 =subset(summaryCars, summaryCars$start <= as.POSIXlt('2005-04-13 16:05:55'))
data2 = subset(summaryCars,summaryCars$start <= as.POSIXlt('2005-04-13 16:10:55'))
data2 = subset(data2, data2$start > as.POSIXlt('2005-04-13 16:05:55'))
data3 = subset(summaryCars,summaryCars$start > as.POSIXlt('2005-04-13 16:10:55'))

summaryLC1= data1 %>%  group_by(nChanges) %>% summarise(n = n(), speedAVG=mean(speedAVG), Time = mean(totalT))
summaryLC2= data2 %>%  group_by(nChanges) %>% summarise(n = n(), speedAVG=mean(speedAVG), Time = mean(totalT))
summaryLC3= data3 %>%  group_by(nChanges) %>% summarise(n = n(), speedAVG=mean(speedAVG), Time = mean(totalT))


par(mfrow=c(1,3))
plot(summaryLC1$nChanges, summaryLC1$Time, sub="From 16:00 to 16:05")
lines(summaryLC1$nChanges, summaryLC1$Time)
plot(summaryLC2$nChanges, summaryLC2$Time, main = "Avg time to travel highway depending on lane changes",sub="From 16:05 to 16:10")
lines(summaryLC2$nChanges, summaryLC2$Time)
plot(summaryLC3$nChanges, summaryLC3$Time, sub="From 16:10 to 16:15")
lines(summaryLC3$nChanges, summaryLC3$Time)

par(mfrow=c(1,1))
}

### Gif generator
### SLOW - AVOID
{
la = data %>%  group_by(Lane_ID) %>% summarise(grp.lane = min(Local_X))
gif_data = data
a = ggplot(gif_data, aes(x= Local_X, y =Local_Y, colour = as.factor(Vehicle_ID), shape = as.factor(Lane_ID))) +
  geom_point() +
  geom_vline(data=la, aes(xintercept=grp.lane, color=as.factor(Lane_ID)),
             linetype="dashed", size =2) +
  theme(legend.position = 'none')+
  labs(title = 'Frame: {frame_time}') +
  transition_time(Frame_ID) +
  ease_aes('linear')

animate(a,duration = 100, fps  =  20, height= 1800 , width = 400)

anim_save("Final2.gif")
}




################ MODELS and Processing DATA
################ Here the data processing starts, the base is still added if you only want to run the models.

##DATA LOAD

data_i80 = fread('data_i80.csv')

data_i80$V1 = NULL
data_i80$O_Zone = NULL
data_i80$D_Zone = NULL
data_i80$Int_ID= NULL
data_i80$Section_ID= NULL
data_i80$Direction= NULL
data_i80$Movement= NULL

data=data_i80[with(data_i80, order(Global_Time)),]
data$newDate=ms_to_date(data$Global_Time - 2*60*60*1000, timezone="Americas/Los_Angeles")

lanesmax =vector()
lanesmin=vector()

{
for ( i in 1:length(levels(as.factor(data$Lane_ID))))
  {
  data_prueba=subset(data, Lane_ID==i)
  lanesmax=c(lanesmax,max(data_prueba$Local_X))
  lanesmin=c(lanesmin,min(data_prueba$Local_X))
  data_prueba=0
  }

data = data[which( data$newDate < as.POSIXlt('2005-04-13 16:20:55'), arr.ind=TRUE)]
diff_cars= n_distinct(levels(as.factor(data$Vehicle_ID)))
print(paste('There are',diff_cars, 'different Cars in the dataset'))
remove(data_i80, diff_cars, mu,i,data_prueba)
}

{
summaryCars= data %>%  group_by(Vehicle_ID) %>% summarise(n = n(), Caminito=list(Lane_ID),
                                                          framesChange=list(Frame_ID),
                                                          firstLane= first(Lane_ID),
                                                          lastLane= last(Lane_ID),
                                                          start = first(newDate),
                                                          end = last(newDate),
                                                          speedAVG = mean(v_Vel*.305),
                                                          accAVG= mean(v_Acc*1.09))
summaryCars$nChanges = as.integer(0)
summaryCars$cFrames = as.integer(0)

for(i in 1:length(summaryCars[[3]]))
{
  summaryCars[[3]][[i]] = summaryCars[[3]][[i]][which(c(1,diff(summaryCars[[3]][[i]])) != 0)]
  summaryCars[["nChanges"]][i] = as.integer(as.integer(length(summaryCars[[3]][[i]]))-1)
}

}

### Merge Data with lanes for preprocessing purposes
{
startend = summaryCars[c("firstLane",'lastLane','Vehicle_ID')]
data= merge(data,startend, by = "Vehicle_ID")
}

options(warn = - 1)


######## first match, following and preceding cars to obtain gap
datalist = list()
frames_available = unique(data$Frame_ID)
for (i in 1:length(frames_available)){
  framen= frames_available[i]
  a= subset(data, Frame_ID == framen)

  a$YPre = a$Local_Y[match(a$Preceding,a$Vehicle_ID)]
  a$YFol = a$Local_Y[match(a$Following,a$Vehicle_ID)]
  datalist[[i]] = a
}
big_data <- data.table::rbindlist(datalist)
big_data$d_PRE =  big_data$YPre - big_data$Local_Y
big_data$d_FOLL = big_data$Local_Y - big_data$YFol
big_data$GAP = big_data$d_PRE + big_data$d_FOLL

#### add preceding and following for left and right, to find gaps
carss = list()
frames_available = unique(big_data$Frame_ID)

for (i in 1:length(frames_available)){
  frame= frames_available[i]
  car=big_data[which(big_data$Frame_ID==frame)]
  car=car[with(car, order(Local_Y)),]
  car$precedinglaneR = as.integer(0)
  car$precedinglaneL = as.integer(0)
  car$followinglaneR = as.integer(0)
  car$followinglaneL = as.integer(0)

  for ( j in car$Vehicle_ID)
  {

   a = (car$Local_Y < car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == (car$Lane_ID[match(j,car$Vehicle_ID)]-1))
   car$followinglaneL[match(j,car$Vehicle_ID)] = car$Vehicle_ID[max(which(a))]

   b = (car$Local_Y > car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == (car$Lane_ID[match(j,car$Vehicle_ID)]-1))
   car$precedinglaneL[match(j,car$Vehicle_ID)] = car$Vehicle_ID[min(which(b))]

   c = (car$Local_Y < car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == (car$Lane_ID[match(j,car$Vehicle_ID)]+1))
   car$followinglaneR[match(j,car$Vehicle_ID)] = car$Vehicle_ID[max(which(c))]

   d = (car$Local_Y > car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == (car$Lane_ID[match(j,car$Vehicle_ID)]+1))
   car$precedinglaneR[match(j,car$Vehicle_ID)] = car$Vehicle_ID[min(which(d))]

  }

  car$YPreL = car$Local_Y[match(car$precedinglaneL,car$Vehicle_ID)]
  car$YFollL = car$Local_Y[match(car$followinglaneL,car$Vehicle_ID)]
  car$YPreR = car$Local_Y[match(car$precedinglaneR,car$Vehicle_ID)]
  car$YFollR = car$Local_Y[match(car$followinglaneR,car$Vehicle_ID)]

  carss[[i]] = car



}

new_big_data <- data.table::rbindlist(carss)
new_big_data$d_PRE_Right =  new_big_data$YPreR - new_big_data$Local_Y
new_big_data$d_PRE_Left =  new_big_data$YPreL - new_big_data$Local_Y
new_big_data$d_FOLL_Right = new_big_data$Local_Y - new_big_data$YFollR
new_big_data$d_FOLL_Left = new_big_data$Local_Y - new_big_data$YFollL





###Arreglo lastFRameeeee

summaryCars= new_big_data %>%  group_by(Vehicle_ID) %>% summarise(n = n(), CaminitoRef=list(Lane_ID),
                                                           Caminito=list(Lane_ID),
                                                          Frames =list(Frame_ID),
                                                          firstLane= first(Lane_ID),
                                                          lastLane= last(Lane_ID),
                                                          start = first(newDate),
                                                          end = last(newDate),
                                                          speedAVG = mean(v_Vel*.305),
                                                          accAVG= mean(v_Acc*1.09))
summaryCars$nChanges = as.integer(0)
summaryCars$FrameChange = as.integer(0)
i=4
for(i in 1:length(summaryCars[[3]]))
{

  summaryCars[[4]][[i]] = summaryCars[[3]][[i]][which(c(1,diff(summaryCars[[3]][[i]])) != 0)]
  summaryCars[[5]][[i]] = summaryCars[[5]][[i]][which(diff(summaryCars[[3]][[i]]) != 0)+1]
  summaryCars[["nChanges"]][i] = as.integer(as.integer(n_distinct(summaryCars[[3]][[i]]))-1)
}
summaryCars$CaminitoRef =NULL

###### include information about lane change
datalist3 = list()
frames_available = as.integer(unique(data$Frame_ID))
# frames_available=order(frames_available)
frames_available=sort(frames_available) #####Important line

for (i in 2:length(frames_available)){
  framen= frames_available[i]
  frameb= frames_available[i-1]
  a= subset(new_big_data, Frame_ID == framen)
  b= subset(new_big_data, Frame_ID == frameb)

  a$previousLane = as.integer(0)

  for ( j in a$Vehicle_ID)
  {
     l = b$Lane_ID[match(j,b$Vehicle_ID)]
     a$previousLane[match(j,a$Vehicle_ID)] = l
  }
  datalist3[[i]] = a
}

first = subset(new_big_data, Frame_ID == frames_available[1])
first$previousLane = NA
datalist3[[1]] = first


prueba = data.table::rbindlist(datalist3)

data$LaneChange =as.integer(0)
prueba= prueba %>% mutate(LaneChange = ifelse(previousLane ==  Lane_ID, 0, ifelse(previousLane >  Lane_ID, 1, 2)))
prueba$LaneChange =as.integer(prueba$LaneChange)

n_distinct(which(prueba$LaneChange > 0))
inttt = prueba$LaneChange
summary(inttt)

##### add vel and acc of adj cars
datalist4 = list()
# prueba = fread('PruebaCompleto.csv')
frames_available = unique(prueba$Frame_ID)
for (i in 1:length(frames_available)){
  framen= frames_available[i]
  a= subset(prueba, Frame_ID == framen)

  a$VPreL = a$v_Vel[match(a$precedinglaneL,a$Vehicle_ID)]
  a$VPreR = a$v_Vel[match(a$precedinglaneR,a$Vehicle_ID)]
  a$ACCPreL = a$v_Acc[match(a$precedinglaneL,a$Vehicle_ID)]
  a$ACCPreR = a$v_Acc[match(a$precedinglaneR,a$Vehicle_ID)]

  a$VFolL = a$v_Vel[match(a$precedinglaneL,a$Vehicle_ID)]
  a$VFolR = a$v_Vel[match(a$precedinglaneR,a$Vehicle_ID)]
  a$ACCFolL = a$v_Acc[match(a$precedinglaneL,a$Vehicle_ID)]
  a$ACCFolR = a$v_Acc[match(a$precedinglaneR,a$Vehicle_ID)]

  a$VFol = a$v_Vel[match(a$Following,a$Vehicle_ID)]
  a$ACCFol = a$v_Acc[match(a$Following,a$Vehicle_ID)]
  a$VPre = a$v_Vel[match(a$Preceding,a$Vehicle_ID)]
  a$ACCPre = a$v_Acc[match(a$Preceding,a$Vehicle_ID)]


  datalist4[[i]] = a
}

last_prueba <- data.table::rbindlist(datalist4)

# last_prueba = fread('PruebaCompleto.csv')
last_prueba$precedinglaneR[is.na(last_prueba$precedinglaneR)] = as.integer(0)
last_prueba$precedinglaneL[is.na(last_prueba$precedinglaneL)] = as.integer(0)
last_prueba$followinglaneR[is.na(last_prueba$followinglaneR)] = as.integer(0)
last_prueba$followinglaneL[is.na(last_prueba$followinglaneL)] = as.integer(0)

last_prueba$YPre[is.na(last_prueba$YPre)] = as.integer(1800)
last_prueba$YFol[is.na(last_prueba$YFol)] = as.integer(0)
last_prueba$d_PRE =  last_prueba$YPre - last_prueba$Local_Y
last_prueba$d_FOLL = last_prueba$Local_Y - last_prueba$YFol
last_prueba$GAP = last_prueba$d_PRE + last_prueba$d_FOLL
last_prueba$YPreL[is.na(last_prueba$YPreL)] = as.integer(1800)
last_prueba$YFollL[is.na(last_prueba$YFollL)] = as.integer(0)
last_prueba$YPreR[is.na(last_prueba$YPreR)] = as.integer(1800)
last_prueba$YFollR[is.na(last_prueba$YFollR)] = as.integer(0)
last_prueba$d_PRE_Right =  last_prueba$YPreR - last_prueba$Local_Y
last_prueba$d_PRE_Left =  last_prueba$YPreL - last_prueba$Local_Y
last_prueba$d_FOLL_Right = last_prueba$Local_Y - last_prueba$YFollR
last_prueba$d_FOLL_Left = last_prueba$Local_Y - last_prueba$YFollL

last_prueba$LaneChange[is.na(last_prueba$LaneChange)] = as.integer(0)

last_prueba$VPreL[is.na(last_prueba$VPreL)] = 0
last_prueba$VPreL[last_prueba$VPreL == 0] = max(last_prueba$VPreL)
last_prueba$VPreR[is.na(last_prueba$VPreR)] = 0
last_prueba$VPreR[last_prueba$VPreR == 0] = max(last_prueba$VPreR)
last_prueba$VPre[is.na(last_prueba$VPre)] = 0
last_prueba$VPre[last_prueba$VPre == 0] = max(last_prueba$VPre)

last_prueba$ACCFolL[is.na(last_prueba$ACCFolL)] = 0
last_prueba$ACCFolR[is.na(last_prueba$ACCFolR)] = 0
last_prueba$ACCPreL[is.na(last_prueba$ACCPreL)] = 0
last_prueba$ACCPreR[is.na(last_prueba$ACCPreR)] = 0
last_prueba$ACCFol[is.na(last_prueba$ACCFol)] = 0
last_prueba$ACCPre[is.na(last_prueba$ACCPre)] = 0

last_prueba$VFolL[is.na(last_prueba$VFolL)] = 0
last_prueba$VFolL[last_prueba$VFolL == 0] = min(last_prueba$VPreL)
last_prueba$VFolR[is.na(last_prueba$VFolR)] = 0
last_prueba$VFolR[last_prueba$VFolR == 0] = min(last_prueba$VPreR)
last_prueba$VFol[is.na(last_prueba$VFol)] = 0
last_prueba$VFol[last_prueba$VFol == 0] = min(last_prueba$VPre)



Labels = as.factor(last_prueba$LaneChange)
summary(Labels)

write.csv(last_prueba,'last_prueba.csv')
saveRDS(last_prueba, file = "last_prueba.rds")


#### You can Skip to this line if the last_prueba dataset is available
# last_prueba = fread('lastprueba.csv')
last_prueba = readRDS(file = "last_prueba.rds")

#### DATA for Models


datachange = subset(last_prueba, LaneChange > 0)
data_nochange = subset(last_prueba,LaneChange = 0)
data_samplenochange = sample_n(data_nochange, 600)
datafull = rbind(datachange,data_samplenochange)
datafull$LaneChange = as.factor(datafull$LaneChange)

data_setM1= subset(datafull, select = c(Local_X,Local_Y, v_Vel, v_Acc, v_Class,VFol,VPre, ACCFol, ACCPre,
                                     Lane_ID,YPre,YFol,d_PRE,d_FOLL, GAP, YPreL, YFollL, YPreR,YFollR,
                                     d_PRE_Right,d_PRE_Left,d_FOLL_Left,d_FOLL_Right, VPreL,VPreR,ACCPreL,ACCPreR,
                                     VFolL,VFolR,ACCFolL,ACCFolR,LaneChange))

data_setM2= subset(datafull, select = c(Vehicle_ID, v_Vel, v_Acc, v_Class, Lane_ID,d_PRE,d_FOLL, GAP,VFol,VPre, ACCFol,
                                                                        ACCPre,
                                           d_PRE_Right,d_PRE_Left,d_FOLL_Left,d_FOLL_Right,
                                           VPreL,VPreR,ACCPreL,ACCPreR,VFolL,VFolR,ACCFolL,
                                           ACCFolR,LaneChange))

indexes=createDataPartition(data_setM1$LaneChange, p=.80, list = F)
train1 = data_setM1[indexes, ]
test1 = data_setM1[-indexes, ]

train2 = data_setM2[indexes, ]
test2 = data_setM2[-indexes, ]

model1 = boosting(LaneChange~., data=train1, boos=TRUE, mfinal=1000)
model2 = boosting(LaneChange~., data=train2, boos=TRUE, mfinal=1000)

pred1 = predict(model1, test1)
print(pred1$confusion)
result = data.frame(test1$LaneChange, pred1$prob, pred1$class)
print(result)
confusionMatrix(pred1$confusion)

pred2 = predict(model2, test2)
print(pred2$confusion)
result = data.frame(test2$LaneChange, pred2$prob, pred2$class)
print(result)
confusionMatrix(pred2$confusion)

saveRDS(model1, file = "model1.rds")
saveRDS(model2, file = "model2.rds")
saveRDS(pred1, file = "prediction1.rds")
saveRDS(pred2, file = "prediction2.rds")
model1 = readRDS(file = "model1.rds")
model2 = readRDS(file = "model2.rds")


par(mar = c(7, 4, 2, 2) + 0.2)
barplot(sort(model1$importance,decreasing = TRUE),las=2, sub="Variables", main = "Importance of variables for model 1")
barplot(sort(model2$importance,decreasing = TRUE),las=2, sub="Variables", main = "Importance of variables for model 2")

