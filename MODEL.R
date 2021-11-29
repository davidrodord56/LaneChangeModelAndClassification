
### Libraries
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
}
##Model preparation

##Date Adjustement
{
ms_to_date = function(ms, t0="1970-01-01", timezone) {
        ## @ms: a numeric vector of milliseconds (big integers of 13 digits)
        ## @t0: a string of the format "yyyy-mm-dd", specifying the date that
        ##      corresponds to 0 millisecond
        ## @timezone: a string specifying a timezone that can be recognized by R
        ## return: a POSIXct vector representing calendar dates and times
        sec = ms / 1000
        as.POSIXct(sec, origin=t0,timezone = timezone)
}
}

data_i80 = fread('data_i80.csv')

data_i80$V1 = NULL

data=data_i80[with(data_i80, order(Global_Time)),]
data$newDate=ms_to_date(data$Global_Time - 2*60*60*1000, timezone="Americas/Los_Angeles")

lanesmax =vector()
lanesmin=vector()


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
remove(data_prueba, diff_cars, mu,i)


####Summary Cars
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
  summaryCars[["nChanges"]][i] = as.integer(as.integer(n_distinct(summaryCars[[3]][[i]]))-1)
}

}

### Merge Data with lanes for preprocessing purposes
startend = summaryCars[c("firstLane",'lastLane','Vehicle_ID')]
data= merge(data,startend, by = "Vehicle_ID")

#### Gat Cars that at any time passes for the 6th lane
adjacentCars = vector()
for(i in 1:length(summaryCars[[3]]))
  {
  if(6 %in% summaryCars[[3]][[i]]) {
    adjacentCars = c(adjacentCars,summaryCars[[1]][[i]] )
  }
  }

### Get Cars that start at 7th lane or in the ramp
importantVehicles = summaryCars$Vehicle_ID[which(summaryCars$firstLane == 7)]

### Get Dataframes for 7th lane and 6th Lane
data_7thLane = data[which(Vehicle_ID %in% importantVehicles)]
data_7thLane$DF = 7
data_7thLane$DFA = 6
data_7thLane$Lane_ID[data_7thLane$Local_X > lanesmin[7]] = as.integer(7)

data_6thLane = data[which(Vehicle_ID %in% adjacentCars)]
data_6thLane$DF = 6
data_6thLane$DFA = 7
data_6thLane$Lane_ID[data_6thLane$Local_X > lanesmin[7] & data_6thLane$firstLane == 7] = as.integer(7)

full = bind_rows(data_6thLane, data_7thLane)
full=full[!duplicated(full[ , c("Vehicle_ID","Frame_ID")]),]
full$Lane_ID = as.factor(as.integer((full$Lane_ID)))

{
summaryCars= full %>%  group_by(Vehicle_ID) %>% summarise(n = n(), Caminito=list(Lane_ID),
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
  summaryCars[["nChanges"]][i] = as.integer(as.integer(n_distinct(summaryCars[[3]][[i]]))-1)
}

}



####### plot cars
carn= 254
car=full[which(full$Vehicle_ID==carn)]
limits = c(0,95,0,1800)
plot(car$Local_X,car$Local_Y,col =as.factor(car$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
lines(car$Local_X,car$Local_Y)
abline(v= c(lanesmin))
print(carn)
remove(car,carn,cars)

####### Check REsults
{
plot(data_7thLane$Local_X,data_7thLane$Local_Y,col =as.factor(data_7thLane$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
abline(v= c(lanesmin))

plot(data_6thLane$Local_X,data_6thLane$Local_Y,col =as.factor(data_6thLane$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
abline(v= c(lanesmin))
}

interest= full$Vehicle_ID[which(full$Lane_ID == 7 & full$firstLane != 7)]
interest = unique(interest)

carn= sample(interest, 1)
car=full[which(full$Vehicle_ID==carn)]
limits = c(0,95,0,1800)
plot(car$Local_X,car$Local_Y,col =as.factor(car$Lane_ID), xlim = limits[1:2], ylim = limits[3:4])
lines(car$Local_X,car$Local_Y)
abline(v= c(lanesmin))
print(carn)
remove(car,carn,cars)






########
datalist = list()
frames_available = unique(full$Frame_ID)
for (i in 1:length(frames_available)){
  framen= frames_available[i]
  a= subset(full, Frame_ID == framen)
  a$YPre = a$Local_Y[match(a$Preceding,a$Vehicle_ID)]
  a$YFol = a$Local_Y[match(a$Following,a$Vehicle_ID)]
  datalist[[i]] = a
}

big_data <- data.table::rbindlist(datalist)
big_data$d_PRE =  big_data$YPre - big_data$Local_Y
big_data$d_FOLL = big_data$Local_Y - big_data$YFol























########################## plot frames

####### plot cars

car=big_data[which(big_data$Frame_ID==882)]
limits = c(0,95,0,1800)
plot(car$Local_X,car$Local_Y,col =as.factor(car$Vehicle_ID), xlim = limits[1:2], ylim = limits[3:4])
abline(v= c(lanesmin))

car=car[with(car, order(Local_Y)),]
car$precedinglane6 = as.integer(0)
for ( i in car$Vehicle_ID[which(car$Lane_ID == 7)])
 {
  car$precedinglane6[match(i,car$Vehicle_ID)] = car$Vehicle_ID[match(car$Local_Y[match(i,car$Vehicle_ID)-1],car$Local_Y)]
 }




car=big_data[which(big_data$Frame_ID==882)]
car=big_data



carss = list()
frames_available = unique(big_data$Frame_ID)
for (i in 1:length(frames_available)){
  frame= frames_available[i]
  car=big_data[which(big_data$Frame_ID==frame)]
  car=car[with(car, order(Local_Y)),]
  car$precedinglane6 = as.integer(0)
  car$followinglane6 = as.integer(0)
  for ( j in car$Vehicle_ID[which(car$Lane_ID == 7)])
  {
   a = (car$Local_Y < car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == 6)
   car$followinglane6[match(j,car$Vehicle_ID)] = car$Vehicle_ID[max(which(a))]

   b = (car$Local_Y > car$Local_Y[match(j,car$Vehicle_ID)]) & (car$Lane_ID == 6)
   car$precedinglane6[match(j,car$Vehicle_ID)] = car$Vehicle_ID[min(which(b))]
  }

  car$YPre6 = car$Local_Y[match(car$precedinglane6,car$Vehicle_ID)]
  car$YFol6 = car$Local_Y[match(car$followinglane6,car$Vehicle_ID)]
  car$velPre6 = car$v_Vel[match(car$precedinglane6,car$Vehicle_ID)]
  car$velFoll6= car$v_Vel[match(car$followinglane6,car$Vehicle_ID)]


  carss[[i]] = car

}
new_big_data <- data.table::rbindlist(carss)
new_big_data$d_PRE6 =  new_big_data$YPre6 - new_big_data$Local_Y
new_big_data$d_FOLL6 = new_big_data$Local_Y - new_big_data$YFol6





hist(new_big_data$velFoll6)

hist(new_big_data$velPre6)








a = (car$Local_Y < car$Local_Y[match(i,car$Vehicle_ID)]) & (car$Lane_ID = 6)
car$Vehicle_ID[max(which(a))]














data_plot= new_big_data[which(new_big_data$Frame_ID==882 & new_big_data$Local_Y < 800 & new_big_data$Local_X > 60)]

colores = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

basic <- ggplot(data_plot, aes(Local_X, Local_Y, colour = factor(Vehicle_ID), shape = factor(Lane_ID) )) +
       geom_point()  + geom_text(aes(label=Vehicle_ID),hjust=0, vjust=0)
basic

















