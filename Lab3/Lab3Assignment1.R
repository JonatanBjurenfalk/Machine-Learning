# Assignment 1. Kernel methods. 
set.seed(1234567890)
library("geosphere")
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

# These three values are up to the students, the width/smoothing coefficient
h_distance <- 250000
h_date <- 25
h_time <-5
  
pointOfInterest <- c(59.8332051, 17.518366) # The point to predict (up to the students)

date <- "1996-04-03" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00",
           "18:00:00", "20:00:00", "22:00:00", "00:00:00")
temp <- vector(length=length(times))

# Students' code here
# Gaussian kernel
kernel = function(u) {
  return(exp(-(abs(u)^2)))
}

# Distance
testDist <- c()
testDistKernel <- c()
for (i in 1:dim(stations)[1]){
  distance = distHaversine(c(stations[i,]$latitude, stations[i,]$longitude), pointOfInterest, r=6378137)
  stations$kernelDist[i] <- kernel(distance / h_distance)
  
  testDist[i] <- distance
  testDistKernel[i] <- stations$kernelDist[i]
  
}
st = merge(stations,temps,by="station_number")

plot(x = testDist, y = testDistKernel, main = "Smoothing factor distance") # checking smoothing factor for distance

# Excluding all data points after date.
st <- st[!(as.POSIXct(st$date) - as.POSIXct(date) > 0),]

# Date and distance
distanceKernel = c()
dateKernel = c()
allTemp <- c()
dateTest <- c()
for (i in 1:dim(st)[]) {
  
  distanceKernel[i] <- st[i,]$kernelDist
  
  dateDiff <- unclass((as.POSIXct(date) - as.POSIXct(st[i,]$date)))
  dateDiff <- dateDiff %% 365
  if (dateDiff > 365/2) {
    dateDiff <- 365 - dateDiff
  }
  dateKernel[i] <- kernel(dateDiff/h_date)
  dateTest[i] <- dateDiff
  allTemp[i] <- st[i,]$air_temperature
  
}

plot(x = dateTest, y = dateKernel, main = "Smoothing factor date") # checking smoothing factor for date

# Computing temp value for every two hours
timeKernel <- c()
testTime <- c()
tempMult <- c()
for (i in 1:length(temp)) {
  
  # Time
  for (j in 1:dim(st)[1]) {
    
    # Time
    timeDiff <- abs(unclass(as.POSIXct(times[i], format="%H:%M:%S") - as.POSIXct(st[j,]$time, format="%H:%M:%S")))
    if (timeDiff > 12) {
      timeDiff <- 24 - timeDiff
    }
    timeKernel[j] <- kernel(timeDiff/h_time)
    testTime[j] <- timeDiff
    
  }
  
  if (i == 1) {
    plot(x = testTime, y = timeKernel, main = "Smoothing factor time")
  } else {
    points(x = testTime, y = timeKernel)
  }

  # Adding the temp value to the vector
  temp[i] <- sum((distanceKernel + dateKernel + timeKernel)*allTemp)/sum(distanceKernel + dateKernel + timeKernel)
  
  # Multiplying
  tempMult[i] <- sum((distanceKernel*dateKernel*timeKernel)*allTemp)/sum(distanceKernel*dateKernel*timeKernel)
  
  
}

plot(temp, type="o", col = "black", main = "Kernel temp values 04-00", ylim=c(0,6))
points(tempMult, type="o", col = "red")





