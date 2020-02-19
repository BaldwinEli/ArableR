install.packages("devtools")
library(devtools)

#install.packages("C:\\Users\\Eli\\HonorsResearch\\aRable_0.1.1.tgz", repos = NULL, type = "source")
library(aRable)

install.packages("readr")
library(readr)

#This is where you enter details of your arable account. These details have been removed from this document as it is public.
email <- "your email here"
password <- "your password here"
tenant <- "your tenant name here"

#name devices
device_upslope <- "A000671"
device_midslope <- "A000680"
device_downslope <- "A000667"
#create lists
devices <- c(device_downslope, device_midslope, device_upslope)
measures <- c("health", "aux_raw", "dsd_raw", "raw", "calibrated", "hourly", "daily")

#Get a specific piece of information froma specific device at a specific time, assign it to a variable, and then view it.
test <- ArableClient(device=devices[1], measure=measures[6], start="2018-06-", end = "2018-06-10", email=email, password=password, tenant=tenant)
test

#convert raw values from the devices into a usable form, reflectance. uw stands for upwelling and dw stands for downwelling.
rho <- function(uw, dw){
  output <- uw/dw
  return(output)
}

#Define a function to calculate the index, then a function to calculate that index from aRable values. This is done for many indices
NDVI <- function(NIR, red){
  output <- (NIR-red)/(NIR+red)
  return(output)
}
aRableNDVI <- function(xuw, xdw, yuw, ydw){
  x <- rho(xuw, xdw)
  y <- rho(yuw, ydw)
  output <- NDVI(x,y)
  return(output)
}

CI <- function(NIR, otherBand) {
  output <- NIR/otherBand - 1
  return(output)
}
aRableGCI <- function(deviceNum, dateYMD, dateYMD2){
  datFram <- ArableClient(device=devices[deviceNum], measure=measures[6], start=dateYMD, end=dateYMD2, email=email, password=password, tenant=tenant)
  x <- rho(datFram[13,17], xdw[13,16])
  y <- rho(datFram[13,9], xdw[13,8])
  output <- CI(x,y)
  return(output)
}

WDRVI <- function(NIR, red){
  output <- (0.2*NIR-red)/(0.2*NIR+red)
}
arableWDRVI <- function(deviceNum, dateYMD, dateYMD2){
  datFram <- ArableClient(device=devices[deviceNum], measure=measures[6], start=dateYMD, end=dateYMD2, email=email, password=password, tenant=tenant)
  x <- rho(datFram[13,17], datFram[13,16])
  y <- rho(datFram[13,13], datFram[13,12])
  output <- WDRVI(x,y)
  return(output)
}

RERndvi <- function(NIR, red, rededge){
  output <- ((NIR-red)/(NIR+red))*sqrt(rededge)
}
arableRERndvi <- function(deviceNum, dateYMD, dateYMD2){
  datFram <- ArableClient(device=devices[deviceNum], measure=measures[6], start=dateYMD, end=dateYMD2, email=email, password=password, tenant=tenant)
  x <- rho(datFram[13,17], datFram[13,16])
  y <- rho(datFram[13,13], datFram[13,12])
  z <- rho(datFram[13,15], datFram[13,14])
  output <- RERndvi(x,y,z)
  return(output)
}

CIrre <- function(NIR, red, rededge){
  output <- ((NIR/(0.5*red+0.5*rededge))-1)
}
arableCIrre <- function(deviceNum, dateYMD, dateYMD2){
  datFram <- ArableClient(device=devices[deviceNum], measure=measures[6], start=dateYMD, end=dateYMD2, email=email, password=password, tenant=tenant)
  x <- rho(datFram[13,17], datFram[13,16])
  y <- rho(datFram[13,13], datFram[13,12])
  z <- rho(datFram[13,15], datFram[13,14])
  output <- CIrre(x,y,z)
  return(output)
}

#get a vegetation index value for each device for a certain date.
for (devicex in 1:3){
  temp <- arableCIrre(devicex, "2018-08-30", "2018-08-31")
  print(temp)
}

#get the NDVI and GCVI values from each device for a date
for (devicex in 1:3){
  temp <- ArableClient(device=devices[devicex], measure=measures[7], start="2018-09-06", end = "2018-09-07", email=email, password=password, tenant=tenant)
  print(cbind(temp[1,11], temp[1,7]))
}

