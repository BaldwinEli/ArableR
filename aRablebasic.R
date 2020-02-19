install.packages("devtools")
library(devtools)

#install.packages("C:\\Users\\Eli\\HonorsResearch\\aRable_0.1.1.tgz", repos = NULL, type = "source")
library(aRable)

install.packages("readr")
library(readr)

email <- "lestes@clarku.edu"
password <- "rollinghills88"
tenant <- "clark"

device_upslope <- "A000671"
device_midslope <- "A000680"
device_downslope <- "A000667"
devices <- c(device_downslope, device_midslope, device_upslope)
measures <- c("health", "aux_raw", "dsd_raw", "raw", "calibrated", "hourly", "daily")

ArableClient(device=devices[1], measure=measures[7], start="2018-06-09", end = "2018-06-14", email=email, password=password, tenant=tenant)

test <- ArableClient(device=devices[1], measure=measures[6], start="2018-06-", end = "2018-06-10", email=email, password=password, tenant=tenant)
test

NDVI <- function(NIR, red){
  output <- (NIR-red)/(NIR+red)
  return(output)
}

rho <- function(uw, dw){
  output <- uw/dw
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

aRableGCI <- function(deviceNum, dateYMD){
  datFram <- ArableClient(device=devices[deviceNum], measure=measures[6], start="dateYMD", end="2018-12-01", email=email, password=password, tenant=tenant)
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

for (devicex in 1:3){
  yeet <- arableCIrre(devicex, "2018-08-30", "2018-08-31")
  print(yeet)
}

for (devicex in 1:3){
  temp <- ArableClient(device=devices[devicex], measure=measures[7], start="2018-09-06", end = "2018-09-07", email=email, password=password, tenant=tenant)
  print(cbind(temp[1,11], temp[1,7]))
}

initSS <- c(testNDVItoLAI$Height ~ testNDVItoLAI$first, testNDVItoLAI, )

SSndviLAI <- selfStart(testNDVItoLAI$Height ~ testNDVItoLAI$first, )

nls