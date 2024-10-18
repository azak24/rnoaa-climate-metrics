################################################################################
# This code is the initial data pull from rnoaa
# Takes a very long time to run - only needs to be run once
# I do not recommend running this script - proceed to script 2

# Amanda Zak
# June 2023
################################################################################
library(devtools)
library(rnoaa) # NOTE: will be discontinued. unsure of replacement.

# API Key
options(noaakey = "zKFfJECzNyemgTqscZcTrrRmygzCfysJ")

# Use ghcnd_stations to get station ID list:

# Vector of start dates for each year of data I want
startdate <- NA
for (i in 1:20) {
  if (i < 11) {
    startdate[i] <- paste0(1993+i,"-12-01")
  } else {
    startdate[i] <- paste0(2002+i,"-12-01")
  } 
}

# Vector of end dates for each year of data I want
enddate <- NA
for (i in 1:20) {
  if (i < 11) {
    enddate[i] <- paste0(1994+i,"-03-31")
  } else {
    enddate[i] <- paste0(2003+i,"-03-31")
  }
}

# List of years to reference
year <- c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
# bounding box:
lat1 <- 40.377
lat2 <- 42.51
lon1 <- -74.18
lon2 <- -80.14

# Decade 1:
stations1 <- ghcnd_stations() %>% 
  filter(latitude >= lat1 & latitude <= lat2 & longitude >= lon2 & longitude <= lon1) %>% 
  filter(first_year <= year[1] & last_year >= year[9])
# Decade 2:
stations2 <- ghcnd_stations() %>% 
  filter(latitude >= lat1 & latitude <= lat2 & longitude >= lon2 & longitude <= lon1) %>% 
  filter(first_year <= year[11] & last_year >= year[17])
saveRDS(stations1,"data/stations1.RData")
saveRDS(stations2,"data/stations2.RData")

# Plotting
library(sp)
library(rgeos)
ycoord <- c(40.877,40.877,42.01,42.01,40.877)
xcoord <- c(-79.64,-74.68,-74.68,-79.64,-79.64)
starea <- sp::Line(cbind(xcoord,ycoord))
starea <- sp::Lines(list(starea),ID='A') 
starea <- sp::SpatialLines(list(starea))
stpnt1 <- sp::SpatialPoints(cbind(stations1$longitude,stations1$latitude))
stpnt2 <- sp::SpatialPoints(cbind(stations2$longitude,stations2$latitude))
plot(stpnt1) # decade 1 stations
plot(starea,col="red",add=TRUE) # study area
plot(stpnt2) # decade 2 stations
plot(starea,col="red",add=TRUE) # study area



# Use ghcnd_search to get the data for each station, in a loop

# Decade 1
datalist1 <- list()
start <- Sys.time()
for (i in 1:(nrow(stations1))) {
  start <- 1 # start at year 1
  end <- 10 # end at year 10
  for (j in start:end) { # 10 years total to pull per station
    try({
    temp <- ghcnd_search(stations1$id[i], date_min = startdate[j], date_max = enddate[j], 
             var = c("PRCP","TMIN","TMAX","SNWD","SNOW"))
    datalist1[[length(datalist1)+1]] <- temp
    # Sys.sleep(5) # 5-second delay between iterations necessary for rnoaa
    })
    paste0(round(((i-1)*10+j)/(nrow(stations1)*10),2)*100,"% completed")
  }
}
end <- Sys.time()
print(paste("Start =",start,"  End =",end,"  Elapsed = ",end-start))
saveRDS(datalist1, file = "data/alltypes1.RData") # save as backup

# Decade 2
datalist2 <- list()
start <- Sys.time()
for (i in 1:(nrow(stations2))) {
  start <- 11 # start at year 1
  end <- 20 # end at year 10
  for (j in start:end) { # 10 years total to pull per station
    try({
      temp <- ghcnd_search(stations2$id[i], date_min = startdate[j], date_max = enddate[j], 
                           var = c("PRCP","TMIN","TMAX","SNWD","SNOW"))
      datalist2[[length(datalist2)+1]] <- temp
      # Sys.sleep(5) # 5-second delay between iterations necessary for rnoaa
    })
    paste0(round(((i-1)*10+j)/(nrow(stations2)*10),2)*100,"% completed")
  }
}
end <- Sys.time()
print(paste("Start =",start,"  End =",end,"  Elapsed = ",end-start))
saveRDS(datalist2, file = "data/alltypes2.RData") # save as backup