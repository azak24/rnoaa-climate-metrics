################################################################################
# Organize the weather station data pulled in rnoaa-datapull and calculate
# desired winter metrics
# Amanda Zak
# June 2023
################################################################################
library(devtools)
library(tidyverse)
library(dplyr)


# Organize data

# Decade 1
dec1dat <- readRDS("data/alltypes1.RData")
# combine sublists
for (i in 1:length(dec1dat)) {
  dec1dat[[i]] <- bind_rows(dec1dat[[i]])
}
# combine list
dec1dat <- bind_rows(dec1dat)
# redorder rows
dec1dat <- dec1dat[,c(1,3,2,7,8,9,10,4,5,6)]
# pivot longer
dec1dat <- pivot_longer(dec1dat,cols=c(3:7),names_to="type",values_to="value")
# get rid of NA rows
dec1dat <- na.omit(dec1dat)
# make date column a date object
dec1dat$date <- as_datetime(dec1dat$date)
# add year column
dec1dat$year <- dec1dat$date + days(31)
dec1dat$year <- year(dec1dat$year)

# Decade 2
dec2dat <- readRDS("data/alltypes2.RData")
# combine sublists
for (i in 1:length(dec2dat)) {
  dec2dat[[i]] <- bind_rows(dec2dat[[i]])
}
# combine list
dec2dat <- bind_rows(dec2dat)
# redorder rows
dec2dat <- dec2dat[,c(1,3,2,7,8,9,10,4,5,6)]
# pivot longer
dec2dat <- pivot_longer(dec2dat,cols=c(3:7),names_to="type",values_to="value")
# get rid of NA rows
dec2dat <- na.omit(dec2dat)
# make date column a date object
dec2dat$date <- as_datetime(dec2dat$date)
# add year column
dec2dat$year <- dec2dat$date + days(31)
dec2dat$year <- year(dec2dat$year)

# save as file
saveRDS(dec1dat,"data/dec1dat.RData")
saveRDS(dec2dat,"data/dec2dat.RData")

# Extract data types into individual data frames
# Remove duplicate rows
PRCP1 <- distinct(subset(dec1dat,dec1dat$type == "prcp"))
PRCP2 <- distinct(subset(dec2dat,dec2dat$type == "prcp"))
TMAX1 <- distinct(subset(dec1dat,dec1dat$type == "tmax"))
TMAX2 <- distinct(subset(dec2dat,dec2dat$type == "tmax"))
TMIN1 <- distinct(subset(dec1dat,dec1dat$type == "tmin"))
TMIN2 <- distinct(subset(dec2dat,dec2dat$type == "tmin"))
SNOW1 <- distinct(subset(dec1dat,dec1dat$type == "snow"))
SNOW2 <- distinct(subset(dec2dat,dec2dat$type == "snow"))
SNWD1 <- distinct(subset(dec1dat,dec1dat$type == "snwd"))
SNWD2 <- distinct(subset(dec2dat,dec2dat$type == "snwd"))

# save progress
sets <- c("PRCP1","PRCP2","TMAX1","TMAX2","TMIN1","TMIN2","SNOW1","SNOW2","SNWD1","SNWD2")
for (set in 1:length(sets)) {
  eval(parse(text=paste0("saveRDS(",sets[set],", file = 'data/",sets[set],".RData')")))
  
}


### remove the problem years
# these are years where snowfall and snow depth do not match

# summarize total snowfalls and max snow depths
sf1 <- SNOW1 %>% group_by(id, year, date) %>%
  summarise(n = max(value)) %>%
  group_by(id, year) %>% 
  summarise(n = sum(n))
sd1 <- SNWD1 %>%
  group_by(id, year, date) %>% 
  summarise(n = max(value)) %>%
  group_by(id, year) %>% 
  summarise(n = max(n))
# join
sfd <- full_join(sf1,sd1,by=c("id","year"))
colnames(sfd) <- c("id","year","tot.snow","max.snwd")
# find stations/years where SF is > 0 and max SD is 0, or vice versa
remove <- sfd[which((sfd$tot.snow == 0 & sfd$max.snwd > 0)|(sfd$max.snwd == 0 & sfd$tot.snow > 0)),]
# missing years will be removed from their respective data set
removeSNOW <- remove[which(remove$max.snwd > 0 & remove$tot.snow == 0),]
removeSNWD <- remove[which(remove$tot.snow > 0 & remove$max.snwd == 0),]
# remove these station/years
sums <- 0
for (i in 1:nrow(removeSNOW)) {
  station <- removeSNOW$id[i]
  yr <- removeSNOW$year[i]
  sums <- sums + length(which(SNOW1$year == yr & SNOW1$id == station))
  SNOW1 <- SNOW1[which(!(SNOW1$id == station & SNOW1$year == yr)),]
}
sums <- 0
for (i in 1:nrow(removeSNWD)) {
  station <- removeSNWD$id[i]
  year <- removeSNWD$year[i]
  sums <- sums + length(which(SNWD1$year == yr & SNWD1$id == station))
  SNWD1 <- SNWD1[which(!(SNWD1$id == station & SNWD1$year == yr)),]
}

# save this point, rewriting the SNOW1 and SNWD1 files
saveRDS(SNOW1,"SNOW1.RData")
saveRDS(SNWD1,"SNWD1.RData")


# Only keep data that meets the constraints:
  # at least 90% of days in each December–March period
  # at least 7 of the 10 winter seasons

dy <- 31+31+28+31 # number of days from Dec 1 to March 31 for non-leap-year
dyl <- 31+31+29+31 # number of days for leap year
leap <- c(1996,2000,2004,2016,2020) # leap years
sets <- c("PRCP1","PRCP2","TMAX1","TMAX2","TMIN1","TMIN2","SNOW1","SNOW2","SNWD1","SNWD2")
for (set in 1:length(sets)) {
  eval(parse(text=paste0("temp <- ",sets[set]," %>% group_by(id, year) %>% 
           summarise(n = length(unique(date))) %>% # count number of days
           mutate(prop = n/(ifelse(year %in% leap,dyl,dy))) %>%
           subset(prop >= 0.9)")))
  eval(parse(text=paste0("temp2 <- temp %>% group_by(id) %>%
    summarise(n = n()) %>%
    mutate(prop = n/10) %>%
    subset(prop >= 0.7)")))
  eval(parse(text=paste0(sets[set],"stations <- unique(temp2$id)")))
  # subset data set to keep just data from those stations
  eval(parse(text=paste0(sets[set],"dat <- subset(",sets[set],",",sets[set],"$id %in% ",sets[set],"stations)")))
  # save as a file
  eval(parse(text=paste0("saveRDS(",sets[set],"dat, file = 'data/",sets[set],"dat.RData')")))
}


# Check for irregularities in data

sets <- c("PRCP1","PRCP2","TMAX1","TMAX2","TMIN1","TMIN2","SNOW1","SNOW2","SNWD1","SNWD2")
plotlist <- list()
for (set in 1:length(sets)) {
  # flag <- unique(eval(parse(text=paste0(sets[set],"$fl_q"))))
  # print(c(sets[set],flag))
  plot <- ggplot(eval(parse(text=paste0(sets[set],"dat"))),aes(x = date, y = value, color = id))+
    geom_point(show.legend = FALSE) +
    facet_wrap(~year,ncol=4,scales="free") +
    ggtitle(sets[set])
  plotlist[[set]] <- plot
}
# PRCP
plotlist[[1]]
plotlist[[2]]
# one very high value in Jan 2001, but not sure if it's an error
subset(PRCP1dat,PRCP1dat$qflag == "X")
subset(PRCP2dat,PRCP2dat$qflag == "X")
# it's not flagged, and SNOW data seems to confirm it
# will keep

# TMAX
plotlist[[3]]
plotlist[[4]]
# fine
subset(TMAX1dat,TMAX1dat$qflag == "X")
subset(TMAX2dat,TMAX2dat$qflag == "X")

# TMIN
plotlist[[5]]
plotlist[[6]]
# fine
subset(TMIN1dat,TMIN1dat$qflag == "X")
subset(TMIN2dat,TMIN2dat$qflag == "X")

# SNOW
plotlist[[7]]
plotlist[[8]]
# fine
subset(SNOW1dat,SNOW1dat$qflag == "X")
subset(SNOW2dat,SNOW2dat$qflag == "X")

# SNWD
plotlist[[9]]
plotlist[[10]]
subset(SNWD1dat,SNWD1dat$qflag == "X")
subset(SNWD2dat,SNWD2dat$qflag == "X")
# Remove 2018 & 2019 outliers (X-flagged)
SNWD2dat <- subset(SNWD2dat,SNWD2dat$qflag != "X")
# update file
saveRDS(SNWD2dat,"SNWD2dat.RData")


# number of stations remaining
length(unique(PRCP1dat$id)) # 147
length(unique(PRCP2dat$id)) # 213
length(unique(TMAX1dat$id)) # 83
length(unique(TMAX2dat$id)) # 117
length(unique(TMIN1dat$id)) # 84
length(unique(TMIN2dat$id)) # 116
length(unique(SNOW1dat$id)) # 128
length(unique(SNOW2dat$id)) # 120
length(unique(SNWD1dat$id)) # 89
length(unique(SNWD2dat$id)) # 103

# save progress
sets <- c("PRCP1","PRCP2","TMAX1","TMAX2","TMIN1","TMIN2","SNOW1","SNOW2","SNWD1","SNWD2")
for (set in 1:length(sets)) {
  eval(parse(text=paste0("saveRDS(",sets[set],"dat, file = '",sets[set],"dat.RData')")))
  
}

# Calculate desired statistics

# days below freezing
bf1 <- TMAX1dat %>%
  group_by(id, year) %>% 
  summarize(n = length(which(value < 0))) %>% 
  group_by(id) %>% 
  summarize(n = mean(n))

bf2 <- TMAX2dat %>%
  group_by(id, year) %>% 
  summarize(n = length(which(value < 0))) %>% 
  group_by(id) %>% 
  summarize(n = mean(n))

# sum of minimum temperature (degree days)
dd1 <- TMAX1dat %>% mutate(value = value/10) %>%  # convert to degrees celsius
  subset(value <= 0) %>%  # keep only days below 0
  group_by(id, year) %>% 
  summarise(n = sum(value)) %>% # then sum all days in a year
  group_by(id) %>% 
  summarise(dd = mean(n)) # then take the mean of all years

dd2 <- TMAX2dat %>% mutate(value = value/10) %>% # convert to degrees celsius
  subset(value <= 0) %>%  # keep only days below 0
  group_by(id, year) %>% 
  summarise(n = sum(value)) %>% 
  group_by(id) %>% 
  summarise(dd = mean(n))

# number of days with snow on the ground (snow days)
sd1 <- SNWD1dat %>% subset(SNWD1dat$value > 0) %>%  # keep all measurements > 0
  group_by(id, year) %>% 
  summarise(n = n()) %>% # count how many days in a year there is snow
  group_by(id) %>% 
  summarise(sd = mean(n)) # then average # of days w/ snow across the decade

sd2 <- SNWD2dat %>% subset(SNWD2dat$value > 0) %>%  # keep all measurements > 0
  group_by(id, year) %>% 
  summarise(n = n()) %>% # count how many days in a year there is snow
  group_by(id) %>% 
  summarise(sd = mean(n)) # then average # of days w/ snow across the decade

# total snowfall (snowfall) for December–March
sf1 <- SNOW1dat %>% mutate(value = value/10) %>% # convert to cm
  group_by(id, year) %>% 
  summarise(n = sum(value)) %>% # then sum the snowfall across each year
  group_by(id) %>% 
  summarise(sf = mean(n)) # then take the average for each decade

sf2 <- SNOW2dat %>% mutate(value = value/10) %>% # convert to cm
  group_by(id, year) %>% 
  summarise(n = sum(value)) %>% # then sum the snowfall across each year
  group_by(id) %>% 
  summarise(sf = mean(n)) # then take the average for each decade

# max and min temps across study area in recent decade
tmax2 <- TMAX2dat %>% mutate(value = value/10) %>% # convert to degrees celsius
  group_by(id, year, date) %>% 
  summarise(n = max(value)) %>% # cut down to one measurement per day
  group_by(id, year) %>% 
  summarise(n = mean(n)) %>% # mean temp per station per year
  group_by(id) %>% 
  summarise(temp = mean(n)) # mean temp per station over the decade

tmin2 <- TMIN2dat %>% mutate(value = value/10) %>% # convert to degrees celsius
  group_by(id, year, date) %>% 
  summarise(n = min(value)) %>% # cut down to one measurement per day
  group_by(id, year) %>% 
  summarise(n = mean(n)) %>% # mean temp per station per year
  group_by(id) %>% 
  summarise(temp = mean(n)) # mean temp per station over the decade

# precip across study area in recent decade
precip <- PRCP2dat %>% mutate(value = value/100) %>% # convert to cm
  group_by(id, year, date) %>% 
  summarise(n = max(value)) %>% # cut down to one measurement per day
  group_by(id, year) %>% 
  summarise(n = sum(n)) %>% # total precip per station per year
  group_by(id) %>% 
  summarise(prcp = mean(n)) # mean precip per station over the decade

# join to station coordinate data
stations1 <- readRDS("data/stations1.RData")
stations2 <- readRDS("data/stations2.RData")
sets <- c("bf2","dd2","sd2","sf2","tmax2","tmin2","precip")
for (set in 1:length(sets)) {
  eval(parse(text = paste0(sets[set],"sum <- inner_join(",sets[set],",stations2,multiple='first')")))
}
sets <- c("bf1","dd1","sf1","sd1")
for (set in 1:length(sets)) {
  eval(parse(text = paste0(sets[set],"sum <- inner_join(",sets[set],",stations1,multiple='first')")))
}

# export for gis use
sets <- c("bf2sum","dd2sum","sd2sum","sf2sum","tmax2sum","tmin2sum","precipsum")
for (set in 1:length(sets)) {
  write.csv(eval(parse(text=paste0(sets[set]))),paste0("output/",sets[set],".csv"),row.names=FALSE)
}
sets <- c("bf1sum","dd1sum","sd1sum","sf1sum")
for (set in 1:length(sets)) {
  write.csv(eval(parse(text=paste0(sets[set]))),paste0("output/",sets[set],".csv"),row.names=FALSE)
}


### Looking at changes from decade 1 to decade 2

hist(bf1$n)
hist(bf2$n)
mean(bf1$n)
mean(bf2$n)
# shows cooling

hist(dd1$dd)
hist(dd2$dd)
mean(dd1$dd)
mean(dd2$dd)
# shows cooling

hist(sd1$sd)
hist(sd2$sd)
mean(sd1$sd)
mean(sd2$sd)
# shows warming

hist(sf1$sf)
hist(sf2$sf)
mean(sf1$sf)
mean(sf2$sf)
# shows warming



stations1 <- readRDS("stations1.RData")
stations2 <- readRDS("stations2.RData")
sets <- c("dd2")
for (set in 1:length(sets)) {
  eval(parse(text = paste0(sets[set],"sum <- inner_join(",sets[set],",stations2,multiple='first')")))
}
sets <- c("dd1")
for (set in 1:length(sets)) {
  eval(parse(text = paste0(sets[set],"sum <- inner_join(",sets[set],",stations1,multiple='first')")))
}
sets <- c("dd2sum","dd1sum")
for (set in 1:length(sets)) {
  write.csv(eval(parse(text=paste0(sets[set]))),paste0(sets[set],".csv"),row.names=FALSE)
}

