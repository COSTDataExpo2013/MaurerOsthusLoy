##-------------------##
#### Preliminaries ####
##-------------------##
# Remember to make sure you're in the right
# working directory!

library(checkpoint)
checkpoint("2015-08-06")

# load libraries
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(mapproj)
library(maps)
library(maptools)

# source functions
source("functions.R")


##-------------------------------------------##
#### CREATE CLEAN DATA ON INDIVIDUAL LEVEL ####
##-------------------------------------------##

# load each year data
dat1 <- read.csv("data/sotc08.csv", header = TRUE)
dat2 <- read.csv("data/sotc09.csv", header = TRUE)
dat3 <- read.csv("data/sotc10.csv", header = TRUE)
varnamesfile <- read.csv("Codebooks/JSMDATAEXPOMATCHINGVARS.csv", header = TRUE)


### fixes needed prior to combining files ###

# QD2A missing from 08 data --> add NA's
dat1$QD2A <- factor(rep(NA, nrow(dat1)))

# 2009 and 2010 need CitySubtypeWord (QS5) converted into a character from a factor
# 2008 needs it converted from an integer so that they will play nice
dat1$QS5 <- as.character(dat1$QS5)
dat2$QS5 <- as.character(dat2$QS5)
dat3$QS5 <- as.character(dat3$QS5)



### combine all files ###
# first pick off variables from each year using columns from varnamesfile as vectors
dat1small <- dat1[ , as.character(varnamesfile$sc08) ]
dat2small <- dat2[ , as.character(varnamesfile$sc09) ]
dat3small <- dat3[ , as.character(varnamesfile$sc10) ]

names(dat1small) <- as.character(varnamesfile$Variable.equivalence)
names(dat2small) <- as.character(varnamesfile$Variable.equivalence)
names(dat3small) <- as.character(varnamesfile$Variable.equivalence)

dat <- rbind(dat1small, dat2small, dat3small)


### Post Combining Data Cleaning ###

# Fix citySubTypeWord 
str(dat1$QS5)
str(dat2$QS5)
str(dat3$QS5)
unique(dat$citySubTypeWord)
for (i in 1:9){
     word <- unique(dat$citySubTypeWord)[i]
     num <- as.character(dat$citySubTypeNum[which(dat$citySubTypeWord==word)[1]])
    dat$citySubTypeWord[which(dat$citySubTypeWord==num)]<- word
}
table(dat[,6:7])

# Several Variables need a 5-point scale cleanup
# comSat
dat$comSat <- revalue(dat$comSat, c("Extremely satisfied" = 5, "Not at all satisfied" = 1))

# proud
dat$proud <- revalue(dat$proud, c("Strongly agree" = 5, "Strongly disagree" = 1))

# comp5YearPast
dat$comp5YearPast <- revalue(dat$comp5YearPast, c("Much better" = 5, "Much worse" = 1))

# comp5YearFuture
dat$comp5YearFuture <- revalue(dat$comp5YearFuture, c("Will be much better" = 5, "Will be much worse" = 1))

# affordHousing, jobAvail, imAccept, raceAccept, gayAccept, econCondNow
vgood_vbad <- subset(dat, select = c(affordHousing, jobAvail, imAccept, raceAccept, gayAccept, econCondNow))
dat <- subset(dat, select = -c(affordHousing, jobAvail, imAccept, raceAccept, gayAccept, econCondNow))

vgood_vbad <- apply(vgood_vbad, 2, revalue, replace = c("Very good" = 5, "Very bad" = 1))
dat <- cbind(dat, vgood_vbad)

dat$affordHousing <- revalue(dat$affordHousing, c("Very good" = 5, "Very bad" = 1))



# econCondFuture
levels(dat$econCondFuture)[3] <- "Same"

# employStatus
levels(dat$employStatus)[1] <- "Disabled/unable to work" 

# incomeSat
dat$incomeSat <- revalue(dat$incomeSat, c("Strongly agree" = 5, "Strongly disagree" = 1))

# crimeNow
dat$crimeNow <- revalue(dat$crimeNow, c("Extremely high" = 1, "Extremely low" = 5))

# closeFriends
dat$closeFriends <- revalue(dat$closeFriends, c("Most, OR" = "Most"))

# closeFam
dat$closeFam <- revalue(dat$closeFam, c("Most, OR" = "Most"))

# maritalStatus
dat$maritalStatus <- revalue(dat$maritalStatus, c("Separated, OR" = "Separated"))

# eduMax
dat$eduMax <- revalue(dat$eduMax, c("College graduate, OR" = "College graduate"))

# Reordering the levels of income
dat$income <- factor(dat$income, 
                     levels = c("Under $15,000", "$15,000 to $24,999", 
                                "$25,000 to $34,999", "$35,000 to $44,999",
                                "$45,000 to $54,999", "$55,000 to $74,999", 
                                "$75,000 to $99,999", "$100,000 or over",
                                "(DK)", "(Refused)") )
  
# # race
# dat$race <- revalue(dat$race, c("Yes, female respondent available" = "female", 
#                                 "Yes, male respondent available" = "male"))
# levels(dat$race)[c(1,3,4,6,7,10,12,13,17,20)] <- c("(NA)", "Hispanic","More than one",
#                                                    "(NA)", "American Indian or Alaskan Native", "Asian", "(DK)",
#                                                    "(DK)", "(NA)", "Some other race")

# gender
dat$gender <- revalue(dat$gender,c("Yes, female respondent available" = "female", 
                                   "Yes, male respondent available" = "male"))                                   


## delete all rows with missing survey weights ##
dat <- dat[-which(is.na(dat$svywt)), ]



##-----------------------------------------------------------------------------##
#### CREATE COMPOSITE SCORE DATA, AGGREGATED BY CITY AND YEAR (USING SVYWTS) ####
##-----------------------------------------------------------------------------##

# Standardizing all composite scores
# 1 is the best possible score. 
# 0 is the worst possible score.
dat <- mutate(dat, passion_rs = rescale01(passion),
              leadership_rs = rescale01(leadership),
              loyalty_rs = rescale01(loyalty),
              basicServ_rs = rescale01(basicServ),
              education_rs = rescale01(education),
              safety_rs = rescale01(safety),
              aesthetic_rs = rescale01(aesthetic),
              economy_rs = rescale01(economy),
              socialOff_rs = rescale01(socialOff),
              civicInv_rs = rescale01(civicInv),
              openness_rs = rescale01(openness),
              socialCap_rs = rescale01(socialCap),
              domains_rs = rescale01(domains),
              comOff_rs = rescale01(comOff),
              comAttach_rs = rescale01(comAttach))

# Aggregating
cityYearSvywt <- ddply(dat, .(city, year), summarise,
                       wt = sum(svywt),
                       people = length(svywt),
                       passion = weighted.mean(passion_rs, svywt, na.rm = TRUE),
                       leadership =  weighted.mean(leadership_rs, svywt, na.rm = TRUE),
                       loyalty = weighted.mean(loyalty_rs, svywt, na.rm = TRUE),
                       basicServ = weighted.mean(basicServ_rs, svywt, na.rm = TRUE),
                       education = weighted.mean(education_rs, svywt, na.rm = TRUE),
                       safety = weighted.mean(safety_rs, svywt, na.rm = TRUE),
                       aesthetic = weighted.mean(aesthetic_rs, svywt, na.rm = TRUE),
                       economy = weighted.mean(economy_rs, svywt, na.rm = TRUE),
                       socialOff = weighted.mean(socialOff_rs, svywt, na.rm = TRUE),
                       civicInv = weighted.mean(civicInv_rs, svywt, na.rm = TRUE),
                       openness = weighted.mean(openness_rs, svywt, na.rm = TRUE),
                       socialCap = weighted.mean(socialCap_rs, svywt, na.rm = TRUE),
                       domains = weighted.mean(domains_rs, svywt, na.rm = TRUE),
                       comOff = weighted.mean(comOff_rs, svywt, na.rm = TRUE),
                       comAttach = weighted.mean(comAttach_rs, svywt, na.rm = TRUE))

head(cityYearSvywt)

##--------------------------------------------------------------------##
#### CREATE COMPOSITE SCORE DATA, AGGREGATED BY CITY (USING SVYWTS) ####
##--------------------------------------------------------------------##

cityagg <- ddply(dat, .(city), summarise,
                 wt = sum(svywt),
                 people = length(svywt),
                 passion = weighted.mean(passion_rs, svywt, na.rm = TRUE),
                 leadership =  weighted.mean(leadership_rs, svywt, na.rm = TRUE),
                 loyalty = weighted.mean(loyalty_rs, svywt, na.rm = TRUE),
                 basicServ = weighted.mean(basicServ_rs, svywt, na.rm = TRUE),
                 education = weighted.mean(education_rs, svywt, na.rm = TRUE),
                 safety = weighted.mean(safety_rs, svywt, na.rm = TRUE),
                 aesthetic = weighted.mean(aesthetic_rs, svywt, na.rm = TRUE),
                 economy = weighted.mean(economy_rs, svywt, na.rm = TRUE),
                 socialOff = weighted.mean(socialOff_rs, svywt, na.rm = TRUE),
                 civicInv = weighted.mean(civicInv_rs, svywt, na.rm = TRUE),
                 openness = weighted.mean(openness_rs, svywt, na.rm = TRUE),
                 socialCap = weighted.mean(socialCap_rs, svywt, na.rm = TRUE),
                 domains = weighted.mean(domains_rs, svywt, na.rm = TRUE),
                 comOff = weighted.mean(comOff_rs, svywt, na.rm = TRUE),
                 comAttach = weighted.mean(comAttach_rs, svywt, na.rm = TRUE))


##-------------------------------------------------##
#### Attach Lat/Long to all aggregated data sets ####
##-------------------------------------------------##

# Maps Data Read in
data(us.cities)
akhi <- which (us.cities$country.etc=="AK"| us.cities$country.etc=="HI" |
                 us.cities$country.etc=="ma")
us.cities <- us.cities[-akhi,]
states <- map_data("state")

a <- unique(dat$city)
cityset <- str_replace(a, ",", "")
b <- us.cities$name[which(us.cities$name %in% cityset)]
locs <- data.frame(city = as.character(c(str_replace(b, ",", ""),setdiff(cityset,intersect(cityset, us.cities$name)))),
                   lat = c(us.cities[ which(us.cities$name %in% cityset) , 4],c(45.4647,33.6889,33.0800,26.7150,44.9444) ),
                   long = c(us.cities[ which(us.cities$name %in% cityset) , 5],c(-98.4861,-78.8869,-83.2322,-80.0536,-93.0931) ))
locs$city <- as.character(locs$city)
for (i in 1:nrow(locs)){
  a <- locs$city[i]
  str_sub(a, -3, -4) <- ","
  locs$city[i] <- a
}

# do the city names line up in our two data sets?
setdiff(unique(locs$city), unique(as.character(cityYearSvywt$city)))
setdiff(unique(as.character(cityYearSvywt$city)),unique(locs$city))
# yes


### Add lat long to each aggregated data set ### 
# reassign city names to a character
cityYearSvywt$city <- as.character(cityYearSvywt$city)
cityagg$city <- as.character(cityagg$city)
cityyearagg <- merge(cityYearSvywt, locs, all.x=TRUE, by="city")
cityagg2 <- merge(cityagg, locs, all.x=TRUE, by="city")


head(cityyearagg)

##-----------------------------------------------------------------------------------##
#### Create City Year Difference Data (units of percent change from previous year) ####
##-----------------------------------------------------------------------------------##

cityyear89 <- ddply(cityyearagg, .(city), summarise,
                    passion = passion[year == 2009] / passion[year == 2008] * 100 - 100,
                    loyalty = loyalty[year == 2009] / loyalty[year == 2008] * 100 - 100,
                    basicServ = basicServ[year == 2009] / basicServ[year == 2008] * 100 - 100,
                    leadership = leadership[year == 2009] / leadership[year == 2008] * 100 - 100,
                    education = education[year == 2009] / education[year == 2008] * 100 - 100,
                    safety = safety[year == 2009] / safety[year == 2008] * 100 - 100,
                    aesthetic = aesthetic[year == 2009] / aesthetic[year == 2008] * 100 - 100,
                    economy = economy[year == 2009] / economy[year == 2008] * 100 - 100,
                    socialOff = socialOff[year == 2009] / socialOff[year == 2008] * 100 - 100,
                    civicInv = civicInv[year == 2009] / civicInv[year == 2008] * 100 - 100,
                    openness= openness[year == 2009] / openness[year == 2008] * 100 - 100,
                    socialCap = socialCap[year == 2009] / socialCap[year == 2008] * 100 - 100,
                    domains = domains[year == 2009] / domains[year == 2008] * 100 - 100,
                    comOff = comOff[year == 2009] / comOff[year == 2008] * 100 - 100,
                    comAttach = comAttach[year == 2009] / comAttach[year == 2008] * 100 - 100,
                    lat = lat[1],
                    long = long[1])

cityyear89$Year <- "2009/2008"
cityyear910 <- ddply(cityyearagg,.(city), summarise,
                     passion=passion[year == 2010] / passion[year == 2009] * 100 - 100,
                     loyalty=loyalty[year == 2010] / loyalty[year == 2009] * 100 - 100,
                     basicServ=basicServ[year == 2010] / basicServ[year == 2009] * 100 - 100,
                     leadership=leadership[year == 2010] / leadership[year == 2009] * 100 - 100,
                     education=education[year == 2010] / education[year == 2009] * 100 - 100,
                     safety=safety[year == 2010] / safety[year == 2009] * 100 - 100,
                     aesthetic=aesthetic[year == 2010] / aesthetic[year == 2009] * 100 - 100,
                     economy=economy[year == 2010] / economy[year == 2009] * 100 - 100,
                     socialOff=socialOff[year == 2010] / socialOff[year == 2009] * 100 - 100,
                     civicInv=civicInv[year == 2010] / civicInv[year == 2009] * 100 - 100,
                     openness=openness[year == 2010] / openness[year == 2009] * 100 - 100,
                     socialCap=socialCap[year == 2010] / socialCap[year == 2009] * 100 - 100,
                     domains=domains[year == 2010] / domains[year == 2009] * 100 - 100,
                     comOff=comOff[year == 2010] / comOff[year == 2009] * 100 - 100,
                     comAttach=comAttach[year == 2010] / comAttach[year == 2009] * 100 - 100,
                     lat=lat[1],
                     long=long[1])
cityyear910$Year <- "2010/2009"

cityyear810 <- ddply(cityyearagg,.(city), summarise,
                     passion=passion[year == 2010] / passion[year == 2008] * 100 - 100,
                     loyalty=loyalty[year == 2010] / loyalty[year == 2008] * 100 - 100,
                     basicServ=basicServ[year == 2010] / basicServ[year == 2008] * 100 - 100,
                     leadership=leadership[year == 2010] / leadership[year == 2008] * 100 - 100,
                     education=education[year == 2010] / education[year == 2008] * 100 - 100,
                     safety=safety[year == 2010] / safety[year == 2008] * 100 - 100,
                     aesthetic=aesthetic[year == 2010] / aesthetic[year == 2008] * 100 - 100,
                     economy=economy[year == 2010] / economy[year == 2008] * 100 - 100,
                     socialOff=socialOff[year == 2010] / socialOff[year == 2008] * 100 - 100,
                     civicInv=civicInv[year == 2010] / civicInv[year == 2008] * 100 - 100,
                     openness=openness[year == 2010] / openness[year == 2008] * 100 - 100,
                     socialCap=socialCap[year == 2010] / socialCap[year == 2008] * 100 - 100,
                     domains=domains[year == 2010] / domains[year == 2008] * 100 - 100,
                     comOff=comOff[year == 2010] / comOff[year == 2008] * 100 - 100,
                     comAttach=comAttach[year == 2010] / comAttach[year == 2008] * 100 - 100,
                     lat=lat[1],
                     long=long[1])
cityyear810$Year <- "2010/2008"

cityyearpercha <- rbind(cityyear89, cityyear910, cityyear810)
cityyearpercha$Year <- factor(cityyearpercha$Year, levels = c("2009/2008", "2010/2009", "2010/2008"))

##-------------------------##
#### Write All CSV Files ####
##-------------------------##

# Uncomment to save files.

# write individual data set
write.csv(dat, "data/AllYearsAllData.csv", row.names=FALSE)

# write aggregated over city
write.csv(cityagg2, "data/cityagg.csv", row.names=FALSE)

# write aggregated over city and year
write.csv(cityyearagg, "data/cityyearagg.csv", row.names=FALSE)

# write percent change over year data
write.csv(cityyearpercha, "data/cityyearpercha.csv", row.names=FALSE)

# write aggregated over city and year w/o lat & long
write.csv(cityYearSvywt, "data/cityYearSvywt.csv", row.names=FALSE)