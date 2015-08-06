##-------------------##
#### Preliminaries ####
##-------------------##
# Remember to make sure you're in the right
# working directory!

# load libraries
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(mapproj)
library(maps)
library(maptools)


##-------------------------------------------##
#### CREATE CLEAN DATA ON INDIVIDUAL LEVEL ####
##-------------------------------------------##

# load each year data
dat1 <- read.csv("data/sotc08.csv", header=TRUE)
dat2 <- read.csv("data/sotc09.csv", header=TRUE)
dat3 <- read.csv("data/sotc10.csv", header=TRUE)
varnamesfile <- read.csv("Codebooks/JSMDATAEXPOMATCHINGVARS.csv", header=TRUE)


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

#Several Variables need a 5 point scale cleanup
# comSat
levels(dat$comSat)[7] <- "5"
levels(dat$comSat)[8] <- "1"
# proud
levels(dat$proud)[7] <- "5"
levels(dat$proud)[8] <- "1"
# comp5YearPast
levels(dat$comp5YearPast)[7] <- "5"
levels(dat$comp5YearPast)[8] <- "1"
# comp5YearFuture
levels(dat$comp5YearFuture)[6] <- "5"
levels(dat$comp5YearFuture)[7] <- "1"
# affordHousing
levels(dat$affordHousing)[8] <- "5"
levels(dat$affordHousing)[7] <- "1"
# jobAvail
levels(dat$jobAvail)[8] <- "5"
levels(dat$jobAvail)[7] <- "1"
# imAccept
levels(dat$imAccept)[7] <- "5"
levels(dat$imAccept)[6] <- "1"
# raceAccept
levels(dat$raceAccept)[7] <- "5"
levels(dat$raceAccept)[6] <- "1"
# gayAccept
levels(dat$gayAccept)[7] <- "5"
levels(dat$gayAccept)[6] <- "1"
# econCondNow
levels(dat$econCondNow)[7] <- "5"
levels(dat$econCondNow)[6] <- "1"
# econCondFuture
levels(dat$econCondFuture)[3] <- "Same"
# employStatus
levels(dat$employStatus)[1] <- "Disabled/unable to work" 
# incomeSat
levels(dat$incomeSat)[6] <- "5"
levels(dat$incomeSat)[7] <- "1" 
# crimeNow
levels(dat$crimeNow)[6] <- "5"
levels(dat$crimeNow)[7] <- "1"
# crime
levels(dat$crimeNow)[6] <- "5"
levels(dat$crimeNow)[7] <- "1"
# closeFriends
levels(dat$closeFriends)[7] <- "Most"
# closeFam
levels(dat$closeFam)[7] <- "Most"
# maritalStatus
levels(dat$maritalStatus)[8] <- "Separated"
# eduMax
levels(dat$eduMax)[4] <- "College graduate"
# income
dat$income <- factor(dat$income, levels=levels(dat$income)[c(10,2,3,4,5,6,7,1,8,9)])
# race
levels(dat$race)[c(1,3,4,6,7,10,12,13,17,20)] <- c("(NA)", "Hispanic","More than one",
                                                   "(NA)", "American Indian or Alaskan Native", "Asian", "(DK)",
                                                   "(DK)", "(NA)", "Some other race")
# gender
levels(dat$gender)[1] <- "female"
levels(dat$gender)[2] <- "male"                                     


## delete all rows with missing survey weights ##
dat <- dat[-which(is.na(dat$svywt)),]



##-----------------------------------------------------------------------------##
#### CREATE COMPOSITE SCORE DATA, AGGREGATED BY CITY AND YEAR (USING SVYWTS) ####
##-----------------------------------------------------------------------------##
comp<-dat[,c("city","year","svywt","passion","loyalty",
             "basicServ","leadership","education","safety",
             "aesthetic","economy","socialOff","civicInv",
             "openness","socialCap","domains","comOff","comAttach")]

# Calculate this to standardize all composite scores
# 1 is the best possible score. 
# 0 is the worst possible score.

passionn <- min(comp$passion,na.rm=T)
passionx <- max(comp$passion,na.rm=T)-passionn

leadershipn <- min(comp$leadership,na.rm=T)
leadershipx <- max(comp$leadership,na.rm=T)-leadershipn

loyaltyn <- min(comp$loyalty,na.rm=T)
loyaltyx <- max(comp$loyalty,na.rm=T)-loyaltyn

basicServn <- min(comp$basicServ,na.rm=T)
basicServx <- max(comp$basicServ,na.rm=T)-basicServn

educationn <- min(comp$education,na.rm=T)
educationx <- max(comp$education,na.rm=T)-educationn

safetyn <- min(comp$safety,na.rm=T)
safetyx <- max(comp$safety,na.rm=T)-safetyn

aestheticn <- min(comp$aesthetic,na.rm=T)
aestheticx <- max(comp$aesthetic,na.rm=T)-aestheticn

economyn <- min(comp$economy,na.rm=T)
economyx <- max(comp$economy,na.rm=T)-economyn

socialOffn <- min(comp$socialOff,na.rm=T)
socialOffx <- max(comp$socialOff,na.rm=T)-socialOffn

civicInvn <- min(comp$civicInv,na.rm=T)
civicInvx <- max(comp$civicInv,na.rm=T)-civicInvn

opennessn <- min(comp$openness,na.rm=T)
opennessx <- max(comp$openness,na.rm=T)-opennessn

socialCapn <- min(comp$socialCap,na.rm=T)
socialCapx <- max(comp$socialCap,na.rm=T)-socialCapn

domainsn <- min(comp$domains,na.rm=T)
domainsx <- max(comp$domains,na.rm=T)-domainsn

comOffn <- min(comp$comOff,na.rm=T)
comOffx <- max(comp$comOff,na.rm=T)-comOffn

comAttachn <- min(comp$comAttach,na.rm=T)
comAttachx <- max(comp$comAttach,na.rm=T)-comAttachn

cityYearSvywt <- ddply(dat,.(city,year),summarise,
                       wt=sum(svywt),
                       people=length(svywt),
                       passion=sum(svywt*((passion-passionn)/passionx),na.rm=T)/sum(svywt[!is.na(passion)]),
                       leadership=sum(svywt*((leadership-leadershipn)/leadershipx),na.rm=T)/sum(svywt[!is.na(leadership)]),
                       loyalty=sum(svywt*((loyalty-loyaltyn)/loyaltyx),na.rm=T)/sum(svywt[!is.na(loyalty)]),
                       basicServ=sum(svywt*((basicServ-basicServn)/basicServx),na.rm=T)/sum(svywt[!is.na(basicServ)]),
                       education=sum(svywt*((education-educationn)/educationx),na.rm=T)/sum(svywt[!is.na(education)]),
                       safety=sum(svywt*((safety-safetyn)/safetyx),na.rm=T)/sum(svywt[!is.na(safety)]),
                       aesthetic=sum(svywt*((aesthetic-aestheticn)/aestheticx),na.rm=T)/sum(svywt[!is.na(aesthetic)]),
                       economy=sum(svywt*((economy-economyn)/economyx),na.rm=T)/sum(svywt[!is.na(economy)]),
                       socialOff=sum(svywt*((socialOff-socialOffn)/socialOffx),na.rm=T)/sum(svywt[!is.na(socialOff)]),
                       civicInv=sum(svywt*((civicInv-civicInvn)/civicInvx),na.rm=T)/sum(svywt[!is.na(civicInv)]),
                       openness=sum(svywt*((openness-opennessn)/opennessx),na.rm=T)/sum(svywt[!is.na(openness)]),
                       socialCap=sum(svywt*((socialCap-socialCapn)/socialCapx),na.rm=T)/sum(svywt[!is.na(socialCap)]),
                       domains=sum(svywt*((domains-domainsn)/domainsx),na.rm=T)/sum(svywt[!is.na(domains)]),
                       comOff=sum(svywt*((comOff-comOffn)/comOffx),na.rm=T)/sum(svywt[!is.na(comOff)]),
                       comAttach=sum(svywt*((comAttach-comAttachn)/comAttachx),na.rm=T)/sum(svywt[!is.na(comAttach)]))

head(cityYearSvywt)


##--------------------------------------------------------------------##
#### CREATE COMPOSITE SCORE DATA, AGGREGATED BY CITY (USING SVYWTS) ####
##--------------------------------------------------------------------##

cityagg <- ddply(dat,.(city),summarise,
                 wt=sum(svywt),
                 people=length(svywt),
                 passion=sum(svywt*((passion-passionn)/passionx),na.rm=T)/sum(svywt[!is.na(passion)]),
                 leadership=sum(svywt*((leadership-leadershipn)/leadershipx),na.rm=T)/sum(svywt[!is.na(leadership)]),
                 loyalty=sum(svywt*((loyalty-loyaltyn)/loyaltyx),na.rm=T)/sum(svywt[!is.na(loyalty)]),
                 basicServ=sum(svywt*((basicServ-basicServn)/basicServx),na.rm=T)/sum(svywt[!is.na(basicServ)]),
                 education=sum(svywt*((education-educationn)/educationx),na.rm=T)/sum(svywt[!is.na(education)]),
                 safety=sum(svywt*((safety-safetyn)/safetyx),na.rm=T)/sum(svywt[!is.na(safety)]),
                 aesthetic=sum(svywt*((aesthetic-aestheticn)/aestheticx),na.rm=T)/sum(svywt[!is.na(aesthetic)]),
                 economy=sum(svywt*((economy-economyn)/economyx),na.rm=T)/sum(svywt[!is.na(economy)]),
                 socialOff=sum(svywt*((socialOff-socialOffn)/socialOffx),na.rm=T)/sum(svywt[!is.na(socialOff)]),
                 civicInv=sum(svywt*((civicInv-civicInvn)/civicInvx),na.rm=T)/sum(svywt[!is.na(civicInv)]),
                 openness=sum(svywt*((openness-opennessn)/opennessx),na.rm=T)/sum(svywt[!is.na(openness)]),
                 socialCap=sum(svywt*((socialCap-socialCapn)/socialCapx),na.rm=T)/sum(svywt[!is.na(socialCap)]),
                 domains=sum(svywt*((domains-domainsn)/domainsx),na.rm=T)/sum(svywt[!is.na(domains)]),
                 comOff=sum(svywt*((comOff-comOffn)/comOffx),na.rm=T)/sum(svywt[!is.na(comOff)]),
                 comAttach=sum(svywt*((comAttach-comAttachn)/comAttachx),na.rm=T)/sum(svywt[!is.na(comAttach)]))



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

cityyear89 <- ddply(cityyearagg,.(city), summarise,
                    passion=passion[year==2009]/passion[year==2008]*100-100,
                    loyalty=loyalty[year==2009]/loyalty[year==2008]*100-100,
                    basicServ=basicServ[year==2009]/basicServ[year==2008]*100-100,
                    leadership=leadership[year==2009]/leadership[year==2008]*100-100,
                    education=education[year==2009]/education[year==2008]*100-100,
                    safety=safety[year==2009]/safety[year==2008]*100-100,
                    aesthetic=aesthetic[year==2009]/aesthetic[year==2008]*100-100,
                    economy=economy[year==2009]/economy[year==2008]*100-100,
                    socialOff=socialOff[year==2009]/socialOff[year==2008]*100-100,
                    civicInv=civicInv[year==2009]/civicInv[year==2008]*100-100,
                    openness=openness[year==2009]/openness[year==2008]*100-100,
                    socialCap=socialCap[year==2009]/socialCap[year==2008]*100-100,
                    domains=domains[year==2009]/domains[year==2008]*100-100,
                    comOff=comOff[year==2009]/comOff[year==2008]*100-100,
                    comAttach=comAttach[year==2009]/comAttach[year==2008]*100-100,
                    lat=lat[1],
                    long=long[1])

cityyear89$Year <- "2009/2008"
cityyear910 <- ddply(cityyearagg,.(city), summarise,
                     passion=passion[year==2010]/passion[year==2009]*100-100,
                     loyalty=loyalty[year==2010]/loyalty[year==2009]*100-100,
                     basicServ=basicServ[year==2010]/basicServ[year==2009]*100-100,
                     leadership=leadership[year==2010]/leadership[year==2009]*100-100,
                     education=education[year==2010]/education[year==2009]*100-100,
                     safety=safety[year==2010]/safety[year==2009]*100-100,
                     aesthetic=aesthetic[year==2010]/aesthetic[year==2009]*100-100,
                     economy=economy[year==2010]/economy[year==2009]*100-100,
                     socialOff=socialOff[year==2010]/socialOff[year==2009]*100-100,
                     civicInv=civicInv[year==2010]/civicInv[year==2009]*100-100,
                     openness=openness[year==2010]/openness[year==2009]*100-100,
                     socialCap=socialCap[year==2010]/socialCap[year==2009]*100-100,
                     domains=domains[year==2010]/domains[year==2009]*100-100,
                     comOff=comOff[year==2010]/comOff[year==2009]*100-100,
                     comAttach=comAttach[year==2010]/comAttach[year==2009]*100-100,
                     lat=lat[1],
                     long=long[1])
cityyear910$Year <- "2010/2009"

cityyear810 <- ddply(cityyearagg,.(city), summarise,
                     passion=passion[year==2010]/passion[year==2008]*100-100,
                     loyalty=loyalty[year==2010]/loyalty[year==2008]*100-100,
                     basicServ=basicServ[year==2010]/basicServ[year==2008]*100-100,
                     leadership=leadership[year==2010]/leadership[year==2008]*100-100,
                     education=education[year==2010]/education[year==2008]*100-100,
                     safety=safety[year==2010]/safety[year==2008]*100-100,
                     aesthetic=aesthetic[year==2010]/aesthetic[year==2008]*100-100,
                     economy=economy[year==2010]/economy[year==2008]*100-100,
                     socialOff=socialOff[year==2010]/socialOff[year==2008]*100-100,
                     civicInv=civicInv[year==2010]/civicInv[year==2008]*100-100,
                     openness=openness[year==2010]/openness[year==2008]*100-100,
                     socialCap=socialCap[year==2010]/socialCap[year==2008]*100-100,
                     domains=domains[year==2010]/domains[year==2008]*100-100,
                     comOff=comOff[year==2010]/comOff[year==2008]*100-100,
                     comAttach=comAttach[year==2010]/comAttach[year==2008]*100-100,
                     lat=lat[1],
                     long=long[1])
cityyear810$Year <- "2010/2008"

cityyearpercha <- rbind(cityyear89,cityyear910,cityyear810)
cityyearpercha$Year <- factor(cityyearpercha$Year,levels=c("2009/2008","2010/2009","2010/2008"))

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

write.csv(cityYearSvywt, "data/cityYearSvywt.csv", row.names=FALSE)