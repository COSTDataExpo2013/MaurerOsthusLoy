
## ----setup,echo=F,include=F,eval=T---------------------------------------
library(GGally)
library(reshape2)
library(ggplot2)
library(lubridate)
library(stringr)
library(maps)
library(mapproj)
library(lattice)
library(plyr)
library(maptools)
library(gridExtra)
library(scales)
library(RColorBrewer)

#read in all data
# individual data
dat <- read.csv("Data/AllYearsAllData.csv",header=T)
#daves code called this something different
alldat <- dat
# city and year aggregated
cityyearagg <- read.csv("Data/cityyearagg.csv",header=T)
# city only aggregated
###  cityagg <- read.csv("Data/cityagg.csv",header=T)
# city percent change data
###  cityyearpercha <- read.csv("Data/cityyearpercha.csv",header=T)
# city and year aggregated using survey weights
cityyear<-read.csv("Data/cityYearSvywt.csv")
# maps data read in
library(maps)
library(maptools)
data(us.cities)
akhi <- which (us.cities$country.etc=="AK"| us.cities$country.etc=="HI" |
                 us.cities$country.etc=="ma")
us.cities <- us.cities[-akhi,]
states <- map_data("state")


### Adam: color specification ###
DetColor <- "#66C2A5" # "midnightblue"
MilColor <- "#FC8D62" # "darkred"
StaColor <- "#8DA0CB" # "darkgreen"
BilColor <- "#E78AC3" # "darkmagenta"
othercity <- "gray"
citycolors <- c(othercity,BilColor,DetColor,MilColor,StaColor)
cityimport <- cityyearagg[cityyearagg$colorcode != "All Other Cities", ]

#set up theme for plots
theme_noy1 <-    theme_bw() + theme(axis.line = element_blank(), 
                        axis.text.x = element_blank(), 
                        axis.title.x = element_blank(),
                        legend.position = "none")


# binning function for binned scatterplots
binwsurveywgtsyears <- function(dat, x, y, xbin, ybin, xstart, xend, ystart, yend){

  dat$binnedx <- rep(NA, nrow(dat))
  dat$binnedy <-  rep(NA, nrow(dat))
  # bin the x's
  xcenters <- seq(xstart, xend, xbin)
  xlower <- seq(xstart-.5*xbin, xend-.5*xbin, xbin)
  for (i in 1:length(xcenters)){
    dat$binnedx[which(dat[,which(names(dat)==x)] > xlower[i])] <- xcenters[i]   
  }
  ycenters <- seq(ystart, yend, ybin)
  ylower <- seq(ystart-.5*ybin, yend-.5*ybin, ybin)
  for (i in 1:length(ycenters)){
    dat$binnedy[which(dat[,which(names(dat)==y)] > ylower[i])] <- ycenters[i]   
  }  
  yearwgts <- ddply(dat,.(year),summarise,
                    weightedfill = sum(svywt))
  outdat <-ddply(dat,.(binnedx,binnedy,year),summarise,
                 weightedfill = sum(svywt))
  for(i in 2008:2010){
  outdat$weightedfill[outdat$year == i] <- outdat$weightedfill[outdat$year == i] / yearwgts$weightedfill[yearwgts$year==i]
  }
  return(outdat)
}

#build theme for binned scatterplots
binnedtheme <- theme_bw()+theme(axis.line = element_blank(), legend.position = "none")
lowcolor <- "linen" # "lightcyan"


## ----summariesofdata,warning=FALSE,echo=FALSE----------------------------
cityyeartable<-table(dat$city,dat$year)
minperson<-min(cityyeartable)
maxperson<-max(cityyeartable)
medianperson<-median(cityyeartable)


## ----timeplots, echo=FALSE , warning=FALSE, fig.width=10, fig.height=8, out.width='1\\linewidth', fig.pos='H',fig.align='center',fig.cap="Time plots of city-level index variables between 2008 and 2010."----

#set up data and colors for plotting
cityyearagg$colorcode <- rep("All Other Cities",nrow(cityyearagg))
cityyearagg$colorcode[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")] <-as.character(cityyearagg$city[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])
cityyearagg$linesize <- rep(.4,nrow(cityyearagg))
cityyearagg$linesize[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")] <- .5
cityyearagg$namelabs <- rep(NA,nrow(cityyearagg))
cityyearagg$namelabs[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")] <- as.character(cityyearagg$city[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])
cityyearagg$namelabs[cityyearagg$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")] <- c( NA, "Biloxi, MS", NA,
   NA, "Detroit, MI", NA,
   NA, "Milledgeville, GA", NA,
   NA, "State College, PA", NA)

p1 <- qplot(year,comAttach,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg) +
  theme_noy1  + scale_colour_manual(values = citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010)) + ylab("Community Attachment")

p2 <- qplot(year,economy,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_bw() + scale_colour_manual(values = citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010), labels = c(2008,2009,2010)) + ylab("Economic Outlook") + 
  xlab("Year") +
  theme(legend.position = "none")

p3 <- qplot(year,aesthetic,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_bw() + scale_colour_manual(values = citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010), labels = c(2008,2009,2010)) + ylab("Aesthetics") +
  xlab("Year") +
  theme(legend.position = "none")

p4 <- qplot(year,passion,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_noy1+ scale_colour_manual(values = citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010)) + ylab("Passion")

p5 <- qplot(year,loyalty,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_noy1+ scale_colour_manual(values =citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010)) + ylab("Loyalty")

p6 <- qplot(year,socialCap,geom=c("line","text"),group=city,label=namelabs, 
      colour=colorcode,data=cityyearagg)+ theme_bw()+ theme(legend.position="none")+ 
      scale_colour_manual(values = citycolors) +
      geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
      scale_x_continuous(breaks=c(2008,2009,2010))  + ylab("Social Capital")

p6_2 <- qplot(year,socialCap,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_noy1+ scale_colour_manual(values =citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010)) + ylab("Social Capital")

p7 <- qplot(year,civicInv,geom=c("line","text"),group=city,label=namelabs, colour=colorcode,data=cityyearagg)+
  theme_noy1+ scale_colour_manual(values =citycolors) + 
  geom_line(data=cityyearagg, aes(group=city,label=namelabs, colour=colorcode)) + 
  scale_x_continuous(breaks=c(2008,2009,2010)) + ylab("Civic Involvement")

#plot them all to the save graphics window
grid.arrange(p1,p7,p2,p3,nrow=2)



## ----triangular, echo=FALSE, fig.width=8, fig.height=10, out.width='1\\linewidth', fig.pos='H',fig.align='center',fig.cap="Histograms of the marginal distributions of the community attachment index (CAI) and economic outlook index (EOI) for the four cities in 2008 along with a binned scatterplot of the bivariate distribution. Interestingly, the bivariate distribution has a triangular shape for each city."----

# data sets
scdata <- dat[which(dat$city=="State College, PA"),]
detdata <- dat[which(dat$city=="Detroit, MI"),]
mildata <- dat[which(dat$city=="Milledgeville, GA"),]
bildata <- dat[which(dat$city=="Biloxi, MS"),]


# First, thinking about histograms using survey weights
surveyhistdata <- function(data, variable, year = NULL, bin.width) {
  if(!is.null(year)) data <- subset(data, year == year)

	# Setting up the bins
	xrange <- range(data[,variable], na.rm=TRUE)
	xcenters <- seq(from = xrange[1], to = xrange[2], by=bin.width)
	xlower <- xcenters - .5 * bin.width
	xupper <- xcenters + .5 * bin.width
	
	# Cutting the variable into the bins
	data$bins <- cut(data[,variable], breaks = c(xlower[1], xupper), 
		labels = round(xcenters, 2))
	
	# Working with the weights
	data$denom <- sum(data$svywt)
	outdata <- ddply(data, .(bins), summarise, weights = sum(svywt) / unique(denom))
	
	return(outdata)
}


###------All cities for 2008-------------------------------------------------###


# Data for weighted histograms
sc_cai_hist <- dlply(scdata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
sc_eoi_hist <- dlply(scdata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

det_cai_hist <- dlply(detdata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
det_eoi_hist <- dlply(detdata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

mil_cai_hist <- dlply(mildata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
mil_eoi_hist <- dlply(mildata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

bil_cai_hist <- dlply(bildata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
bil_eoi_hist <- dlply(bildata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

# # data for community attachment histograms
# sc_comattach_hist <- surveyhistdata(scdata, "comAttach", year = 2008, 1/3)
# det_comattach_hist <- surveyhistdata(detdata, "comAttach", year = 2008, 1/3)
# mil_comattach_hist <- surveyhistdata(mildata, "comAttach", year = 2008, 1/3)
# bil_comattach_hist <- surveyhistdata(bildata, "comAttach", year = 2008, 1/3)
# 
# # data for econ. histograms
# sc_econ_hist <- surveyhistdata(scdata, "economy", year = 2008, 1/3)
# det_econ_hist <- surveyhistdata(detdata, "economy", year = 2008, 1/3)
# mil_econ_hist <- surveyhistdata(mildata, "economy", year = 2008, 1/3)
# bil_econ_hist <- surveyhistdata(bildata, "economy", year = 2008, 1/3)


# We have to adjust the width of the bars to make it look like a histogram. 
# Any oppinion on the fill/colour?

# Also, should we use density or relative frequency as our y-label?

## TODO: fix the labels and tick marks

cap1 <- ggplot(sc_cai_hist[[1]], aes(x = bins, y = weights)) +
  geom_bar(stat = 'identity', width = 1, fill = StaColor, colour = I("black")) + 
	xlab("CAI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights,
                bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:5) + 
	theme_bw()


cap2 <- ggplot(det_cai_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = DetColor, colour = I("black")) +
	xlab("CAI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights,
                bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:5) + 
	theme_bw()
	
cap3 <- ggplot(mil_cai_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = MilColor, colour = I("black")) +
	xlab("CAI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights,
                bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:5) + 
	theme_bw()

	
cap4 <- ggplot(bil_cai_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = BilColor, colour = I("black")) +
	xlab("CAI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights,
                bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:5) + 
	theme_bw()

	

econp1 <- ggplot(sc_eoi_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = StaColor, colour = I("black")) + 
	xlab("EOI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights,
                bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:3) + 
	theme_bw()


econp2 <- ggplot(det_eoi_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = DetColor, colour = I("black")) +
	xlab("EOI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights,
                bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:3) + 
	theme_bw()
	
econp3 <- ggplot(mil_eoi_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = MilColor, colour = I("black")) +
	xlab("EOI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights,
                bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:3) + 
	theme_bw()
	
econp4 <- ggplot(bil_eoi_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = BilColor, colour = I("black")) +
	xlab("EOI") + 
	ylab("Rel. Frequency") +
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights,
                bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) +
	scale_x_discrete(breaks = 1:3) + 
	theme_bw()


# row labels
rlab1 <- textGrob("State College")
rlab2 <- textGrob("Detroit")
rlab3 <- textGrob("Milledgeville")
rlab4 <- textGrob("Biloxi")

# binned scatterplots
x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5  
lowcolor <- "#EFF4FF"
BinnedEconSCYears <- binwsurveywgtsyears(subset(scdata, year==2008), 
x, y, xbin, ybin, xstart, xend, ystart, yend)
p2sc <- qplot(binnedx, binnedy, geom="tile", fill=weightedfill,
      data=BinnedEconSCYears) +
  scale_fill_gradient(low=lowcolor, high=StaColor) +
  scale_x_continuous(breaks = 1:3) +
  xlab("EOI") + ylab("CAI") +
#  ggtitle("Binned Scatterplot for State College Residents (using survey weights)") + 
  binnedtheme
  

x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5  
lowcolor <- "#E9FFF9"
BinnedEconDetriotYears <- binwsurveywgtsyears(subset(detdata, year==2008), 
x, y, xbin, ybin, xstart, xend, ystart, yend)
p1det <- qplot(binnedx, binnedy, geom="tile", fill=weightedfill,
      data=BinnedEconDetriotYears) +
  scale_fill_gradient(low=lowcolor, high=DetColor) +
  scale_x_continuous(breaks = 1:3) +
  xlab("EOI") + ylab("CAI") +
#  ggtitle("Binned Scatterplot for Detroit Residents (using survey weights)") + 
  binnedtheme


#Ecomony Binned Plot Millidgeville
x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5  
lowcolor <- "#FFF8F5"
BinnedEconMilledgevilleYears <- binwsurveywgtsyears(subset(mildata, year==2008), 
x, y, xbin, ybin, xstart, xend, ystart, yend)
p1mil <- qplot(binnedx, binnedy, geom="tile", fill=weightedfill,
      data=BinnedEconMilledgevilleYears) +
  scale_fill_gradient(low=lowcolor, high=MilColor) +
  scale_x_continuous(breaks = 1:3) +
  xlab("EOI") + ylab("CAI") +
#  ggtitle("Binned Scatterplot for Milledgeville Residents (using survey weights)") + 
  binnedtheme


lowcolor <- "#FFF3FB"
x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5  
BinnedEconBiloxiYears <- binwsurveywgtsyears(subset(bildata, year==2008), 
x, y, xbin, ybin, xstart, xend, ystart, yend)
p1bil <- qplot(binnedx, binnedy, geom="tile", fill=weightedfill,
      data=BinnedEconBiloxiYears) +
  scale_fill_gradient(low=lowcolor, high=BilColor) +
  scale_x_continuous(breaks = 1:3) +
  xlab("EOI") + ylab("CAI") +
#  ggtitle("Binned Scatterplot of Attachment vs Economy for Biloxi") + 
  binnedtheme 

# this way for arranding the grib works with ggsave...	
gridplots <- arrangeGrob(rlab1, cap1, econp1, p2sc, 
	rlab2, cap2, econp2, p1det, 
	rlab3, cap3, econp3, p1mil,
	rlab4, cap4, econp4, p1bil,
	ncol=4)
	
grid.draw(gridplots)


## ----city_grid_plot, include=FALSE---------------------------------------
city_grid_plot <- function(data, lowcolor, highcolor){
  
  year_dat <- split(data, f = data$year)
	
	cai_hist <- llply(year_dat, surveyhistdata, variable = "comAttach", bin.width = 1/3)
	eoi_hist <- llply(year_dat, surveyhistdata, variable = "economy", bin.width = 1/3)
	
	binned_scatter <- llply(year_dat, binwsurveywgtsyears, x = "economy", y = "comAttach", xbin = 1/3, ybin = 1/3, xstart = 1, xend = 3, ystart = 1, yend = 5)
	
	# histograms
	cap1 <- ggplot(cai_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("CAI") + 
	ylab("Rel. Frequency") +
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) +
  scale_x_discrete(breaks = 1:5) + 
	theme_bw()
	
	cap2 <- ggplot(cai_hist[[2]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("CAI") + 
	ylab("Rel. Frequency") +
    scale_x_discrete(breaks = 1:5) +
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) + 
  theme_bw()
	
	cap3 <- ggplot(cai_hist[[3]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("CAI") + 
	ylab("Rel. Frequency") +
    scale_x_discrete(breaks = 1:5) + 
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) +
  theme_bw()
	
	econp1 <- ggplot(eoi_hist[[1]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("EOI") + 
	ylab("Rel. Frequency") +
    ylim(c(0, max(eoi_hist[[1]]$weights, eoi_hist[[2]]$weights, eoi_hist[[3]]$weights))) +
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()
	
	econp2 <- ggplot(eoi_hist[[2]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("EOI") + 
	ylab("Rel. Frequency") +
    ylim(c(0, max(eoi_hist[[1]]$weights, eoi_hist[[2]]$weights, eoi_hist[[3]]$weights))) +
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()
	
	econp3 <- ggplot(eoi_hist[[3]], aes(x = bins, y = weights)) +
	geom_bar(stat = 'identity', width = 1, fill = highcolor, colour = I("black")) + 
	xlab("EOI") + 
	ylab("Rel. Frequency") +
    ylim(c(0, max(eoi_hist[[1]]$weights, eoi_hist[[2]]$weights, eoi_hist[[3]]$weights))) +
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()
	
	# row labels
	rlab1 <- textGrob("2008")
	rlab2 <- textGrob("2009")
	rlab3 <- textGrob("2010")
	
	# binned scatterplots
	binned1 <- qplot(binnedx, binnedy, geom="tile", fill = weightedfill,
      data = binned_scatter[[1]]) +
      scale_fill_gradient(low=lowcolor, high=highcolor) +
    scale_x_continuous(breaks = 1:3) +
      xlab("EOI") + ylab("CAI") +
      binnedtheme
      
    binned2 <- qplot(binnedx, binnedy, geom="tile", fill = weightedfill,
      data = binned_scatter[[2]]) +
      scale_fill_gradient(low=lowcolor, high=highcolor) +
    scale_x_continuous(breaks = 1:3) +
      xlab("EOI") + ylab("CAI") +
      binnedtheme
      
    binned3 <- qplot(binnedx, binnedy, geom="tile", fill = weightedfill,
      data = binned_scatter[[3]]) +
      scale_fill_gradient(low=lowcolor, high=highcolor) +
    scale_x_continuous(breaks = 1:3) +
      xlab("EOI") + ylab("CAI") +
      binnedtheme
    
    
    gridplots <- arrangeGrob(rlab1, cap1, econp1, binned1, 
      rlab2, cap2, econp2, binned2, 
	  rlab3, cap3, econp3, binned3,
	  ncol=4)
    
    return(gridplots)
}



## ----StateCollegeBinned, echo=FALSE , warning=FALSE, fig.width=8, fig.height=8, out.width='.95\\linewidth', fig.pos='H',fig.align='center',fig.cap="Histograms of the marginal distribution of the community attachment index (CAI) and economic outlook index (EOI) along with a binned scatterplot of the bivariate distribution for State College, by year.", cache=TRUE----
# State College doesn't have a CAI of 5 in the last two years, to we have to add 
# rows with weight 0 to make x-axes conformable
scdata_adj <- data.frame(matrix(NA, nrow = 2, ncol = ncol(scdata)))
colnames(scdata_adj) <- colnames(scdata)
scdata_adj[1:2, "comAttach"] <- 1
scdata_adj[1:2, "city"] <- "State College, PA"
scdata_adj[1:2, "svywt"] <- 0
scdata_adj[1:2, "year"] <- c(2009, 2010)

scdata_adj <- rbind(scdata, scdata_adj)

scgrid <- city_grid_plot(data = scdata_adj, lowcolor = "#EFF4FF", highcolor = StaColor)
grid.draw(scgrid)


## ----DetroitBinned, echo=FALSE , warning=FALSE, fig.width=8, fig.height=8, out.width='.95\\linewidth', fig.pos='H',fig.align='center',fig.cap="Histograms of the marginal distribution of the community attachment index (CAI) and economic outlook index (EOI) along with a binned scatterplot of the bivariate distribution for Detroit, by year.", cache=TRUE----
detgrid <- city_grid_plot(data = detdata, lowcolor = "#E9FFF9", highcolor = DetColor)
grid.draw(detgrid)


## ----MillBinned, echo=FALSE , warning=FALSE, fig.width=8, fig.height=8, out.width='.95\\linewidth', fig.pos='H',fig.align='center',fig.cap="Histograms of the marginal distribution of the community attachment index (CAI) and economic outlook index (EOI) along with a binned scatterplot of the bivariate distribution for Milledgeville, by year.", cache=TRUE----
milgrid <- city_grid_plot(data = mildata, lowcolor = "#FFF8F5", highcolor = MilColor)
grid.draw(milgrid)


## ----BiloxiBinned, echo=FALSE , warning=FALSE, fig.width=8, fig.height=8, out.width='.95\\linewidth', fig.pos='H',fig.align='center',fig.cap="Histograms of the marginal distribution of the community attachment index (CAI) and economic outlook index (EOI) along with a binned scatterplot of the bivariate distribution for Biloxi, by year.", cache=TRUE----
bilgrid <- city_grid_plot(data = bildata, lowcolor = "#FFF3FB", highcolor = BilColor)
grid.draw(bilgrid)


## ----EconomicandCivilInvolvementChanges, echo=FALSE , warning=FALSE, fig.width=8, fig.height=4, out.width='.9\\linewidth', fig.pos='H',fig.align='center', fig.cap = "Scatterplots of year-to-year percent change in the economic outlook index against year-to-year percent change in the civic involvement index for 2008 to 2009 (left) and 2009 to 2010 (right). All 26 communities are included in the plot, but the four cities are plotted using the first letter of their name (S = State College, D = Detroit, B = Biloxi, and M = Milledgeville)."----
#Need to create a data frame that tracks the changes in scores over years
yearcha0809<-ddply(cityyear,.(city),summarise,civicInv0809=100*(civicInv[year==2009]/civicInv[year==2008]-1),
                   economy0809=100*(economy[year==2009]/economy[year==2008]-1),
                   basicServ0809=100*(basicServ[year==2009]/basicServ[year==2008]-1))
yearcha0809$year<-"2008 - 2009"
names(yearcha0809)<-c("city","civicInv","economy","basicServ","year")
yearcha0910<-ddply(cityyear,.(city),summarise,
                   civicInv0910=100*(civicInv[year==2010]/civicInv[year==2009]-1),
                   economy0910=100*(economy[year==2010]/economy[year==2009]-1),
                   basicServ0910=100*(basicServ[year==2010]/basicServ[year==2009]-1))
yearcha0910$year<-"2009 - 2010"
names(yearcha0910)<-names(yearcha0809)

#yearcha is a data frame, each row is a city/change of year. The values for civicInv, economy, and basicServ
#represent their year to year percent change in sentiment.
#For instance, the value for Aberdeen, SD for civicInv for the year 2008 - 2009 is 15.64. That means, from 2008 to 2009,
#Aberdeen, SD views on civic involvement increased by 15.64 percent (or became 15.64 percent more favorable). 
#Thus, greater than 0 represents a more favorable view for the year when compared to the previous year, and
#less than 0 represents a less favorable view for the year when compared to the previous year.
yearcha<-rbind(yearcha0809,yearcha0910)


##Plots
#Concentrating on the 2008 - 2009 graph, we see that all communities have a less favorable view of their economy
#in 2009 compared to 2008. It ranges from 2% (Detroit) to 50% (Wichita). At the same time, we see that all but State
#College and Macon GA have more favorable views on civic involvement. Thus, from 2008 to 2009, we can safely say,
#almost universally, that the country had a worse view on the economy, but a better view on civic involvement in 
#2009, as compared to 2008.
#Concentrating on the 2009 - 2010 graph, we see that all communities (sans Biloxi and Milledgeville) have a more
#favorable view of the economy from 2009 to 2010. Also, most of the communities have a decrease in civic involvement
#during this time, except for Gary, Long Beach, Miami, and Milledgeville. I don't know why these 4 communities
#continued to see an increase in civic involvement, when all other communities see a decrease

# colorscheme<-c("gray","darkmagenta","midnightblue","darkred","darkgreen")
colorscheme<-c("gray","#E78AC3","#66C2A5","#FC8D62","#8DA0CB")
yearcha$colorcode<-"All Other Citites"
yearcha[yearcha$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA"),]$colorcode <-as.character(yearcha$city[yearcha$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])


four_cities <- yearcha[yearcha$colorcode!="All Other Citites",]
four_cities$glyph <- str_sub(four_cities$city, start = 1, end = 1)

qplot(civicInv,economy,data=yearcha[yearcha$colorcode=="All Other Citites",],
      geom="point",xlab="Year-to-Year % Change in Civic Involvement",
      ylab="Year-to-Year % Change in Economic Outlook",colour=I("gray"),
      xlim=c(-30,30),ylim=c(-60,60)) +
    geom_hline(y=0) + geom_vline(x=0) +
    theme_bw() + facet_wrap(~year, ncol = 2) +
    geom_text(data = four_cities, aes(label = glyph, colour = colorcode)) +
    scale_color_manual(values=colorscheme[-1]) +
    theme(legend.position="none")  



## ----FriendsandFamily, echo=FALSE , warning=FALSE, fig.width=6, fig.height=6, out.width='.7\\linewidth', fig.pos='H',fig.align='center', fig.cap="Heat plot of average community attachment scores for the combination of answers to the questions: (1) How much of your family lives in the area? and (2) How many of your close friends live in your community?"----
#Proximity to friends is more important that proximity to family when it comes to community attachment. Regardless of how many family members are close to you, if few friends are close to you your community attachment, on average, is below the mean. If all or nearly all your close friends live near you, regardless of how many family members live near you, your community attachment level is above the mean

goodlevelsfam<-setdiff(levels(alldat$closeFam),c("(DK)","(Refused)"))
goodlevelsfriends<- setdiff(levels(alldat$closeFriends),c("(DK)","(Refused)"))
closefriends1<-subset(alldat,closeFam %in% goodlevelsfam & closeFriends %in% goodlevelsfriends)
closefriends1[closefriends1$closeFam=="Most, OR",]$closeFam<-"Most"
closefriends1[closefriends1$closeFriends=="Most, OR",]$closeFriends<-"Most"
closefriends1$closeFam<-factor(closefriends1$closeFam)
closefriends1$closeFriends<-factor(closefriends1$closeFriends)

closefriends2<-ddply(closefriends1,.(closeFam,closeFriends),summarise,comattach=sum(svywt*comAttach,na.rm=T)/sum(svywt,na.rm=T),count=sum(svywt,na.rm=T))


famorder<-c(1,5,2,4,3)
closefriends2$closeFam<-factor(closefriends2$closeFam,levels=levels(closefriends2$closeFam)[famorder])

friendorder<-c(1,5,2,4,3)
closefriends2$closeFriends<-factor(closefriends2$closeFriends,levels=levels(closefriends2$closeFriends)[friendorder])


## Here is where you can change the colors selected
# cols<-brewer.pal(5,"YlOrBr")
# midpt<-mean(closefriends2$comattach)

qplot(closeFriends,closeFam,geom="tile", fill=comattach, data=closefriends2, xlab="How many of your close friends live in your community?", ylab="How much of your family lives in this area?") +
  scale_fill_gradient("Community\nAttachment", low = "white", high = "gray8") + 
  theme_bw() + 
  theme(legend.position="bottom")



## ----AcceptingCommunities, echo=FALSE , warning=FALSE, fig.width=9, fig.height=3, out.width='\\linewidth', fig.pos='H',fig.align='center', fig.cap="Scatterplot of the community attachment index against the answers to the acceptance questions. All 26 communities are included in the plot, but the four cities are plotted using the first letter of their name (S = State College, D = Detroit, B = Biloxi, and M = Milledgeville)."----

# plot settings
fontsize<-3.5
ymin<-2.6
ymax<-4.1
xmin<-2.5
xmax<-4.5

#Relationship between Acceptance of Gays/Lesbians and Community Attachment
gayaccept <- ddply(alldat,.(city),summarise,ga=mean(as.numeric(as.character(gayAccept)),na.rm=T),comattach=mean(comAttach,na.rm=T))
gayaccept$colorcode <- "All Other Citites"
gayaccept[gayaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA"),]$colorcode <-as.character(gayaccept$city[gayaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])

gayaccept_4cities <- gayaccept[gayaccept$colorcode!="All Other Citites",]
gayaccept_4cities$glyph <- str_sub(gayaccept_4cities$city, 1, 1)

gayplot <- qplot(ga,comattach,data = gayaccept[gayaccept$colorcode=="All Other Citites",], geom="point", color = I("gray"), xlab = "How accepting are you\nof gay and lesbian people?", ylab = "Community Attachment") + 
#   scale_color_manual(values=colorscheme) + 
  theme_bw() + 
  theme(legend.position="") +
  scale_x_continuous(limits=c(xmin,xmax)) + 
  scale_y_continuous(limits=c(ymin,ymax)) + 
  geom_text(data=gayaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 4) + 
  coord_fixed() + 
  ylab(NULL)


#Relationship between Acceptance of Immigrants and Community Attachment
imaccept<-ddply(alldat,.(city),summarise,im=mean(as.numeric(as.character(imAccept)),na.rm=T),comattach=mean(comAttach,na.rm=T))
imaccept$colorcode<-"All Other Citites"
imaccept[imaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA"),]$colorcode <-as.character(imaccept$city[imaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])

imaccept_4cities <- imaccept[imaccept$colorcode!="All Other Citites",]
imaccept_4cities$glyph <- str_sub(imaccept_4cities$city, 1, 1)


immigrantplot <- qplot(im, comattach, data = imaccept[imaccept$colorcode=="All Other Citites",], geom = "point", color = I("gray"), xlab="How accepting are you\nof immigrants?", ylab = "Community Attachment") + 
#   scale_color_manual(values=colorscheme) +
  theme_bw() +
  theme(legend.position="") +
  scale_x_continuous(limits=c(xmin,xmax)) +
  scale_y_continuous(limits=c(ymin,ymax)) +
  geom_text(data = imaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 4) +
  coord_fixed() + 
  ylab(NULL)


#Relationship between Acceptance of Racial/Ethnic Minorities and Community Attachment
raceaccept<-ddply(alldat,.(city),summarise,ra=mean(as.numeric(as.character(raceAccept)),na.rm=T),comattach=mean(comAttach,na.rm=T))
raceaccept$colorcode<-"All Other Citites"
raceaccept[raceaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA"),]$colorcode <-as.character(raceaccept$city[raceaccept$city %in% c("Biloxi, MS","Detroit, MI","Milledgeville, GA","State College, PA")])

raceaccept_4cities <- raceaccept[raceaccept$colorcode!="All Other Citites",]
raceaccept_4cities$glyph <- str_sub(raceaccept_4cities$city, 1, 1)

raceplot <- qplot(ra, comattach, data = raceaccept[raceaccept$colorcode=="All Other Citites",], geom = "point", label = city, color = I("gray"), xlab="How accepting are you\nof racial/ethnic minorities?", ylab="Community Attachment") +
#   scale_color_manual(values=colorscheme)+
  theme_bw()+theme(legend.position="")+
  scale_x_continuous(limits=c(xmin,xmax))+
  scale_y_continuous(limits=c(ymin,ymax))+
  geom_text(data=raceaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 4) +
  coord_fixed() + 
  ylab(NULL)


# combine into one graphic window
grid.arrange(
  arrangeGrob(gayplot, immigrantplot, raceplot, ncol=3, 
              left = textGrob("Community Attachement", rot = 90, vjust = 1)),
  clip=TRUE)


