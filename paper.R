############################################################
# Preliminaries
############################################################
# Remember to make sure you're in the right
# working directory!

library(checkpoint)
checkpoint("2015-08-06")

## Loading necessary libraries
library(GGally)
library(reshape2)
library(ggplot2)
library(lubridate)
library(stringr)
library(maps)
library(mapproj)
library(maptools)
library(lattice)
library(plyr)
library(gridExtra)
library(grid)
library(scales)
library(RColorBrewer)

## Source necessary functions
source("functions.R")

## Load the formatted data.  
## See 'data-munging.R' for the formatting script.

# individual data
dat <- read.csv("data/AllYearsAllData.csv", header = T)
alldat <- dat

# city and year aggregated
cityyearagg <- read.csv("data/cityyearagg.csv", header = T)

# city and year aggregated using survey weights
cityyear <- read.csv("data/cityYearSvywt.csv")

## Load the maps data and subset to continental US
data(us.cities)
akhi <- which(us.cities$country.etc == "AK" | us.cities$country.etc == "HI" | us.cities$country.etc == "ma")
us.cities <- us.cities[-akhi, ]
states <- map_data("state")

## Color specification for plots
DetColor <- "#66C2A5"  # 'midnightblue'
MilColor <- "#FC8D62"  # 'darkred'
StaColor <- "#8DA0CB"  # 'darkgreen'
BilColor <- "#E78AC3"  # 'darkmagenta'
othercity <- "gray"
citycolors <- c(othercity, BilColor, DetColor, MilColor, StaColor)
cityimport <- cityyearagg[cityyearagg$colorcode != "All Other Cities", 
                          ]
lowcolor <- "linen"  # 'lightcyan'

## Creating a theme for plots
theme_noy1 <- theme_bw() + theme(axis.line = element_blank(), axis.text.x = element_blank(), 
                                 axis.title.x = element_blank(), legend.position = "none")

## Creating theme for binned scatterplots
binnedtheme <- theme_bw() + theme(axis.line = element_blank(), legend.position = "none")


############################################################ 
# Creating the plots
############################################################

## Checking number of respondents per city, per year
cityyeartable <- table(dat$city, dat$year)
minperson <- min(cityyeartable)
maxperson <- max(cityyeartable)
medianperson <- median(cityyeartable)


## Setting up data and colors for plotting
cityyearagg$colorcode <- rep("All Other Cities", nrow(cityyearagg))
cityyearagg$colorcode[cityyearagg$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")] <- as.character(cityyearagg$city[cityyearagg$city %in% 
                                                                                                                            c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
cityyearagg$linesize <- rep(I(1), nrow(cityyearagg))
cityyearagg$linesize[cityyearagg$city %in% c("Biloxi, MS", "Detroit, MI", 
                                             "Milledgeville, GA", "State College, PA")] <- I(1.5)

cityyearagg$namelabs <- rep(NA, nrow(cityyearagg))
cityyearagg$namelabs[cityyearagg$city %in% c("Biloxi, MS", "Detroit, MI", 
                                             "Milledgeville, GA", "State College, PA")] <- as.character(cityyearagg$city[cityyearagg$city %in% 
                                                                                                                           c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
cityyearagg$namelabs[cityyearagg$city %in% c("Biloxi, MS", "Detroit, MI", 
                                             "Milledgeville, GA", "State College, PA")] <- c(NA, "Biloxi, MS", NA, 
                                                                                             NA, "Detroit, MI", NA, NA, "Milledgeville, GA", NA, NA, "State College, PA", 
                                                                                             NA)

cityyearagg$linetype <- rep("solid", nrow(cityyearagg))
cityyearagg$linetype[cityyearagg$city == "Biloxi, MS"] <- "dotdash"
cityyearagg$linetype[cityyearagg$city == "Detroit, MI"] <- "dotted"
cityyearagg$linetype[cityyearagg$city == "Milledgeville, GA"] <- "twodash"
cityyearagg$linetype[cityyearagg$city == "State College, PA"] <- "dashed"


# -------------------------------------
# Figure 1
# -------------------------------------

index_names <- function(var, value) {
  value <- as.character(value)
  if(var == "scale") {
    value[value == "comAttach"] <- "Community Attachment"
    value[value == "civicInv"] <- "Civic Involvement"
    value[value == "economy"] <- "Economic Outlook"
    value[value == "aesthetic"] <- "Aesthetics"
  }
  return(value)
}

fig1_df <- melt(cityyearagg, id.vars = c("city", "year", "colorcode", "linetype"), 
                measure.vars = c("comAttach", "civicInv", "economy", "aesthetic"), 
                variable.name = "scale")
fig1_df$scale <- index_names("scale", fig1_df$scale)

ggplot(fig1_df, aes(x = year, y = value, group = city)) +
  geom_line(colour = I("gray")) + 
  geom_line(data = subset(fig1_df, colorcode != "All Other Cities"), 
            aes(group = city, colour = colorcode, linetype = linetype), 
            size = I(1.25)) +
  scale_colour_manual("City", values = citycolors[-1]) +
  scale_linetype_identity() +
  scale_x_continuous(breaks = c(2008, 2009, 2010)) +
  facet_wrap(~ scale, ncol = 2, scales = "free_y") + 
  ylab("Index value") +
  xlab("Year") +
  theme_bw() +
  theme(legend.position="top", legend.text=element_text(size = 12), legend.title=element_text(size = 12)) + 
  guides(colour = guide_legend(override.aes = list(linetype = c("dotdash", "dotted", "twodash", "dashed"), 
                                                   size = .9), keywidth = 3))

# 
# 
# qplot(year, value, data = subset(fig1_df, colorcode == "All Other Cities"), geom = "line", group = city, colour = I("gray")) + 
#   theme_noy1 +
# #   geom_line(data = subset(fig1_df, colorcode != "All Other Cities"), aes(group = city, colour = colorcode, linetype = linetype), size = I(1.25)) + 
#   scale_colour_manual(values = citycolors[-1]) +
#   scale_linetype_identity() +
#   scale_x_continuous(breaks = c(2008, 2009, 2010)) +
#   facet_wrap(. ~ scale, ncol = 2)
# 
# ## Time plot of community attachment index
# p1 <- qplot(year, comAttach, data = subset(cityyearagg, colorcode == "All Other Cities"), geom = "line", group = city, colour = I("gray")) + 
#   theme_noy1 +  
#   geom_line(data = subset(cityyearagg, colorcode != "All Other Cities"), aes(group = city, colour = colorcode, linetype = linetype), size = I(1.25)) + 
#   scale_colour_manual(values = citycolors[-1]) +
#   scale_linetype_identity() +
#   scale_x_continuous(breaks = c(2008, 2009, 2010)) + 
#   ylab("Community Attachment")
# 
# ## Time plot of civic involvement index
# p2 <- qplot(year, civicInv, data = subset(cityyearagg, colorcode == "All Other Cities"), geom = "line", group = city, colour = I("gray")) + 
#   theme_noy1 + 
#   geom_line(data = subset(cityyearagg, colorcode != "All Other Cities"), aes(group = city, colour = colorcode, linetype = linetype), size = I(1.25)) + 
#   scale_colour_manual(values = citycolors[-1]) + 
#   scale_linetype_identity() +
#   scale_x_continuous(breaks = c(2008, 2009, 2010)) + 
#   ylab("Civic Involvement")
# 
# ## Time plot of economic outlook index
# p3 <- qplot(year, economy, data = subset(cityyearagg, colorcode == "All Other Cities"), geom = "line", group = city, colour = I("gray")) + 
#   theme_bw() + 
#   geom_line(data = subset(cityyearagg, colorcode != "All Other Cities"), aes(group = city, colour = colorcode, linetype = linetype), size = I(1.25)) + 
#   scale_colour_manual(values = citycolors[-1]) + 
#   scale_linetype_identity() +
#   scale_x_continuous(breaks = c(2008, 2009, 2010), labels = c(2008, 2009, 2010)) + 
#   ylab("Economic Outlook") + 
#   xlab("Year") + 
#   theme(legend.position = "none")
# 
# ## Time plot of aesthetics index
# p4 <- qplot(year, aesthetic, data = subset(cityyearagg, colorcode == "All Other Cities"), geom = "line", group = city, colour = I("gray")) + 
#   theme_bw() + 
#   geom_line(data = subset(cityyearagg, colorcode != "All Other Cities"), aes(group = city, colour = colorcode, linetype = linetype), size = I(1.25)) + 
#   scale_colour_manual("City", values = citycolors[-1]) + 
#   scale_linetype_identity() +
#   scale_x_continuous(breaks = c(2008, 2009, 2010), labels = c(2008, 2009, 2010)) + 
#   ylab("Aesthetics") + 
#   xlab("Year") + 
#   theme(legend.position="top", legend.text=element_text(size = 12), legend.title=element_text(size = 12)) + 
#   guides(colour = guide_legend(override.aes = list(linetype = c("dotdash", "dotted", "twodash", "dashed"), size = .9), 
#                                keywidth = 3))

## All plots in same graphics window
# legend <- g_legend(p4)
# grid.arrange(legend, arrangeGrob(p1, p2, p3, p4 + theme(legend.position = "none"), nrow = 2), nrow = 2, heights = c(.15, 1.75))


# -------------------------------------
# Figure 2
# -------------------------------------

## Distributions of the community attachment index (CAI) and economic
## outlook index (EOI) for the four cities in 2008 along with a binned
## scatterplots of the bivariate distribution. 

## data sets
scdata <- dat[which(dat$city == "State College, PA"), ]
detdata <- dat[which(dat$city == "Detroit, MI"), ]
mildata <- dat[which(dat$city == "Milledgeville, GA"), ]
bildata <- dat[which(dat$city == "Biloxi, MS"), ]


## All cities for 2008

## State College data
sc_cai_hist <- dlply(scdata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
sc_eoi_hist <- dlply(scdata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

## Detroit data
det_cai_hist <- dlply(detdata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
det_eoi_hist <- dlply(detdata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

## Milledgeville data
mil_cai_hist <- dlply(mildata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
mil_eoi_hist <- dlply(mildata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)

## Biloxi data
bil_cai_hist <- dlply(bildata, .(year), surveyhistdata, variable = "comAttach", bin.width = 1/3)
bil_eoi_hist <- dlply(bildata, .(year), surveyhistdata, variable = "economy", bin.width = 1/3)


## Histograms of the community attachment index
cap1 <- ggplot(sc_cai_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = StaColor, colour = I("black")) + 
  xlab("CAI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights, bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:5) + 
  theme_bw()

# # Alternative specification....
# ggplot(subset(scdata, year == 2008), aes(x = comAttach)) + 
#   geom_histogram(aes(y = (..count..)/sum(..count..), weight = svywt), binwidth = 1/3,  
#                  fill = StaColor, colour = I("black")) + 
# #   facet_grid(year ~ .) +
#   xlab("CAI") + 
#   ylab("Rel. Frequency") +
#   theme_bw()
# 
# 
# ggplot(subset(scdata, year == 2009), aes(x = comAttach)) + 
#   geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1/3,  
#                  fill = StaColor, colour = I("black")) + 
#   #   facet_grid(year ~ .) +
#   xlab("CAI") + 
#   ylab("Rel. Frequency") +
#   theme_bw()
# 
# ggplot(subset(scdata, year == 2010), aes(x = comAttach)) + 
#   geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1/3,  
#                  fill = StaColor, colour = I("black")) + 
#   #   facet_grid(year ~ .) +
#   xlab("CAI") + 
#   ylab("Rel. Frequency") +
#   theme_bw()

  

cap2 <- ggplot(det_cai_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = DetColor, colour = I("black")) + 
  xlab("CAI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights, bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:5) + 
  theme_bw()

cap3 <- ggplot(mil_cai_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = MilColor, colour = I("black")) + 
  xlab("CAI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights, bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:5) + 
  theme_bw()


cap4 <- ggplot(bil_cai_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = BilColor, colour = I("black")) + 
  xlab("CAI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_cai_hist[[1]]$weights, det_cai_hist[[1]]$weights, bil_cai_hist[[1]]$weights, mil_cai_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:5) + 
  theme_bw()


## Histograms of the economic outlook index
econp1 <- ggplot(sc_eoi_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = StaColor, colour = I("black")) + 
  xlab("EOI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights, bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()


econp2 <- ggplot(det_eoi_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = DetColor, colour = I("black")) + 
  xlab("EOI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights, bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()

econp3 <- ggplot(mil_eoi_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = MilColor, colour = I("black")) + 
  xlab("EOI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights, bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()

econp4 <- ggplot(bil_eoi_hist[[1]], aes(x = bins, y = weights)) + 
  geom_bar(stat = "identity", width = 1, fill = BilColor, colour = I("black")) + 
  xlab("EOI") + 
  ylab("Rel. Frequency") + 
  ylim(c(0, max(sc_eoi_hist[[1]]$weights, det_eoi_hist[[1]]$weights, bil_eoi_hist[[1]]$weights, mil_eoi_hist[[1]]$weights))) + 
  scale_x_discrete(breaks = 1:3) + 
  theme_bw()

## row labels for combined plot
rlab1 <- textGrob("State College")
rlab2 <- textGrob("Detroit")
rlab3 <- textGrob("Milledgeville")
rlab4 <- textGrob("Biloxi")


## binned scatterplots
x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5
lowcolor <- "#EFF4FF"
BinnedEconSCYears <- binwsurveywgtsyears(subset(scdata, year == 2008), x, y, xbin, ybin, xstart, xend, ystart, yend)
p2sc <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = BinnedEconSCYears) + 
  scale_fill_gradient(low = lowcolor, high = StaColor) + scale_x_continuous(breaks = 1:3) + 
  xlab("EOI") + ylab("CAI") +
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
BinnedEconDetriotYears <- binwsurveywgtsyears(subset(detdata, year == 2008), x, y, xbin, ybin, xstart, xend, ystart, yend)
p1det <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = BinnedEconDetriotYears) + 
  scale_fill_gradient(low = lowcolor, high = DetColor) + scale_x_continuous(breaks = 1:3) + 
  xlab("EOI") + ylab("CAI") + 
  binnedtheme


x <- "economy"
y <- "comAttach"
xbin <- 1/3
ybin <- 1/3
xstart <- 1
ystart <- 1
xend <- 3
yend <- 5
lowcolor <- "#FFF8F5"
BinnedEconMilledgevilleYears <- binwsurveywgtsyears(subset(mildata, year == 2008), x, y, xbin, ybin, xstart, xend, ystart, yend)
p1mil <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = BinnedEconMilledgevilleYears) + 
  scale_fill_gradient(low = lowcolor, high = MilColor) + scale_x_continuous(breaks = 1:3) + 
  xlab("EOI") + ylab("CAI") + 
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
BinnedEconBiloxiYears <- binwsurveywgtsyears(subset(bildata, year == 2008), x, y, xbin, ybin, xstart, xend, ystart, yend)
p1bil <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = BinnedEconBiloxiYears) + 
  scale_fill_gradient(low = lowcolor, high = BilColor) + scale_x_continuous(breaks = 1:3) + 
  xlab("EOI") + ylab("CAI") + 
  binnedtheme

## Creating the plot matrix
gridplots <- arrangeGrob(rlab1, cap1, econp1, p2sc, rlab2, cap2, econp2, 
                         p1det, rlab3, cap3, econp3, p1mil, rlab4, cap4, econp4, p1bil, ncol = 4)

grid.draw(gridplots)


# -------------------------------------
# Figure 3
# -------------------------------------

## Histograms of the marginal
## distribution of the community attachment index (CAI) and economic
## outlook index (EOI) along with a binned scatterplot of the bivariate
## distribution for State College, by year.
scdata_adj <- data.frame(matrix(NA, nrow = 2, ncol = ncol(scdata)))
colnames(scdata_adj) <- colnames(scdata)
scdata_adj[1:2, "comAttach"] <- 1
scdata_adj[1:2, "city"] <- "State College, PA"
scdata_adj[1:2, "svywt"] <- 0
scdata_adj[1:2, "year"] <- c(2009, 2010)

scdata_adj <- rbind(scdata, scdata_adj)

scgrid <- city_grid_plot(data = scdata_adj, lowcolor = "#EFF4FF", highcolor = StaColor)
grid.draw(scgrid)


# -------------------------------------
# Figure 4
# -------------------------------------

## Histograms of the marginal
## distribution of the community attachment index (CAI) and economic
## outlook index (EOI) along with a binned scatterplot of the bivariate
## distribution for Detroit, by year.
detgrid <- city_grid_plot(data = detdata, lowcolor = "#E9FFF9", highcolor = DetColor)
grid.draw(detgrid)


# -------------------------------------
# Figure 5
# -------------------------------------

## Histograms of the marginal
## distribution of the community attachment index (CAI) and economic
## outlook index (EOI) along with a binned scatterplot of the bivariate
## distribution for Milledgeville, by year.
milgrid <- city_grid_plot(data = mildata, lowcolor = "#FFF8F5", highcolor = MilColor)
grid.draw(milgrid)


# -------------------------------------
# Figure 6
# -------------------------------------

## Histograms of the marginal
## distribution of the community attachment index (CAI) and economic
## outlook index (EOI) along with a binned scatterplot of the bivariate
## distribution for Biloxi, by year.
bilgrid <- city_grid_plot(data = bildata, lowcolor = "#FFF3FB", highcolor = BilColor)
grid.draw(bilgrid)


# -------------------------------------
# Figure 7
# -------------------------------------

## Scatterplots of year-to-year percent change in the economic outlook index
## against year-to-year percent change in the civic involvement index for 2008
## to 2009 and 2009 to 2010. 
yearcha0809 <- ddply(cityyear, .(city), summarise, civicInv0809 = 100 * 
                       (civicInv[year == 2009]/civicInv[year == 2008] - 1), economy0809 = 100 * 
                       (economy[year == 2009]/economy[year == 2008] - 1), basicServ0809 = 100 * 
                       (basicServ[year == 2009]/basicServ[year == 2008] - 1))
yearcha0809$year <- "2008 - 2009"
names(yearcha0809) <- c("city", "civicInv", "economy", "basicServ", "year")
yearcha0910 <- ddply(cityyear, .(city), summarise, civicInv0910 = 100 * 
                       (civicInv[year == 2010]/civicInv[year == 2009] - 1), economy0910 = 100 * 
                       (economy[year == 2010]/economy[year == 2009] - 1), basicServ0910 = 100 * 
                       (basicServ[year == 2010]/basicServ[year == 2009] - 1))
yearcha0910$year <- "2009 - 2010"
names(yearcha0910) <- names(yearcha0809)

# yearcha is a data frame, each row is a city/change of year. The
# values for civicInv, economy, and basicServ represent their year to
# year percent change in sentiment. 
yearcha <- rbind(yearcha0809, yearcha0910)


## Plots Concentrating on the 2008 - 2009
colorscheme <- c("gray", "#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB")
yearcha$colorcode <- "All Other Citites"
yearcha[yearcha$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA"), ]$colorcode <- as.character(yearcha$city[yearcha$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])


four_cities <- yearcha[yearcha$colorcode != "All Other Citites", ]
four_cities$glyph <- str_sub(four_cities$city, start = 1, end = 1)

qplot(civicInv, economy, data = subset(yearcha, colorcode == "All Other Citites"), geom = "point", colour = I("gray")) + 
  xlim(-30, 30) + 
  ylim(-60, 60) +
  xlab("Year-to-Year % Change in Civic Involvement") +
  ylab("Year-to-Year % Change in Economic Outlook") +
  geom_hline(y = 0) + 
  geom_vline(x = 0) + 
  facet_wrap(~year, ncol = 2) + 
  geom_text(data = four_cities, aes(label = glyph, colour = colorcode), size = 7) + 
  scale_color_manual(values = colorscheme[-1]) + 
  theme_bw() + 
  theme(legend.position = "none")


# -------------------------------------
# Figure 8
# -------------------------------------

## Heat map of average
## community attachment scores for the combination of answers to the
## questions: (1) How much of your family lives in the area? and (2) How
## many of your close friends live in your community?

# creating dataset for heat map
goodlevelsfam <- setdiff(levels(alldat$closeFam), c("(DK)", "(Refused)"))
goodlevelsfriends <- setdiff(levels(alldat$closeFriends), c("(DK)", "(Refused)"))
closefriends1 <- subset(alldat, closeFam %in% goodlevelsfam & closeFriends %in% 
                          goodlevelsfriends)
closefriends1[closefriends1$closeFam == "Most, OR", ]$closeFam <- "Most"
closefriends1[closefriends1$closeFriends == "Most, OR", ]$closeFriends <- "Most"
closefriends1$closeFam <- factor(closefriends1$closeFam)
closefriends1$closeFriends <- factor(closefriends1$closeFriends)

closefriends2 <- ddply(closefriends1, .(closeFam, closeFriends), summarise, 
                       comattach = sum(svywt * comAttach, na.rm = T)/sum(svywt, na.rm = T), 
                       count = sum(svywt, na.rm = T))


famorder <- c(1, 5, 2, 4, 3)
closefriends2$closeFam <- factor(closefriends2$closeFam, 
                                 levels = levels(closefriends2$closeFam)[famorder])

friendorder <- c(1, 5, 2, 4, 3)
closefriends2$closeFriends <- factor(closefriends2$closeFriends, 
                                     levels = levels(closefriends2$closeFriends)[friendorder])


## Constructing the heat map - Figure 8
qplot(closeFriends, closeFam, geom = "tile", fill = comattach, data = closefriends2) +
  xlab("How many of your close friends live in your community?") + 
  ylab("How much of your family lives in this area?") + 
  scale_fill_gradient("Community\nAttachment", low = "white", high = "gray8") + 
  theme_bw() + 
  theme(legend.position = "bottom")


# -------------------------------------
# Figure 9
# -------------------------------------

## Scatterplots of the community
## attachment index against the answers to the acceptance questions. All
## 26 communities are included in the plot, but the four cities are
## plotted using the first letter of their name 

# plot settings
fontsize <- 3.5
ymin <- 2.6
ymax <- 4.1
xmin <- 2.5
xmax <- 4.5

## Summarizing acceptance indices for plotting
accept <- melt(alldat, id.vars = c("city"), measure.vars = c("gayAccept", "imAccept", "raceAccept"))

accept <- ddply(alldat, .(city), summarise, 
                ga = mean(as.numeric(as.character(gayAccept)), na.rm = T),
                im = mean(as.numeric(as.character(imAccept)), na.rm = T),
                ra = mean(as.numeric(as.character(raceAccept)), na.rm = T),
                comattach = mean(comAttach, na.rm = T))
accept$colorcode <- "All Other Citites"
accept[accept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA"), ]$colorcode <- as.character(accept$city[accept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
accept_4cities <- accept[accept$colorcode != "All Other Citites", ]
accept_4cities$glyph <- str_sub(accept_4cities$city, 1, 1)

## Relationship between Acceptance of Gays/Lesbians and Community Attachment
gayacceptplot <- qplot(ga, comattach, data = subset(accept, colorcode == "All Other Citites"), 
                       geom = "point", color = I("gray")) +
  xlab("How accepting are you\nof gay and lesbian people?") + 
  ylab(NULL) +
  theme_bw() + 
  theme(legend.position = "") + 
  scale_x_continuous(limits = c(xmin, xmax)) + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  geom_text(data = accept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
  coord_fixed()

## Relationship between Acceptance of Immigrants and Community Attachment
immigrantplot <- qplot(im, comattach, data = subset(accept, colorcode == "All Other Citites"), 
                       geom = "point", color = I("gray")) + 
  xlab("How accepting are you\nof immigrants?") + 
  ylab(NULL) + 
  theme_bw() + 
  theme(legend.position = "") + 
  scale_x_continuous(limits = c(xmin, xmax)) + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  geom_text(data = accept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
  coord_fixed() 

## Relationship between Acceptance of Racial/Ethnic Minorities and Community Attachment
raceplot <- qplot(ra, comattach, data = subset(accept, colorcode == "All Other Citites"), 
                  geom = "point", color = I("gray")) +
  xlab("How accepting are you\nof racial/ethnic minorities?") + 
  ylab(NULL) + 
  theme_bw() + theme(legend.position = "") + 
  scale_x_continuous(limits = c(xmin, xmax)) + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  geom_text(data = accept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
  coord_fixed()

# ## Relationship between Acceptance of Gays/Lesbians and Community Attachment
# # summarizing acceptance index for plotting
# gayaccept <- ddply(alldat, .(city), summarise, 
#                    ga = mean(as.numeric(as.character(gayAccept)), na.rm = T), 
#                    comattach = mean(comAttach, na.rm = T))
# 
# gayaccept$colorcode <- "All Other Citites"
# gayaccept[gayaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA"), ]$colorcode <- as.character(gayaccept$city[gayaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
# 
# gayaccept_4cities <- gayaccept[gayaccept$colorcode != "All Other Citites", ]
# gayaccept_4cities$glyph <- str_sub(gayaccept_4cities$city, 1, 1)
# 
# # code for scatterplot
# gayacceptplot <- qplot(ga, comattach, data = subset(gayaccept, colorcode == "All Other Citites"), geom = "point", color = I("gray")) +
#   xlab("How accepting are you\nof gay and lesbian people?") + 
#   ylab(NULL) +
#   theme_bw() + 
#   theme(legend.position = "") + 
#   scale_x_continuous(limits = c(xmin, xmax)) + 
#   scale_y_continuous(limits = c(ymin, ymax)) + 
#   geom_text(data = gayaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
#   coord_fixed()


# ## Relationship between Acceptance of Immigrants and Community Attachment
# # summarizing acceptance index for plotting
# imaccept <- ddply(alldat, .(city), summarise, im = mean(as.numeric(as.character(imAccept)), na.rm = T), comattach = mean(comAttach, na.rm = T))
# imaccept$colorcode <- "All Other Citites"
# imaccept[imaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA"), ]$colorcode <- as.character(imaccept$city[imaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
# 
# imaccept_4cities <- imaccept[imaccept$colorcode != "All Other Citites", ]
# imaccept_4cities$glyph <- str_sub(imaccept_4cities$city, 1, 1)
# 
# # code for scatterplot
# immigrantplot <- qplot(im, comattach, data = subset(imaccept, colorcode == "All Other Citites"), geom = "point", color = I("gray")) + 
#   xlab("How accepting are you\nof immigrants?") + 
#   ylab(NULL) + 
#   theme_bw() + 
#   theme(legend.position = "") + 
#   scale_x_continuous(limits = c(xmin, xmax)) + 
#   scale_y_continuous(limits = c(ymin, ymax)) + 
#   geom_text(data = imaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
#   coord_fixed() 
# 
# 
# ## Relationship between Acceptance of Racial/Ethnic Minorities and Community Attachment
# # summarizing acceptance index for plotting
# raceaccept <- ddply(alldat, .(city), summarise, ra = mean(as.numeric(as.character(raceAccept)), na.rm = T), comattach = mean(comAttach, na.rm = T))
# raceaccept$colorcode <- "All Other Citites"
# raceaccept[raceaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA"), ]$colorcode <- as.character(raceaccept$city[raceaccept$city %in% c("Biloxi, MS", "Detroit, MI", "Milledgeville, GA", "State College, PA")])
# 
# raceaccept_4cities <- raceaccept[raceaccept$colorcode != "All Other Citites", ]
# raceaccept_4cities$glyph <- str_sub(raceaccept_4cities$city, 1, 1)
# 
# # code for scatterplot
# raceplot <- qplot(ra, comattach, data = subset(raceaccept, colorcode == "All Other Citites"), geom = "point", color = I("gray")) +
#   xlab("How accepting are you\nof racial/ethnic minorities?") + 
#   ylab(NULL) + 
#   theme_bw() + theme(legend.position = "") + 
#   scale_x_continuous(limits = c(xmin, xmax)) + 
#   scale_y_continuous(limits = c(ymin, ymax)) + 
#   geom_text(data = raceaccept_4cities, aes(label = glyph), colour = colorscheme[-1], size = 7) + 
#   coord_fixed()


# Figure 9: scatterplots combined into one graphic window
grid.arrange(arrangeGrob(gayacceptplot, immigrantplot, raceplot, ncol = 3, left = textGrob("Community Attachement", rot = 90, vjust = 1)), clip = TRUE)
