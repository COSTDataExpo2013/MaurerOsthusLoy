##---------------------------------------------##
#### Functions for Maurer, Osthus, Loy Paper ####
##---------------------------------------------##

# Rescale to min = 0; max = 1
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}


# Binning function for the binned scatterplots
binwsurveywgtsyears <- function(dat, x, y, xbin, ybin, xstart, xend, ystart, yend) {
  dat$binnedx <- rep(NA, nrow(dat))
  dat$binnedy <- rep(NA, nrow(dat))
  # bin the x's
  xcenters <- seq(xstart, xend, xbin)
  xlower <- seq(xstart - 0.5 * xbin, xend - 0.5 * xbin, xbin)
  for (i in 1:length(xcenters)) {
    dat$binnedx[which(dat[, which(names(dat) == x)] > xlower[i])] <- xcenters[i]
  }
  ycenters <- seq(ystart, yend, ybin)
  ylower <- seq(ystart - 0.5 * ybin, yend - 0.5 * ybin, ybin)
  for (i in 1:length(ycenters)) {
    dat$binnedy[which(dat[, which(names(dat) == y)] > ylower[i])] <- ycenters[i]
  }
  yearwgts <- ddply(dat, .(year), summarise, weightedfill = sum(svywt))
  outdat <- ddply(dat, .(binnedx, binnedy, year), summarise, weightedfill = sum(svywt))
  for (i in 2008:2010) {
    outdat$weightedfill[outdat$year == i] <- outdat$weightedfill[outdat$year == i]/yearwgts$weightedfill[yearwgts$year == i]
  }
  return(outdat)
}

## Binning function for the weighted histograms
surveyhistdata <- function(data, variable, year = NULL, bin.width) {
  if (!is.null(year)) 
    data <- subset(data, year == year)
  
  # Setting up the bins
  xrange <- range(data[, variable], na.rm = TRUE)
  xcenters <- seq(from = xrange[1], to = xrange[2], by = bin.width)
  xlower <- xcenters - 0.5 * bin.width
  xupper <- xcenters + 0.5 * bin.width
  
  # Cutting the variable into the bins
  data$bins <- cut(data[, variable], breaks = c(xlower[1], xupper), 
                   labels = round(xcenters, 2))
  
  # Working with the weights
  data$denom <- sum(data$svywt)
  outdata <- ddply(data, .(bins), summarise, weights = sum(svywt)/unique(denom))
  
  return(outdata)
}

# Function stripping legend from plot
g_legend <- function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Function to create figures 3-6 more easily
city_grid_plot <- function(data, lowcolor, highcolor) {
  
  year_dat <- split(data, f = data$year)
  
  cai_hist <- llply(year_dat, surveyhistdata, variable = "comAttach", bin.width = 1/3)
  eoi_hist <- llply(year_dat, surveyhistdata, variable = "economy", bin.width = 1/3)
  
  binned_scatter <- llply(year_dat, binwsurveywgtsyears, x = "economy", 
                          y = "comAttach", xbin = 1/3, ybin = 1/3, xstart = 1, xend = 3, 
                          ystart = 1, yend = 5)
  
  # histograms
  cap1 <- ggplot(cai_hist[[1]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
    xlab("CAI") + 
    ylab("Rel. Frequency") + 
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) + 
    scale_x_discrete(breaks = 1:5) + 
    theme_bw()
  
  cap2 <- ggplot(cai_hist[[2]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
    xlab("CAI") + 
    ylab("Rel. Frequency") + 
    scale_x_discrete(breaks = 1:5) + 
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) + 
    theme_bw()
  
  cap3 <- ggplot(cai_hist[[3]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
    xlab("CAI") + 
    ylab("Rel. Frequency") + 
    scale_x_discrete(breaks = 1:5) + 
    ylim(c(0, max(cai_hist[[1]]$weights, cai_hist[[2]]$weights, cai_hist[[3]]$weights))) + 
    theme_bw()
  
  econp1 <- ggplot(eoi_hist[[1]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
    xlab("EOI") + 
    ylab("Rel. Frequency") + 
    ylim(c(0, max(eoi_hist[[1]]$weights, eoi_hist[[2]]$weights, eoi_hist[[3]]$weights))) + 
    scale_x_discrete(breaks = 1:3) + 
    theme_bw()
  
  econp2 <- ggplot(eoi_hist[[2]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
    xlab("EOI") + 
    ylab("Rel. Frequency") + 
    ylim(c(0, max(eoi_hist[[1]]$weights, eoi_hist[[2]]$weights, eoi_hist[[3]]$weights))) + 
    scale_x_discrete(breaks = 1:3) + 
    theme_bw()
  
  econp3 <- ggplot(eoi_hist[[3]], aes(x = bins, y = weights)) + 
    geom_bar(stat = "identity", width = 1, fill = highcolor, colour = I("black")) + 
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
  binned1 <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = binned_scatter[[1]]) + 
    scale_fill_gradient(low = lowcolor, high = highcolor) + 
    scale_x_continuous(breaks = 1:3) + 
    xlab("EOI") + 
    ylab("CAI") + 
    binnedtheme
  
  binned2 <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = binned_scatter[[2]]) + 
    scale_fill_gradient(low = lowcolor, high = highcolor) + 
    scale_x_continuous(breaks = 1:3) + 
    xlab("EOI") + 
    ylab("CAI") + 
    binnedtheme
  
  binned3 <- qplot(binnedx, binnedy, geom = "tile", fill = weightedfill, data = binned_scatter[[3]]) + 
    scale_fill_gradient(low = lowcolor, high = highcolor) + 
    scale_x_continuous(breaks = 1:3) + 
    xlab("EOI") + 
    ylab("CAI") + 
    binnedtheme
  
  
  gridplots <- arrangeGrob(rlab1, cap1, econp1, binned1, rlab2, cap2, econp2, binned2, rlab3, cap3, econp3, binned3, ncol = 4)
  
  return(gridplots)
}