##---------------------------------------------##
#### Functions for Maurer, Osthus, Loy Paper ####
##---------------------------------------------##


# Cleaning up levels of 5-point scales



# Creating composite scales
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

composite_score <- function(x, wt) {
  x <- rescale01(x)
  x <- x[!is.na(x)]
  
  mean(wt * x)
}

# Computing indices