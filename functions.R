##---------------------------------------------##
#### Functions for Maurer, Osthus, Loy Paper ####
##---------------------------------------------##


# Cleaning up levels of 5-point scales



# Rescale to min = 0; max = 1
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
