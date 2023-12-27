input <- list(
  input_ds="trivial.csv",
  
  random_seed=1,
  num_training_point=100,

  basefunction="Polynomial", 
  num_bases=5
)

reactive <- function(f) function() f

################################################################################




################################################################################
########### General functions ##################################################
################################################################################

dat <- data.frame(
  x=1:100
)
dat$y <- rnorm(sin(dat$x/100),0.1)
write.csv(dat, "data/trivial.csv", row.names = FALSE)

