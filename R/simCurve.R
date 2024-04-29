require(lmerTest)
require(simr)

simCurve <- function(group_count, #Number of individuals
                     within_count, #Number of observations per individual
                     within_between_varying = F,
                     within_between_matrix = NA,
                     slopes = c(), #Fixed effects slopes for which power will be calculated
                     res, #Residual Variance
                     nsim = 1000, #Number of simulations for each power calculation
                     verbose = F,
                     randomIntercept = 0,
                     randomSlope = NA){ #Random intercept variance
  X <- rnorm(group_count * within_count) #Generating random values for predictor variable
  id <- rep(1:group_count, each = within_count) #Generating individual ids
  time <- rep(1:within_count, n = group_count) #Generating timepoints for each observation

  covars <- data.frame(id, time, X = X) #Generating predictor dataset

  powers <- data.frame("EffectSize" = slopes, "Power" = rep(0, length(slopes))) #Generating dataframe to contain calculated effect sizes

  if (is.na(randomSlope)) {
    randomSlope <- 0 #Set random slope variance to 0 by default
  }

  for (i in slopes) {

    fixed <- c(0, i) #Set fixed intercept and slope

    model <- makeLmer(y ~ X + (1|id), fixef = fixed, VarCorr = randomIntercept,  sigma = res,  data = covars) #Create Lmer model for fixed+random effects structure of interest

    sim_X <- powerSim(model, nsim = nsim, test = fixed("X", method = "t"), seed = 1234) #Generate power simulation for model defined above
    if(verbose == T){print(sim_X)} #print power simulation full results if verbose==T
    powers[powers$EffectSize == i,]$Power <- (sim_X$x) / (nsim/100) #Record power observed
  }
  return(powers)
}
