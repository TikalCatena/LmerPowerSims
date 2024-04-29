group_count = 41 #Number of subjects
within_count = 22 #Observations per subject
slopes = seq(.01, .1, by=.01) #generate desired fixed effect slopes to be tested
res = .34 #Set residual variance
randomIntercept = .67 #Set random intercept variance


powerSims <- simCurve(group_count = group_count,
                      within_count = within_count,
                      slopes = slopes,
                      res = res,
                      randomIntercept = randomIntercept,
                      verbose = F, #Set to 'TRUE' to see full information for power calculations
                      nsim = 10)


