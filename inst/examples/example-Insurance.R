library(Countr)
library(MASS)
data(Insurance)


## Poisson model
pois <- glm(Claims ~ District + Group + Age + log(Holders),
            data = Insurance, family = poisson)

mat <- model.matrix(pois)
betas <- coef(pois)
lambda <- exp(mat %*% betas)

x <- Insurance$Claims


wei <- renewal(Claims ~ District + Group + Age + log(Holders),
               data = Insurance, dist = "weibull",
               weiMethod = "conv_direct", convPars = list(nsteps = 500,
                                                          convMethod = "direct"),
               computeHessian = TRUE)

print(summary(wei))
gam <- renewal(Claims ~ District + Group + Age + log(Holders),
               data = Insurance, dist = "gamma",
               convPars = list(nsteps = 500, convMethod = "direct"),
               computeHessian = TRUE)

print(summary(gam))
gengam <- renewal(Claims ~ District + Group + Age + log(Holders),
                  data = Insurance, dist = "gengamma",
                  convPars = list(nsteps = 500, convMethod = "direct"),
                  computeHessian = TRUE)

print(summary(gengam))
## compare AICs
AIC <- cbind(pois = AIC(pois),
             weibull = AIC(wei),
             gamma = AIC(gam),
             gengamma = AIC(gengam)
             )

## The series methods as well as the dePril convolution method seem to be off
## for this example.
