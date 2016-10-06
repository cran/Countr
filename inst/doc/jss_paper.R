### R code from vignette source 'jss_paper.Rnw'

###################################################
### code chunk number 1: jss_paper.Rnw:26-36
###################################################
options(digits = 3)  # number of digits after decimal point
options(width = 70)

library(Countr)
resultsFile <- "result.RData"

load(resultsFile)
## set `resaveResults' after loading result.RData,
##     in case results.RData contains variable `resaveResults'
resaveResults <- FALSE


###################################################
### code chunk number 2: jss_paper.Rnw:41-42 (eval = FALSE)
###################################################
## resaveResults <- TRUE


###################################################
### code chunk number 3: jss_paper.Rnw:437-438
###################################################
library(Countr)


###################################################
### code chunk number 4: jss_paper.Rnw:445-447
###################################################
data(fertility, package = "Countr")
head(fertility)


###################################################
### code chunk number 5: jss_paper.Rnw:487-494 (eval = FALSE)
###################################################
## renewal(formula, data, subset, na.action, weights, offset,
##         dist = c("custom", "weibull", "weibullgam",
##             "gamma", "gengamma", "burr"),
##         anc = NULL, convPars = NULL, link = NULL, time = 1.0,
##         control = renewal.control(...), customPars = NULL,
##         seriesPars = NULL, weiMethod = NULL,
##         computeHessian = TRUE, model = TRUE, y = TRUE, x = FALSE, ...)


###################################################
### code chunk number 6: jss_paper.Rnw:537-539
###################################################
regModel <- Y ~ GERMAN + EDU + VOC + UNI + CATH + PROT + MUSL +
                         RURAL + YEAR_OF_B + AGEMARR


###################################################
### code chunk number 7: jss_paper.Rnw:555-556
###################################################
link_weibull <- list(scale = "log", shape = "log")


###################################################
### code chunk number 8: jss_paper.Rnw:567-570 (eval = FALSE)
###################################################
## gamModel <- renewal(formula = regModel, data = fertility, dist = "gamma",
##                     control = renewal.control(trace = 0)
##                     )


###################################################
### code chunk number 9: jss_paper.Rnw:575-579 (eval = FALSE)
###################################################
## weiCountA <- renewal(formula = Y ~ 1, data = fertility, dist = "weibull",
##                      weiMethod = "series_acc",
##                      control = renewal.control(trace = 0)
##                      )


###################################################
### code chunk number 10: jss_paper.Rnw:596-597 (eval = FALSE)
###################################################
## IV <- glm(regModel, family = poisson(), data = fertility)


###################################################
### code chunk number 11: jss_paper.Rnw:611-613
###################################################
pars <- getParNames("weibull")
pars


###################################################
### code chunk number 12: jss_paper.Rnw:619-622
###################################################
par1Values <- coef(IV)
names(par1Values) <- paste(pars[1], names(par1Values), sep = "_")
names(par1Values) <- gsub('\\(Intercept\\)', "", names(par1Values))


###################################################
### code chunk number 13: <
###################################################
par2Value <- 1.0
names(par2Value) <- paste(par2, "", sep = "_")

start <- c(par1Values, par2Value)


###################################################
### code chunk number 14: jss_paper.Rnw:636-639 (eval = FALSE)
###################################################
## weiModel <- renewal(formula = regModel, data = fertility, dist = "weibull",
##                     control = renewal.control(trace = 0, start = start)
##                     )


###################################################
### code chunk number 15: jss_paper.Rnw:651-654 (eval = FALSE)
###################################################
## weiModel <- renewal(formula = regModel, data = fertility, dist = "weibull",
##                     control = renewal.control(trace = 0, method = "L-BFGS-B")
##                     )


###################################################
### code chunk number 16: jss_paper.Rnw:683-688 (eval = FALSE)
###################################################
## weiModel <- renewal(formula = regModel, data = fertility, dist = "weibull",
##                     weiMethod = "series_acc",
##                     control = renewal.control(trace = 0),
##                     seriesPars = list(terms = 80, iter = 400, eps = 1e-10)
##                     )


###################################################
### code chunk number 17: jss_paper.Rnw:722-739 (eval = FALSE)
###################################################
## mu <- coef(IV)
## names(mu) <- paste0("mu_", names(mu))
## 
## sigma <- Q <- c(1, rep(0, 10))
## names(sigma) <- gsub("mu_", "sigma_", names(mu))
## names(Q) <- gsub("mu_", "Q_", names(mu))
## 
## start <- c(mu, sigma, Q)
## names(start) <- gsub('\\(Intercept\\)', "", names(start))
## 
## anc <- list(sigma = regModel, Q = regModel)
## 
## gengamModel_ext0 <- renewal(formula = regModel, data = fertility,
##                             dist = "gengamma",
##                             anc = anc,
##                             control = renewal.control(start = start, trace = 0),
##                             computeHessian = FALSE)


###################################################
### code chunk number 18: jss_paper.Rnw:743-750 (eval = FALSE)
###################################################
## start <- coef(gengamModel_ext0)
## gengamModel_ext <- renewal(formula = regModel, data = fertility,
##                             dist = "gengamma",
##                             anc = anc,
##                             control = renewal.control(method = "spg",
##                                                       start = start, trace = 0),
##                             computeHessian = FALSE)


###################################################
### code chunk number 19: jss_paper.Rnw:754-755
###################################################
gengamModel_ext$converged


###################################################
### code chunk number 20: jss_paper.Rnw:782-786
###################################################
parNames <- c("scale", "shape")
sWei <- function(tt, distP)
            exp( -distP[["scale"]] * tt ^ distP[["shape"]])
link <- list(scale = "log", shape = "log")


###################################################
### code chunk number 21: jss_paper.Rnw:794-800
###################################################
pars <- coef(IV)
names(pars) <- gsub('\\(Intercept\\)', "",
                    paste0("scale_", names(pars))
                    )
start <- c(pars, shape_ = 1)
control_custom <- renewal.control(start = start, trace = 0)


###################################################
### code chunk number 22: jss_paper.Rnw:812-815
###################################################
.getExtrapol <- function(distP) {
    c(2, distP[["shape"]])
}


###################################################
### code chunk number 23: jss_paper.Rnw:824-827
###################################################
customPars <- list(parNames = parNames,
                   survivalFct = sWei,
                   extrapolFct = .getExtrapol)


###################################################
### code chunk number 24: jss_paper.Rnw:832-836 (eval = FALSE)
###################################################
## weiModelCust <- renewal(formula = regModel, data = fertility, dist = "custom",
##                         link = link,
##                         control = control_custom, customPars = customPars,
##                         computeHessian = FALSE)


###################################################
### code chunk number 25: jss_paper.Rnw:851-852
###################################################
methods(class = "renewal")


###################################################
### code chunk number 26: jss_paper.Rnw:861-862
###################################################
summary(gamModel)


###################################################
### code chunk number 27: jss_paper.Rnw:874-875
###################################################
summary(weiModel)


###################################################
### code chunk number 28: jss_paper.Rnw:884-886 (eval = FALSE)
###################################################
## se_boot <- se.coef(object = weiModel, type =  "boot", R = 5)
## confint_boot <- confint(object = weiModel, type = "boot", R = 5)


###################################################
### code chunk number 29: jss_paper.Rnw:896-897
###################################################
poissModel <- IV


###################################################
### code chunk number 30: jss_paper.Rnw:899-920
###################################################
mat <-
    cbind(logLik = c(logLik(poissModel),
                     logLik(gamModel),
                     logLik(weiModel),
                     logLik(gengamModel_ext)),
          nPars = c(length(coef(poissModel)),
                    length(coef(gamModel)),
                    length(coef(weiModel)),
                    length(coef(gengamModel_ext))),
          AIC = c(AIC(poissModel),
                  AIC(gamModel),
                  AIC(weiModel),
                  AIC(gengamModel_ext)),
          BIC = c(BIC(poissModel),
                  BIC(gamModel),
                  BIC(weiModel),
                  BIC(gengamModel_ext))
          )

rownames(mat) <- c("Pois", "gam", "wei", "gengam_ext")
print(mat)


###################################################
### code chunk number 31: jss_paper.Rnw:942-943
###################################################
newData <- head(fertility)


###################################################
### code chunk number 32: jss_paper.Rnw:945-949 (eval = FALSE)
###################################################
## predNew.response <- predict(weiModel, newdata = newData,
##                             type = "response", se.fit = TRUE)
## predNew.prob <- predict(weiModel, newdata = newData,
##                         type = "prob", se.fit = TRUE)


###################################################
### code chunk number 33: jss_paper.Rnw:952-953
###################################################
options(digits = 5)


###################################################
### code chunk number 34: jss_paper.Rnw:956-960
###################################################
predtable <- data.frame(newData$Y, predNew.prob$values,
                                   predNew.response$values)
names(predtable) <- c("Y", "P(Y=y|x)", "E(Y|x)")
predtable


###################################################
### code chunk number 35: jss_paper.Rnw:965-966
###################################################
options(digits = 3)


###################################################
### code chunk number 36: jss_paper.Rnw:972-973
###################################################
cbind(builtIn = coef(weiModel), user = coef(weiModelCust))


###################################################
### code chunk number 37: jss_paper.Rnw:1058-1060
###################################################
if(resaveResults)
    save.image(file = resultsFile)


