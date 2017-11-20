### R code from vignette source 'c:/Az/Rdevel/bitbucket/countr_project/jss_paper/Rnw/Countr_guide_paper.Rnw'

###################################################
### code chunk number 1: Countr_guide_paper.Rnw:37-49
###################################################
## this chunk is always evaluated
op <- options()
## this is from the FAQ for JSS:
##    options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
## below we use width = 77, since I don't want to make last minute change.
options(prompt = "R> ", continue = "+  ", width = 77, useFancyQuotes = FALSE)

options(digits = 3)  # number of digits after decimal point
options(show.signif.stars=FALSE)
## if "result.RData" is loaded, do not resave it.
resultsFile <- "result.RData"
resaveResults <- FALSE


###################################################
### code chunk number 2: Countr_guide_paper.Rnw:52-55 (eval = FALSE)
###################################################
## ## if this chunk is executed, save the results when finished.
## ## (see the end of this file)
## resaveResults <- TRUE


###################################################
### code chunk number 3: Countr_guide_paper.Rnw:58-64
###################################################
if(!resaveResults){
    library("Countr")
    library(lmtest)
    load(resultsFile)
    resaveResults <- FALSE # in case 'resultsFile' contains variable 'resaveResults'
}


###################################################
### code chunk number 4: Countr_guide_paper.Rnw:704-710 (eval = FALSE)
###################################################
## renewalCount(formula, data, subset, na.action, weights, offset,
##   dist = c("custom", "weibull", "weibullgam", "gamma", "gengamma", "burr"),
##   anc = NULL, convPars = NULL, link = NULL, time = 1.0,
##   control = renewal.control(...), customPars = NULL,
##   seriesPars = NULL, weiMethod = NULL,
##   computeHessian = TRUE, model = TRUE, y = TRUE, x = FALSE, ...)


###################################################
### code chunk number 5: Countr_guide_paper.Rnw:742-743
###################################################
library("Countr")


###################################################
### code chunk number 6: load-aux-pkg
###################################################
library(dplyr)
library(xtable)


###################################################
### code chunk number 7: Countr_guide_paper.Rnw:779-781
###################################################
data(football)
table(football$awayTeamGoals)


###################################################
### code chunk number 8: Countr_guide_paper.Rnw:791-796 (eval = FALSE)
###################################################
## away_poiss <- glm(formula = awayTeamGoals ~ 1, family = poisson,
##   data = football)
## away_wei <- renewalCount(formula = awayTeamGoals ~ 1, data = football,
##   dist = "weibull", computeHessian = FALSE,
##   control = renewal.control(trace = 0))


###################################################
### code chunk number 9: Countr_guide_paper.Rnw:802-805
###################################################
breaks_ <- 0:5
pears <- compareToGLM(poisson_model = away_poiss, breaks = breaks_,
  weibull = away_wei)


###################################################
### code chunk number 10: Countr_guide_paper.Rnw:811-815
###################################################
library(dplyr)
frequency_plot(pears$Counts, pears$Actual,
  dplyr::select(pears, contains("_predicted")),
  colours = c("grey", "blue", "green", "black"))


###################################################
### code chunk number 11: Countr_guide_paper.Rnw:844-846
###################################################
lr <- lmtest::lrtest(away_poiss, away_wei)
lr


###################################################
### code chunk number 12: Countr_guide_paper.Rnw:855-858
###################################################
gof_wei <- chiSq_gof(away_wei, breaks = breaks_)
gof_pois <- chiSq_gof(away_poiss, breaks = breaks_)
rbind(Poisson = gof_pois, "Weibull-count" = gof_wei)


###################################################
### code chunk number 13: Countr_guide_paper.Rnw:933-934
###################################################
data("fertility", package = "Countr")


###################################################
### code chunk number 14: head-fertility
###################################################
dataCaption <- "First few rows of fertility data."
print(xtable(head(fertility), caption = dataCaption, label = "tbl:data"),
      rotate.colnames = TRUE)


###################################################
### code chunk number 15: children-table
###################################################
freqtable <-
    count_table(count = fertility$children, breaks = 0:9, formatChar = TRUE)
print(xtable(freqtable, caption = "Fertility data: Frequency distribution of column \\texttt{children}.",
             label = "tbl:freq"))


###################################################
### code chunk number 16: Countr_guide_paper.Rnw:973-976
###################################################
nam_fac <- sapply(fertility, function(x) !is.numeric(x))
fert_factor <- summary(fertility[ , nam_fac])
fert_num <- t(sapply(fertility[ , !nam_fac], summary)) # summary(fertility[ , !nam_fac])


###################################################
### code chunk number 17: covariates-table
###################################################
print(xtable(fert_factor, caption = "Summary of the factor variables", label = "tbl:frecfac"))


###################################################
### code chunk number 18: covariates-table-num
###################################################
print(xtable(fert_num, caption = "Summary of the numeric explanatory variables",
                       label = "tbl:frecnum"))


###################################################
### code chunk number 19: Countr_guide_paper.Rnw:1019-1021
###################################################
regModel <- children ~ german + years_school + voc_train + university +
  Religion + rural + year_birth + age_marriage


###################################################
### code chunk number 20: Countr_guide_paper.Rnw:1036-1037
###################################################
link_weibull <- list(scale = "log", shape = "log")


###################################################
### code chunk number 21: Countr_guide_paper.Rnw:1053-1055 (eval = FALSE)
###################################################
## gamModel <- renewalCount(formula = regModel, data = fertility,
##   dist = "gamma", control = renewal.control(trace = 0) )


###################################################
### code chunk number 22: Countr_guide_paper.Rnw:1107-1108
###################################################
getParNames("weibull")


###################################################
### code chunk number 23: Countr_guide_paper.Rnw:1126-1127
###################################################
renewalNames(regModel, data = fertility, dist = "gamma")


###################################################
### code chunk number 24: Countr_guide_paper.Rnw:1137-1138 (eval = FALSE)
###################################################
## IV <- glm(regModel, family = poisson(), data = fertility)


###################################################
### code chunk number 25: Countr_guide_paper.Rnw:1140-1141
###################################################
coef(IV)


###################################################
### code chunk number 26: Countr_guide_paper.Rnw:1157-1158
###################################################
startW <- renewalCoef(IV, target = "scale")


###################################################
### code chunk number 27: <
###################################################
startW <- c(startW, "shape_" = log(1))
startW


###################################################
### code chunk number 28: Countr_guide_paper.Rnw:1173-1175 (eval = FALSE)
###################################################
## weiModel <- renewalCount(formula = regModel, data = fertility,
##   dist = "weibull", control = renewal.control(trace = 0, start = startW))


###################################################
### code chunk number 29: Countr_guide_paper.Rnw:1189-1192 (eval = FALSE)
###################################################
## weiModelA <- renewalCount(formula = regModel, data = fertility,
##   dist = "weibull",
##   control = renewal.control(trace = 0, method = "L-BFGS-B"))


###################################################
### code chunk number 30: Countr_guide_paper.Rnw:1199-1202 (eval = FALSE)
###################################################
## weiModel_many <- renewalCount(formula = regModel, data = fertility,
##   dist = "weibull", control = renewal.control(trace = 0,
##   method = c("nlminb", "Nelder-Mead", "BFGS")))


###################################################
### code chunk number 31: Countr_guide_paper.Rnw:1209-1210
###################################################
t(weiModel_many$optim)


###################################################
### code chunk number 32: Countr_guide_paper.Rnw:1246-1247
###################################################
anc <- list(sigma = regModel, Q = regModel)


###################################################
### code chunk number 33: Countr_guide_paper.Rnw:1254-1257
###################################################
startA <- renewalCoef(IV, target = "gengamma")
startA[c("Q_", "sigma_")] <- c(1, log(1))
startA


###################################################
### code chunk number 34: Countr_guide_paper.Rnw:1285-1289 (eval = FALSE)
###################################################
## gengamModel_ext0 <- renewalCount(formula = regModel, data = fertility,
##   dist = "gengamma", anc = anc,
##   control = renewal.control(start = startA, trace = 0),
##   computeHessian = FALSE)


###################################################
### code chunk number 35: Countr_guide_paper.Rnw:1297-1302 (eval = FALSE)
###################################################
## startB <- coef(gengamModel_ext0)
## gengamModel_ext <- renewalCount(formula = regModel, data = fertility,
##   dist = "gengamma", anc = anc,
##   control = renewal.control(method = "spg", start = startB, trace = 0),
##   computeHessian = FALSE )


###################################################
### code chunk number 36: Countr_guide_paper.Rnw:1308-1309
###################################################
gengamModel_ext$converged


###################################################
### code chunk number 37: Countr_guide_paper.Rnw:1340-1343
###################################################
parNames <- c("scale", "shape")
sWei <- function(tt, distP) exp( -distP[["scale"]] * tt ^ distP[["shape"]])
link <- list(scale = "log", shape = "log")


###################################################
### code chunk number 38: Countr_guide_paper.Rnw:1360-1361
###################################################
control_custom <- renewal.control(start = startW, trace = 0)


###################################################
### code chunk number 39: Countr_guide_paper.Rnw:1372-1375
###################################################
.getExtrapol <- function(distP) {
    c(2, distP[["shape"]])
}


###################################################
### code chunk number 40: Countr_guide_paper.Rnw:1385-1387
###################################################
customPars <- list(parNames = parNames, survivalFct = sWei,
  extrapolFct = .getExtrapol)


###################################################
### code chunk number 41: Countr_guide_paper.Rnw:1392-1395 (eval = FALSE)
###################################################
## weiModelCust <- renewalCount(formula = regModel, data = fertility,
##   dist = "custom", link = link, control = control_custom,
##   customPars = customPars, computeHessian = FALSE)


###################################################
### code chunk number 42: Countr_guide_paper.Rnw:1448-1449
###################################################
summary(gamModel)


###################################################
### code chunk number 43: Countr_guide_paper.Rnw:1462-1463
###################################################
summary(weiModel)


###################################################
### code chunk number 44: Countr_guide_paper.Rnw:1489-1491 (eval = FALSE)
###################################################
## se_boot <- se.coef(object = weiModel, type =  "boot", R = 5)
## confint_boot <- confint(object = weiModel, type = "boot", R = 5)


###################################################
### code chunk number 45: Countr_guide_paper.Rnw:1589-1590
###################################################
newData <- head(fertility)


###################################################
### code chunk number 46: Countr_guide_paper.Rnw:1592-1596 (eval = FALSE)
###################################################
## predNew.response <- predict(weiModel, newdata = newData, type = "response",
##   se.fit = TRUE)
## predNew.prob <- predict(weiModel, newdata = newData, type = "prob",
##   se.fit = TRUE)


###################################################
### code chunk number 47: Countr_guide_paper.Rnw:1599-1600
###################################################
options(digits = 5)


###################################################
### code chunk number 48: Countr_guide_paper.Rnw:1603-1607
###################################################
predtable <- data.frame(newData$children, predNew.prob$values,
  predNew.response$values)
names(predtable) <- c("Y", "P(Y=y|x)", "E(Y|x)")
predtable


###################################################
### code chunk number 49: Countr_guide_paper.Rnw:1612-1613
###################################################
options(digits = 3)


###################################################
### code chunk number 50: Countr_guide_paper.Rnw:1620-1621
###################################################
cbind(builtIn = coef(weiModel), user = coef(weiModelCust))


###################################################
### code chunk number 51: load-data
###################################################
data(quine, package = "MASS")


###################################################
### code chunk number 52: children-table
###################################################
breaks_ <- c(0, 1, 3, 5:7, 9, 12, 15, 17, 23, 27, 32)
freqtable <-
  count_table(count = quine$Days, breaks = breaks_, formatChar = TRUE)


###################################################
### code chunk number 53: Countr_guide_paper.Rnw:1658-1666
###################################################
##   print(xtable(freqtable,
##   caption = "quine data: Frequency distribution of column \\texttt{Days}.",
##   tabular.environment = "longtable", label = "tbl:freq"))

 print(xtable(freqtable[ , 1:7]), floating = FALSE, only.contents = TRUE)
 cat("\n\\\\[5pt]\n")
 print(xtable(freqtable[ , -(1:7)]), floating = FALSE, only.contents = TRUE)



###################################################
### code chunk number 54: models (eval = FALSE)
###################################################
## quine_form <- as.formula(Days ~ Eth + Sex + Age + Lrn)
## pois <- glm(quine_form, family = poisson(), data = quine)
## nb <- MASS::glm.nb(quine_form, data = quine)
## 
## ## various renewal models
## wei <- renewalCount(formula = quine_form, data = quine, dist = "weibull",
##   computeHessian = FALSE, weiMethod = "conv_dePril",
##   control = renewal.control(trace = 0))
## 
## gam <- renewalCount(formula = quine_form, data = quine, dist = "gamma",
##   computeHessian = FALSE, control = renewal.control(trace = 0))
## 
## gengam <- renewalCount(formula = quine_form, data = quine, dist = "gengamma",
##   computeHessian = FALSE, control = renewal.control(trace = 0))


###################################################
### code chunk number 55: lr-test-poisson
###################################################
library(lmtest)
pois_nb <- lrtest(pois, nb)
pois_wei <- suppressWarnings(lrtest(pois, wei))
pois_gam <- suppressWarnings(lrtest(pois, gam))
pois_gengam <- suppressWarnings(lrtest(pois, gengam))
pois_res <- data.frame("Alternative model" =
  c("negative-binomial", "weibull", "gamma", "generalised-gamma"),
  Chisq = c(pois_nb$Chisq[2], pois_wei$Chisq[2],
            pois_gam$Chisq[2], pois_gengam$Chisq[2]),
  Df = c(1, 1, 1, 2),
  Critical_value = c(rep(qchisq(0.95, 1), 3), qchisq(0.95, 2)),
  stringsAsFactors = FALSE)


###################################################
### code chunk number 56: Countr_guide_paper.Rnw:1743-1745
###################################################
print(xtable(pois_res, caption = "LR results against Poisson model. Each row compares an alternative model vs the Poisson model. All alternatives are preferable to Poisson.",
             label = "tab:lr_pois"))


###################################################
### code chunk number 57: lr-test-renewal
###################################################
gengam_wei <- lrtest(wei, gengam)
gengam_gam <- lrtest(gam, gengam)
gengam_res <- data.frame(Model = c("weibull", "gamma"),
  Chisq = c(gengam_wei$Chisq[2], gengam_gam$Chisq[2]), Df = 1,
  Critical_value = rep(qchisq(0.95, 1), 2), stringsAsFactors = FALSE)


###################################################
### code chunk number 58: Countr_guide_paper.Rnw:1759-1761
###################################################
print(xtable(gengam_res, caption = "LR results against generalised-gamma model",
             label = "tab:lr_gengam"))


###################################################
### code chunk number 59: IC-models
###################################################
ic <- data.frame(Model = c("weibull", "gamma", "negative-binomial"),
  AIC = c(AIC(wei), AIC(gam), AIC(nb)),
  BIC = c(BIC(wei), BIC(gam), BIC(nb)), stringsAsFactors = FALSE)


###################################################
### code chunk number 60: Countr_guide_paper.Rnw:1773-1775
###################################################
print(xtable(ic, caption = "Information criteria results",
             label = "tab:ic_models"))


###################################################
### code chunk number 61: go-f
###################################################
gof <- chiSq_gof(gam, breaks = breaks_)
gof


###################################################
### code chunk number 62: Countr_guide_paper.Rnw:1885-1888
###################################################
options(op) # restore options
if(resaveResults)
    save.image(file = resultsFile)


