print("~~~~~~ renewal regression-- McShane results ~~~~~~~~")
fn <- system.file("extdata", "McShane_paperResults.RDS", package = "Countr")
res <- readRDS(fn)

start <- readRDS(system.file("extdata", "start.RDS", package = "Countr"))

y <- res$y
data <- res$data
form <-
    Y ~ GERMAN + EDU + VOC + UNI + CATH + PROT + MUSL + RURAL + YEAR_OF_B + AGEMARR

## add regresiion on the shape parameter
anc <- list(shape = form)
## =========================== gamma =====================================
print("............ gamma ............")
res <- renewal(formula = form, data = data, dist = "gamma",
               computeHessian = FALSE, anc = anc,
               control = renewal.control(trace = 0, start = start)
               )
