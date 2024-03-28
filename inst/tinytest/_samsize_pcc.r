if (interactive()) library(tinytest)
gamma <- 0.01
effect <- 1
# These two are not exactly the same; see `?samsize_pcc` for details
samsize_pcc(1, 0.1, nfeat=22000, dfeat=1)
MKmisc::ssize.pcc(0.1, 1, nrFeatures = 22000, sigFeatures = 20, verbose = FALSE)
