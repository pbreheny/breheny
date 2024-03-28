if (interactive()) library(tinytest)
gamma <- 0.01
effect <- 1
samsize_pcc(1, 0.1, nfeat=22000, dfeat=20)
MKmisc::ssize.pcc(0.1, 1, nrFeatures = 22000, sigFeatures = 20, verbose = FALSE)
