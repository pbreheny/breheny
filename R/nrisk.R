nrisk <- function(fit, cex=0.8) {
  usr <- par("usr")
  x <- axisTicks(usr[1:2], FALSE)
  xl <- tail(x, 1)
  x <- x[-length(x)]
  s <- summary(fit, x)
  ind1 <- s$strata == levels(s$strata)[1]
  ind2 <- s$strata == levels(s$strata)[2]
  axis(3, line=0, at=s$time[ind1], labels=s$n.risk[ind1], tick=FALSE, cex.axis=cex)
  axis(3, line=1, at=s$time[ind2], labels=s$n.risk[ind2], tick=FALSE, cex.axis=cex)
  lab <- gsub(".*\\=", "", levels(s$strata))
  axis(3, line=0, at=xl, labels=lab[1], tick=FALSE, xpd=TRUE, cex.axis=cex)
  axis(3, line=1, at=xl, labels=lab[2], tick=FALSE, xpd=TRUE, cex.axis=cex)
}
