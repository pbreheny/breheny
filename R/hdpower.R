# Calculate 'sensitivity', the probability of detecting a truly DE gene
hdpower <- function(n, FDR, p0, D=1, sig=1, n1, n2) {
  # Setup
  if (!missing(n1) & !missing(n2)) {
    df <- n1 + n2 - 2
  } else {
    df <- n - 2
    n1 <- n2 <- n/2
  }
  p1 <- 1-p0
  ncp <- D / sig * 1/sqrt(1/n1 + 1/n2)
  
  # Solve for CV satisfying FDR constraint
  f <- function(CV) {
    fp <- p0*(1-pt(CV, df))
    tp <- p1*(1-pt(CV, df, ncp))
    fp/(fp+tp) - FDR
  }
  CV <- uniroot(f, c(0, 10))$root
  
  # Return sensitivity
  1-pt(CV, df, ncp)
}
