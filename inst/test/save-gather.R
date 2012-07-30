source.dir("../../R/")

A <- rnorm(100)
B <- matrix(rnorm(100), ncol=10, nrow=10)
C <- array(rnorm(1000), dim=c(10,10,10))

Bsave(A)
Bsave(A, "A")

Bsave(list(A=A,B=B,C=C), "B")
Bsave(list(A=A,B=B,C=C), "C")
