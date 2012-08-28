load("/peds/laham/extubation/data/data.RData")
Data <- subset(AllData, Attempt=="P1")
df <- Data[, -which(names(Data) %in% c("MR", "DateExtubated", "Outcome", "Planned", "Service", "Etiology", "Diagnosis", "Surgery", "Intensivist", "Stridor", "LOS", "Cost", "Attempt"))]
require(party)
source.dir("~/dev/breheny/R/")

cvTree(Success~., df, df$Success)

X <- model.matrix(Success~., df)

cv

require(cvTools)

call <- call("ctree", formula=Success ~ ., control=ctree_control(testtype="Univariate", mincriterion=.93))
cv <- cvFit(call, data=df, y=df$Success)

fit <- ctree(Success ~ ., df, control=ctree_control(mincriterion=.8))
pdf("tree.pdf")
plot(fit, ip_args=list(id=FALSE, pval=FALSE), tp_args=list(id=FALSE))
dev.off()


library("robustbase")
data("coleman")
fit <- lmrob(Y ~ ., data=coleman)
cvFit(fit, data = coleman, y = coleman$Y, cost = rmspe, K = 5, R = 10, costArgs = list(includeSE=TRUE))



