library(BSDA)
# Calculate sample variances
ss1 = var(X)
ss2 = var(Y)

# Calculate sample sizes
n1 = length(X)
n2 = length(Y) 

# Calculate the testing statistic and degree of freedom.
t = (mean(X)-mean(Y))/sqrt(ss1/n1+ss2/n2)

# calculate the degree of freedom
k = (ss1/n1+ss2/n2)^2/((ss1/n1)^2/(n1-1)+(ss2/n2)^2/(n2-1))
k = ceiling(k)


pt(t,k)         ## p-value for Ha:mu1 < mu2
1-pt(t,k)       ## p-value for Ha:mu1 > mu2
2*pt(-abs(t),k) ## p-value for Ha:mu1 != mu2

# Use the built-in function
t.test(X, Y, alternative=c("less"), paired=FALSE, var.equal=FALSE)
t.test(X, Y, alternative=c("greater"), paired=FALSE, var.equal=FALSE)
t.test(X, Y, alternative=c("two.sided"), paired=FALSE, var.equal=FALSE)