Xbar = mean(X); Xbar
Ybar = mean(Y); Ybar
n1 = length(X); n1
n2 = length(Y); n2

ss1=var(X); ss2=var(Y); ss1; ss2
ssp <- (((n1 - 1)*ss1) + ((n2 - 1)*ss2)) / (n1+n2-2); ssp
t <- (mean(Y) - mean(X)) / sqrt(ssp * ((1/n1) + (1/n2))); t


2*pt(-abs(t), n1+n2-2)  # two tail
pt(t, n1+n2-2)          # left tail
1 - pt(t, n1+n2-2)      # right tail

t.test(X, Y, alternative = c("two.sided"), paired = FALSE, var.equal = TRUE)