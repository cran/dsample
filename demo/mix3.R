##
## Example from Liang (2007)
## Mixture of three normal distributions
##

library(dsample)
expr <- expression(
  1/3*mnormt::dmnorm(x=cbind(x1,x2),
                     mean=c(-8,-8),
                     varcov=matrix(c(1,0.9,0.9,1), ncol=2))
  + 1/3*mnormt::dmnorm(x=cbind(x1,x2),
                       mean=c(6,6),
                       varcov=matrix(c(1,-0.9,-0.9,1), ncol=2))
  + 1/3*mnormt::dmnorm(x=cbind(x1,x2),
                       mean=c(0,0),
                       varcov=matrix(c(1,0,0,1), ncol=2)))
sets <- list(x1=runif(n=1e4, min=-12, max=11),
             x2=runif(n=1e4, min=-12, max=11))
y <- eval(expr=expr, env=sets)
smp <- dsample(expr=expr, rpmat=sets, nk=3e3, n=5e2)
op <- summary(smp, k=3)
op$means
op$modes
do.call(cbind, lapply(split(op$X, op$grp), colMeans))
plot(op, which=2)
