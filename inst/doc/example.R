## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", fig.width=8, fig.height=8
)

## ----setup--------------------------------------------------------------------
library(dsample)

## ----echo=FALSE, out.width="45%"----------------------------------------------
expr <- expression((x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37)
sets <- list(x1=runif(1e4), x2=runif(1e4))
smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp, n=10, k=2)
# op$means
# op$modes
# do.call(cbind, lapply(split(op$X, op$grp), colMeans))
plot(op, which=2)

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
sets <- list(x1=runif(n=1e3, min=-12, max=11),
             x2=runif(n=1e3, min=-12, max=11))
y <- eval(expr=expr, env=sets)
smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp, k=3)
# op$means
# op$modes
# do.call(cbind, lapply(split(op$X, op$grp), colMeans))
plot(op, which=2)

## ----echo=FALSE---------------------------------------------------------------
## data 
y <- c( 1, 1, 1,  1, 0, 0, 0,  0, 0, 0, 0,  0, 1, 1, 0,  0, 0, 1, 0,  0, 0, 0, 0)
temp <- c( 53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81)

## Initialization 
len <- length(y)
fit <- glm(y~temp, family=binomial(link="logit"))
alpha.hat <- coef(fit)[1]
gamma <- 0.577216
b <- exp(alpha.hat + gamma)

nd <- 1e4

## -----------------------------------------------------------------------------
expr <- str2expression("
  lp <- 0
  for(i in 1:len) lp <- lp + 
    y[i] * log(exp(alpha + beta*temp[i])/(1+exp(alpha + beta*temp[i])))
  for(i in 1:len) lp <- lp + 
    (1-y[i])*log(1/(1+exp(alpha + beta*temp[i])))
  lp <- lp + alpha - exp(alpha)/b
  lp <- exp(lp)
")

sets <- list(
  alpha=runif(n=nd, min=10, max=20), 
  beta=runif(n=nd, min=-0.3, max=-0.15)
)

smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp)
op$means
op$stdevs

## ----echo=FALSE---------------------------------------------------------------
wi <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839);
yi <- c(6, 13, 18, 28, 52, 53, 61, 60);
ni <- c(59, 60, 62, 56, 63, 59, 62, 60);
data <- data.frame(wi=wi, yi=yi, ni=ni);

# Given Initial values
a <- 0.25
b <- 4.0
c1 <- 2.0
d <- 10.0 
e <- 2.000004
f <- 1e3
        
# Specifying the range on the parameters
nd <- 1e4
len <- nrow(data)

## -----------------------------------------------------------------------------
expr <- str2expression("
  sigma <- exp(log.sigma)
  m1 <- exp(log.m1)
  
  lp <- 0
  for(i in 1:len) lp <- lp + 
    yi[i]*m1*log((exp((wi[i]-mu)/sigma)/(1+exp((wi[i]-mu)/sigma))))
  for(i in 1:len) lp <- lp + 
    (ni[i]-yi[i])*log(( 1- (exp((wi[i]-mu)/sigma)/(1+exp((wi[i]-mu)/sigma)))^m1 ))
  lp <- lp + (a-1)*log.m1 - 2*(e+1)*log.sigma
  lp <- lp - 0.5*((mu-c1)/d)^2
  lp <- lp - m1/b - 1/(f*sigma^2)
  lp <- exp(lp)
")

sets <- list(
  mu=runif(nd, min=1.75, max=1.85), 
  log.sigma=runif(nd, min=-5, max=-3), 
  log.m1=runif(nd, min=-2, max=0.1)
)

smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp)
op$means
op$stdevs

## ----echo=FALSE---------------------------------------------------------------
x.age <- c( 1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0, 8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
y.length <- c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.35, 2.47, 2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.43, 2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
data <- data.frame(x.age=x.age, y.length=y.length)

# Given Initial values        
len <- nrow(data)
k <- 10^(-3)
tau.alpha <- 10^(-4)
tau.beta <- 10^(-4)

nd <- 1e4

## -----------------------------------------------------------------------------
expr <- str2expression("
  lp <- (len/2 + k - 1)*log(tau)
  for(i in 1:len) lp <- lp - 
    tau*0.5*(y.length[i] - alpha+beta*gamma^x.age[i])^2
  lp <- lp - tau*k - tau.alpha*alpha^2*0.5 - tau.beta*beta^2*0.5
  lp <- exp(lp)
")

sets <- list(
  alpha=runif(nd, min=2, max=3), 
  beta=runif(nd, min=0.5, max=1.5), 
  gamma=runif(nd, min=0.5, max=1.5), 
  tau=runif(nd, min=0.2, max=200)
)

smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp)
op$means
op$stdevs

## ----echo=FALSE---------------------------------------------------------------
x <- c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2, 0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1);

len <- length(x);
nd <- 1e4

cum.x.until.k <- cumsum(x);
cum.x.after.k <- sum(x) - cum.x.until.k;

## -----------------------------------------------------------------------------
expr <- str2expression("
  ll <- 0
  ll <- ll + (cum.x.until.k[kappa]-0.5)*log(theta) + 
        (cum.x.after.k[kappa]-0.5)*log(lambda) - 
        kappa*theta -  (len-kappa)*lambda
  lp <- ll  + 1.5*log(alpha) + 1.5*log(beta) - 
        (theta+1)*alpha - (lambda+1)*beta
  lp <- exp(lp)
")

sets <- list(
  kappa=sample(x=30:50, size=nd, replace=TRUE),
  theta=runif(nd, min=2.2, max=4),
  lambda=runif(nd, min=0.6, max=1.4),
  alpha=runif(nd, min=0, max=2),
  beta=runif(nd, min=0, max=4)
)

smp <- dsample(expr=expr, rpmat=sets, nk=1e3, n=1e3)
op <- summary(smp)
op$means
op$stdevs

## ----echo=FALSE---------------------------------------------------------------
time <- c(94.32, 15.72, 62.88, 125.76, 5.24, 31.44, 1.05, 1.05, 2.10, 10.48)
failure <- c( 5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
data <- data.frame(time=time, failure=failure)

len <- nrow(data)
alpha <- 0.54
gg <- 2.20
delta <- 1.11

nd <- 1e5

## -----------------------------------------------------------------------------
expr <- str2expression("
  ll <- 0
  for(i in 1:len){
    sum.cmd <- gsub(' ', '', paste('ll <- ll +(failure[', i,']+alpha-1)*log(lambda', i,')'))
    eval(parse(text=sum.cmd))
  }
  for(i in 1:len){
    sum.cmd <- gsub(' ', '', paste('ll <- ll - (time[', i,']+bb)*lambda', i))
    eval(parse(text=sum.cmd))
  }
  
  lp <- ll + (10*alpha+gg-1)*log(bb) - delta*bb
  lp <- exp(lp)
")

sets <- list(
  bb=runif(nd, 0, 4),
  lambda1=runif(nd, 0, 0.2),
  lambda2=runif(nd, 0, 0.4),
  lambda3=runif(nd, 0, 0.25),
  lambda4=runif(nd, 0, 0.25),
  lambda5=runif(nd, 0, 2),
  lambda6=runif(nd, 0, 1.5),
  lambda7=runif(nd, 0, 2),
  lambda8=runif(nd, 0, 2),
  lambda9=runif(nd, 0, 4),
  lambda10=runif(nd, 0, 3.5)
)

smp <- dsample(expr=expr, rpmat=sets, nk=5e4, n=3e3)
op <- summary(smp)
op$means
op$stdevs

