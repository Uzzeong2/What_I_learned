library(readxl)
library(ggplot2)
install.packages('tidyr')
library(tidyr)

alzheimer <- readxl::read_xlsx('C:\\Users\\yjk9\\Desktop\\Alzheimer1.xlsx')
alzheimer

library(dplyr)

alzheimer%>%mutate('boxplot'=c(rep(0:4,times=25),rep(10:14,times=22)))%>%
  ggplot(aes(as.factor(boxplot),y))+stat_boxplot(geom='errorbar',width=0.3)+
  geom_boxplot(aes(group=boxplot))+labs(x='',y='')

library(rjags)
library(runjags)

y<-alz$y
I<-alz$lecithin
t<-alz$t

### The model specification

## Likelihood function : L(mu,sigma | Data)

example1.bug <- "model{
    for(i in 1:length(y)) {
      y[i] ~ dbinom(p[i],30)
      p[i] <- ilogit(a+c*I[i]+(d*I[i]+b)*t[i]/12)
    }
  ## prior distribution for parameters (mu, sigma)
    a ~ dunif(-20,20)
    b ~ dunif(-20,20)
    c ~ dunif(-20,20)
    d ~ dunif(-20,20)
  }"

#### Running the model in JAGS
model<-jags.model(textConnection(example1.bug),
                  data=list(y=y,I=I,t=t),n.chains = 3, n.adapt= 10000)


update(model, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples <- coda.samples(model,variable.names=c("a","b","c",'d'),
                             n.iter=20000, thin=10)

for(i in 1:2){
  plot(mcmc_samples[,c(2*i-1,2*i)])
}

summary(mcmc_samples)

alz2<-read_xlsx('C:\\Users\\yjk9\\Desktop\\Alzheimer.xlsx',sheet=2)
alz2

y<-alz2[,2:6]
I<-alz2$lecithin


### The model specification

## Likelihood function : L(mu,sigma | Data)

example1.bug <- "model{
    for(i in 1:47){
      for(t in 1:5){
        y[i,t] ~ dbinom(p[i,t],30)
        p[i,t] <- ilogit(alpha[i]+beta[i]*(t-1)/12)
      }
        alpha[i] <- a+sigma*u[i]+c*I[i]
        beta[i] <- b+d*I[i]
        u[i] ~ dnorm(0,1)
    }
  ## prior distribution for parameters (mu, sigma)
    a ~ dunif(-20,20)
    b ~ dunif(-20,20)
    c ~ dunif(-20,20)
    d ~ dunif(-20,20)
    sigma ~ dunif(0, 10)
  }"

#### Running the model in JAGS
model2<-jags.model(textConnection(example1.bug),
                   data=list(y=y,I=I),n.chains = 3, n.adapt= 10000)

update(model2, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples2 <- coda.samples(model2,
                              variable.names=c("a","b","c",'d',"u","sigma"),
                              n.iter=20000, thin=10)

for(i in 1:17){
  plot(mcmc_samples2[,c(3*i-2,3*i-1,3*i)])
}


alpha.star<-c()
for(i in 1:nrow(y)){
  alpha.star[i]<-log((y$y1[i]+1/2)/(30-y$y1[i]+1/2))
}

mcmc_samples2.1 <- coda.samples(model2,
                                variable.names=c("alpha"),
                                n.iter=20000, thin=10)
alpha<-summary(mcmc_samples2.1)$statistics[,1]
alpha.m<-cbind(as.data.frame(alpha),alpha.star)
c<-coef(lm(alpha~alpha.star,data=alpha.m))
ggplot(alpha.m,aes(alpha.star,alpha))+geom_point()+
  geom_smooth(method='lm',se=F)+
  annotate(geom='text',x=-3,y=0,
           label=paste('R.squared=',
                       round(summary(lm(alpha~alpha.star,
                                        data=alpha.m))$r.squared,4)))+
  ggtitle(paste('y=',round(c[1],3),'+',round(c[2],3),'*x'))