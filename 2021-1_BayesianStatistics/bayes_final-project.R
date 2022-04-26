library(insuranceData)
library(coda)
library(rjags)
library(runjags)

data(SingaporeAuto)
data(dataCar)

# automobile accident frequency <- vehicle variables(type,age), 
                                   # person level variables(age,gender, prior driving experience)
names(SingaporeAuto)
data <- SingaporeAuto

# poi1 <-glm(Clm_Count~ Female + PC + factor(NCD)+factor(AgeCat)+factor(VAgecat1), 
            # offset=log(Exp_weights), family=poisson(link="log"), data=SingaporeAuto)

table(data$Clm_Count)
par(mfrow=c(1,1))
y=data$Clm_Count
hist(y, freq=FALSE, breaks=c(-0.5:(max(y)+1)),
     main=NULL, xlab='Clm_Count', col=4)
X=cbind(rep(1,length(y)), data$Female, factor(data$VehicleType), factor(data$AgeCat), factor(data$VAgecat1), factor(data$NCD), data$PC)
Z=cbind(rep(1,length(y)), factor(data$VehicleType), factor(data$VAgecat1), factor(data$NCD), data$PC)
p=ncol(X);q=ncol(Z)

modelString = "model{for(i in 1:length(y)){
y[i] ~ dpois(mu.pois[i])
mu.pois[i] <- (1-S[i])*lambda[i]+1e-10*S[i]
log(lambda[i]) <- inprod(X[i,], beta[])
S[i] ~ dbern(omega[i])
logit(omega[i]) <- inprod(Z[i,], gamma[])}
for ( i in 1:p){beta[i] ~ dnorm(mu.beta[i], Tau.beta[i])}
for ( i in 1:q){gamma[i] ~ dnorm(mu.gamma[i], Tau.gamma[i])}
}
"

writeLines(modelString, "model_ZIP_mixture.txt")

#prior parameters
mu.beta=rep(0,p)
Tau.beta=rep(0.01,p)
mu.gamma=rep(0,q)
Tau.gamma=rep(0.01,q)
glm.out=glm(y~X-1, family="poisson")
beta.pois=as.vector(glm.out$coefficients)


dataList=list(p=p, q=q, y=y, X=X, Z=Z, mu.beta=mu.beta, Tau.beta=Tau.beta,mu.gamma=mu.gamma, Tau.gamma=Tau.gamma)
initsList=list(beta=beta.pois, gamma=mu.gamma)

require(rjags);
jagsModel.zip=jags.model(file="model_ZIP_mixture.txt", data=dataList, inits=initsList, n.chains=3, n.adapt=100)

update(jagsModel.zip, n.iter=3000)

codaSamples=coda.samples(jagsModel.zip, variable.names=c("beta","gamma"), thin=1, n.chains=3, n.iter=10000, nthin=10)
coda::gelman.diag(codaSamples)

Summary(codaSamples)
dic.zip = dic.samples(jagsModel.zip, 30000); dic.zip