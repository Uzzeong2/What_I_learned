library(forecast);library(fUnitRoots)
# ======================== A ==============================
export.data=read.csv("export.csv", head=T); attach(export.data)
head(export.data)

# 원계열 시도표
export = export.data$export
plot(export, type="l")
# 로그계열 시도표
log.export = log(export); n=length(log.export)
dlog.export=log.export[2:n]-log.export[1:(n-1)]
plot(log.export, type="l") # 증가추세 보임(비정상), 원계열보다 분산 안정적 -> 차분해서 정상화 필요
plot(dlog.export, type="l")

# AIC/BIC order 
aic=c()
for (p in 1:10){
  ar.fit=Arima(log.export, order=c(p,0,0))
  aic[p]=ar.fit$aic}
plot(aic) # AIC order = 6 최소값 위치하니까
# 단위근 검정 ADF & p-value 
#### H0 : delta=0; H1 : delta<0
adfTest(export, type="c", lags=5) #p값 커서 유의하지 않음
adfTest(log.export, type="c", lags=5) #p값 커서 유의하지 않음
adfTest(dlog.export, type="c", lags=5) #reject H0, 유의함. 정상계열 취급.->차분필요

# Arima(p,1,q) 모형추정. p,q의 BIC order?
aic=matrix(rep(0, 9*4), 9,4); bic=matrix(rep(0, 9*4), 9,4)
for (p in 1:9){for (q in 1:4)
  {aic[p,q]=Arima(dlog.export, order=c(p-1,0,q-1))$aic
   bic[p,q]=Arima(dlog.export, order=c(p-1,0,q-1))$bic}}
aic # AIC order = (9-1,4-1)=(8,3)
bic # BIC order = (8-1,3-1)=(7,2)

# Arima(p,1,q) 추정된 방정식
arima.fit=Arima(log.export, order=c(1,1,1))
arima.fit # d.lnYt = 0.2466d.lnY_t-1 + a_t -0.8259a_t-1

# 미래 1년까지의 로그계열의 예측치, 95%예측구간
arima.hat = forecast(arima.fit, h=12)
arima.hat
plot(arima.hat)

# ======================== B ==============================
library(mvtnorm)
set.seed(1)
A = matrix(c(1.2, 0.2, -0.4, 0.6), nrow=2, byrow=T)
e = rmvnorm(100, sigma=diag(2))
y = matrix(0, nrow=100, ncol=2)

y[1,] = e[1,]
for(t in 2:100){
  y[t,] = A%*%as.matrix(y[(t-1),]) + as.matrix(e[t,])
  
}
#y1 시도표
plot(y[,1], type="b", pch=19, cex=0.6, xlab= "", ylab= "")
lines(y[,1])
abline(h=0, lty=2, col=4)
legend("topright", col=1, pch=19, lty=1,
       legend="y1", bty="n")
#y2 시도표
plot(y[,2], type="b", pch=18, cex=0.6, xlab= "", ylab= "", col=2)
lines(y[,2], col=2)
abline(h=0, lty=2, col=4)
legend("topright", col=2, pch=19, lty=1,
       legend="y2", bty="n")

# A의 eigenvalue
eigen(A) # y1:I(1), y2:I(1), y2-y1=I(0), cointegrated

# Vec 모형
I=diag(2)
Pi=A-I
rk(Pi) # 1개의 균형식 공적분
Pi
A%*%eigen(A)$vector[,1]
eigen(A)$value[1]*eigen(A)$vector[,1]

# 공적분 벡터 beta
library(tsDyn);library(urca)
Data = data.frame(y1=y[,1], y2=y[,2])
vecm.fit = VECM(Data, lag=0, r=1, estim="ML", include="none")
summary(vecm.fit)
beta = matrix(c(1, 0.9998379), nrow=1, byrow=T)
 #alpha = 열벡터(0.2142, -0.4055)

# Z_t = beta*Y_t 시도표, I(0) or I(1)?
z = matrix(0, nrow=1, ncol=100)
for(t in 1:100){
  z[,t]= beta%*%y[t,]
}
Z = t(z)
plot(Z, type="b")
plot(Z, type="b", pch=19, cex=0.6, xlab= "", ylab= "Z")
lines(Z, col=1)
abline(h=0, lty=2, col=2)


