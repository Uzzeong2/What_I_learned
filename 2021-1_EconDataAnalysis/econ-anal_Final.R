
############## A : 회귀계수 & 신뢰구간(95%) ################

# 이분산성 자기상관 무시한 OLS 분석
library(sandwich)
data <- read.csv("transport.csv", header=T)
colnames(data)<-c("year","passenger","price","income","driver", "vehicle",
                  "transit","oilprice","gdp","gdp_per","taxp","tax","rail") # on highway
dat <- data[,c(1,3:7,9:13)]

attach(dat)
plot(price, type="l")

#ols.fit = lm(ln_vehicle ~ ln_P + ln_gdp + ln_taxp + ln_transit) ;ols.fit
ols.fit = lm(vehicle ~ price + income + driver + transit + gdp + gdp_per + tax + taxp + rail) ;ols.fit
summary(ols.fit)
OLS.se = summary(ols.fit)$coefficients[2,2]
t_value=summary(ols.fit)$coefficients[2,1]/summary(ols.fit)$coefficients[2,2]
t_value    # t_value 2.58 > 1.96 = H0기각

# 이분산성 감안한 t검정. 기각.
HC.se = sqrt(vcovHC(ols.fit)[2,2])
delta = summary(ols.fit)$coefficients[2,1]
t_HC = delta / HC.se # 3.43 > 1.96 줄어듦, 유의하다

# 자기상관성 감안한 HAC
rho1 = acf(ols.fit$residual)[1] #0.928
rho.se = 1/sqrt(nrow(dat)) # 0.143 < 1.96 H0채택. 데이터 오차항 독립이다

HAC.se = sqrt(vcovHAC(ols.fit)[2,2])
t_HAC = delta / HAC.se 
t_HAC
# white test
lm.fit = lm(formula = vehicle ~ price + driver + transit + rail + gdp_per + income + rail + tax + taxp, data=dat)
summary(lm.fit)

dat$gdp.sq = dat$gdp_per^2
dat$transit.sq = dat$transit^2
dat$P.sq = dat$price^2
dat$tax.sq = dat$tax^2
dat$drive.sq = dat$driver^2
e.sq = lm.fit$residuals^2

lm.fit2 = lm(e.sq ~ gdp_per + transit + price + driver + tax
             + gdp.sq + transit.sq + P.sq + tax.sq + drive.sq
             + gdp_per:transit + gdp_per:price + gdp_per:driver + gdp_per:tax
             + transit:price + transit:driver + transit:tax + price:driver
             + price:tax + driver:tax, data=dat) 
             
summary(lm.fit2)
#qchisq(0.99, df=10)

qchisq(0.95, df=10)
summary(lm.fit2)$r.square*nrow(dat) # 33.4523 > chi(10) 등분산가설 기각. 오차항이 등분산 아니다.

# FGLS 추정
lm.e = lm(e.sq ~ dat$price)
sigma.hat = sqrt(abs(lm.e$fitted.value))

y.str = dat$vehicle/sigma.hat
x1.str = dat$gdp_per/sigma.hat
x2.str = dat$transit/sigma.hat
x3.str = dat$price/sigma.hat
x4.str = dat$driver/sigma.hat

FGLS.fit = lm(y.str ~ x1.str + x2.str + x3.str + x4.str)
summary(FGLS.fit)
library(fUnitRoots);library(lmtest)
bptest(FGLS.fit, ~ ln_transit, data=dat, studentize=F) # 이분산성 없다

# 오차항 자기상관 검정, 더빈왓슨 검정
dwtest(ols.fit) # 양의 자기상관
dwtest(FGLS.fit) # 자기상관 조금 완화됐음

#income/vehicle/price, gdp/vehicle/price, gdpper/vehicle/price 
ols.fit
y = vehicle


# Ivreg
library(ivreg);library(AER)
ivreg.fit = ivreg(log(vehicle) ~ log(price) + log(gdp)|taxp + log(gdp))
ivreg.fit # 가격 탄력성 계수 -0.38 가격 1% 오르면 차 소비량 0.38% 내림
summary(ivreg.fit)

# FGLS 선택 -> ols는 유의성 과장되고, 등분산 변환
confint(ivreg.fit, level=0.95, int="confi") # 신뢰구간


############## B ################

plot(vehicle/1000, type="l")
plot(price, type="l")
plot(taxp, type="l")
# adf 검정
acf(y, main="SACF")
pacf(y, main="SPACF")

adfTest(y/1000, type="c", lag=1)
adfTest(price, type="ct", lag=1)
adfTest(taxp, type="c", lag=1)

# ARIMA 모형으로 향후 2년 검토 추세 있음
log.y = log(vehicle); n=length(log.y); d.log.y=log.y[2:n]-log.y[1:(n-1)]
plot(d.log.y, type="l") #로그계열 차분
library(forecast)
aic1 = matrix(rep(0, 5*5), 5,5); bic1=matrix(rep(0, 5*5), 5,5)
for (p in 1:5){for (q in 1:5)
  {aic1[p,q] = Arima(d.log.y, order=c(p-1,0,q-1))$aic
   bic1[p,q] = Arima(d.log.y, order=c(p-1,0,q-1))$bic}}
min(aic1) # 1,1 -> AIC order = (0,0)
min(bic1) # 1,1 BIC oder = (0,0) AR(0)

arima.fit = Arima(log.y, order=c(1,1,1))
arima.fit=Arima(log.y, order=c(1,1,1))
arima.hat=forecast(arima.fit, h=2)
confint(arima.fit, level=0.95, int = "predi")
arima.hat
######################### VAR/VEC#####################################
library(mvtnorm);library(urca)
Data = data.frame(vehicle/1000, price, taxp)
johanson.test = ca.jo(Data, type="eigen", ecdet="const")
summary(johanson.test) # rank =1
library(tsDyn)
vecm.fit=VECM(Data, lag=0, r=1, estim="ML", include = "none")
summary(vecm.fit)
beta = matrix(c(1, 1515.934, -169.138), nrow=1)
alpha = matrix(c(3.1224, 2.4e-06, 7.1e-05))
pi = alpha%*%beta

aic = matrix(rep(0, 5*5), 5,5); bic=matrix(rep(0, 5*5), 5,5)
for (p in 1:5){for (q in 1:5)
   {aic[p,q] = Arima(log(y/1000), order=c(p-1,0,q-1))$aic
   bic[p,q] = Arima(log(y/1000), order=c(p-1,0,q-1))$bic}}
which.min(aic) # 2,1 -> AIC order = (1,1)

Data = data.frame(vehicle/1000, price, taxp)
johanson.test = ca.jo(Data, type="eigen", ecdet="const")
summary(johanson.test) # 1%에서 기각 -> 10%에서 기각못함 -> rank=1
library(tsDyn);library(vars)
bic=c()
for (p in 1:10){
  bic[p] = summary(vecm.fit)$bic}
which.min(bic) # 1 : BIC order

var.form <- vec2var(johanson.test, r= 1) 

var.fit = VAR(Data, lag=1)
summary(var.fit)

var.hat = predict(var.fit, n.ahead=2)
var.hat


# ADL

# adf 검정
library(fUnitRoots)
aic=c()
for(p in 1:10){
   ar.fit = Arima(price, order = c(p,0,0), method="ML")
   aic[p] = ar.fit$aic}
which.min(aic)
plot(dat$taxp, type="l")
adfTest(dat$vehicle, type="c")
adfTest(dat$price, type="ct")
adfTest(dat$taxp, type="c")

# 공적분의 검토
y=dat$vehicle/1000
attach(dat)
reg = lm(y ~ price + taxp)
z=reg$residual
z
library(forecast)
aic=c()
for(p in 1:10){
   ar.z = Arima(z, order=c(p,0,0), method="ML")
   aic[p] = ar.z$aic}
which.min(aic) # 1

adfTest(z, type="c", lags=0) # 유의함 공적분관계 있음
dy = diff(y)
dx_1 = diff(price)
dx_2 = diff(taxp)
library(MASS)
lm.fit = lm(dy ~ dx_1 + dx_2)
summary(lm.fit)

full.model <- lm.fit
step.model <- stepAIC(full.model, direction="both", trace=FALSE)
summary(step.model)
nrow(dat)*4
z.fit=lm(y~price + taxp, data=dat)
z=z.fit$residuals[1:(49-1)]

lm.fit3 <- lm(dy ~ dx_1 + dx_2+z)
summary(lm.fit3)

price_st <- price[-1]
taxp_st <- taxp[-1]
taxp <- dat$taxp
lm.fit4 <- lm(dy ~ dx_1 + dx_2 + price_st + taxp_st)
summary(lm.fit4)
adl.hat = predict(lm.fit4, n.ahead=2)
adl.hat
confint(lm.fit4, level=0.95, int="predi")
