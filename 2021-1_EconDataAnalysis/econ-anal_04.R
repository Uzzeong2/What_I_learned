## HW 4 ##

## 1

gov_exp = read.csv("Gov_Exp_data.csv", head=T)
attach(gov_exp)
n=length(Y); D1=ifelse(Quarter==1, 1,0); D2=ifelse(Quarter==2, 1,0); D3=ifelse(Quarter==3, 1,0); t = seq(1,n)
ols = lm(Y ~ D1+D2+D3+t)
summary(ols)

# 1)전체계수에 브레이크가 있는지 QLR 검정 (supWald)을 해보시오. 

## 2
install.packages("foreign")
library(foregn)
birth=read.csv("Kor_Birth_Death.csv", head=T); y=birth$Birth; n=length(y); d.y=y[2:n]-y[1:(n-1)]
attach(birth)
plot(d.y, type="l")
plot(y, type="l")
d.y1<-c(0, d.y)
Ols = lm(d.y1 ~ y, data=birth)
Ols.se = sqrt(vcov(Ols)[2,2])
acf(Ols$residual)
sd(Ols$residual)
summary(Ols) #유의하지 않다.
