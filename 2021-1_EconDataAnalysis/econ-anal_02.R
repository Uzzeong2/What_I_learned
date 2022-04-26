## HW2 ##

## PART 1 
# 1
set.seed(1)
library(sandwich)
x=c(); y=c(); u=c(); n=100
x[1]=0; u[1]=0; e=rnorm(n); a=rnorm(n); u=rnorm(n)
y[1]=1+x[1]+u[1]
phi_x = 0.5
phi_u = 0.7
for (t in 2:n) {
  x[t]=phi_x*x[t-1]+e[t]
  u[t]=phi_u*u[t-1]+a[t]
  y[t]=1+x[t]+u[t]}

# 2
ols.fit = lm(y~x)
b=ols.fit$coef[2];b
ols.se = summary(ols.fit)$coef[2,2]
hac.se = sqrt(vcovHAC(ols.fit)[2,2]);hac.se

# 3
ols.res.0=ols.fit$residual[2:n]
ols.res.1=ols.fit$residual[1:(n-1)]
phi_u.hat=lm(ols.res.0 ~ ols.res.1 - 1)$coef[1]
x_s = c(); y_s = c()
for (t in 2:n){
  x_s[t]=x[t]-phi_u.hat*x[t-1]
  y_s[t]=y[t]-phi_u.hat*y[t-1]}
gls.fit = lm(y_s ~ x_s)
b.gls = gls.fit$coef[2];b.gls
gls.se=summary(gls.fit)$coef[2,2];gls.se 

# 4
t1=(b-1)/ols.se
t2=(b-1)/hac.se
t3=(b.gls-1)/gls.se
I1=ifelse(abs(t1)>1.96,1,0)
I2=ifelse(abs(t2)>1.96,1,0)
I3=ifelse(abs(t3)>1.96,1,0)

# simulation 10000번
set.seed(1)
n.sim = 10000;x=c(); y=c(); u=c(); n=100
for(s in 2:n.sim){
  x[1]=0; u[1]=0; e=rnorm(n); a=rnorm(n); u=rnorm(n)
  y[1]=1+x[1]+u[1]
  phi_x = 0.5
  phi_u = 0.7
  for (t in 2:n) {
    x[t]=phi_x*x[t-1]+e[t]
    u[t]=phi_u*u[t-1]+a[t]
    y[t]=1+x[t]+u[t]}
  ols.fit = lm(y~x)
  b[s]=ols.fit$coef[2]
  ols.se[s] = summary(ols.fit)$coef[2,2]
  hac.se[s] = sqrt(vcovHAC(ols.fit)[2,2])
  ols.res.0=ols.fit$residual[2:n]
  ols.res.1=ols.fit$residual[1:(n-1)]
  phi_u.hat=lm(ols.res.0 ~ ols.res.1 - 1)$coef[1]
  x_s = c(); y_s = c()
  for (t in 2:n){
    x_s[t]=x[t]-phi_u.hat*x[t-1]
    y_s[t]=y[t]-phi_u.hat*y[t-1]}
  gls.fit = lm(y_s ~ x_s)
  b.gls[s] = gls.fit$coef[2]
  gls.se[s]=summary(gls.fit)$coef[2,2]
}


# 표 채우기
mean(b);sd(b) # a,b
mean(b.gls);sd(b.gls) # c,d
mean(ols.se);mean(hac.se);mean(gls.se) # e,f,g

# t검정

t1=(b-1)/ols.se;t1
t2=(b-1)/hac.se;t2
t3=(b.gls-1)/gls.se;t3
I1=ifelse(abs(t1)>1.96,1,0); I1
I2=ifelse(abs(t2)>1.96,1,0); I2
I3=ifelse(abs(t3)>1.96,1,0); I3

mean(I1);mean(I2);mean(I3)


## PART 2 : Volatility Spillover Index data
vsi=read.csv("C:/Users/yjk9/Documents/R/econAnalysis/VSI.csv", head=T)
attach(vsi)
plot(index,type="l")

# 1
y = vsi$index
D = vsi$crisis
ybar.U = mean(y[D==0])
ybar.D = mean(y[D==1])
delta_hat = ybar.D - ybar.U;delta_hat
Ols.fit = lm(y ~ D)
summary(Ols.fit)
Ols.se = summary(Ols.fit)$coef[2,2];Ols.se

# 2
acf(Ols.fit$residual)[1]
rho = 0.995
n = 3526
Ols.se = sqrt(vcov(Ols.fit)[2,2])
t_ols = delta_hat / Ols.se # 타당하지 않다, 상관관계 있다

install.packages("lmtest")
library(lmtest)
dwtest(Ols.fit) # H0기각 자기상관 있음

# 3 
Hac.se = sqrt(vcovHAC(Ols.fit)[2,2]) # 오차항의 자기상관을 감안한 오차
t_hac = delta_hat / Hac.se # ols보다는 타당하다, 상관관계 있다

# 4
upper = delta_hat + 1.96*Hac.se/sqrt(n)
lower = delta_hat - 1.96*Hac.se/sqrt(n)
CI = c(lower, upper)








