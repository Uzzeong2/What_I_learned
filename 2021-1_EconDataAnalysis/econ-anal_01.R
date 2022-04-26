## 경제자료분석 HW1 ##
# 1. 1972-2008 월별 Uncertainty Index data (UI_HW.csv) 에대해 다음의 분석을 수행하자.
# 모형 iqr ~ recession을 추정하고 recession 때 불확실성 지수가 얼마 더 높아지는지 delta 추정

 # 1) 추정치의 OLSE delta_bar를 구하여라

ui <- read.csv("UI_HW.csv", header=T)
y<-ui$iqr_gip;x<-ui$recession
ybar.U = mean(y[ui$recession==0])
ybar.D = mean(y[ui$recession==1])
delta_bar = ybar.D - ybar.U

 # 1-1) OLS.se 구하여라
OLS.fit = lm(y~x)
OLS.se = sqrt(vcov(ols.fit)[2,2])

 # 1-2) HC.se 구하여라
library(sandwich)
HC.se = sqrt(vcovHC(ols.fit)[2,2])

 # 1-3) HAC.se 구하여라
acf(OLS.fit$residual)[1]
rho_hat = 0.867; n=444
rho.se = 1/sqrt(n); rho.se
t_rho = rho_hat / rho.se; t_rho # 1.96보다 크므로 H0:rho=0 기각. u_t, u_t-1 상관관계 가진다. 독립X 
HAC.se = sqrt(vcovHAC(ols.fit)[2,2])

 # 1-4) 위에 중 어느것이 가장 적절한지 설명해라
OLS.se;HC.se;HAC.se
t_OLS = delta_bar / OLS.se; t_OLS
t_HC = delta_bar / HC.se; t_HC
t_HAC = delta_bar / HAC.se; t_HAC

 # 2-1) recession기간과 expansion기간 분산이 차이가 나는지 분산비 검정 수행
var.U = var(y[x==0]); n.U=length(y[x==0])
var.D = var(y[x==1]); n.D=length(y[x==1])
F = var.D / var.U
# F(n_D -1, n_U -1, 0.005) = F(71,371,0.005) 
# F(n_D -1, n_U -1, 0.995) = F(71,371,0.995) 
qf(0.005, 71, 371, lower.tail=TRUE)
qf(0.995, 71, 371, lower.tail=TRUE)
# H0 : varU=varD 기각, 분산 동일하지 않음

 # 2-2) 위 (2-1) 의 이분산성을 감안하여 그룹핑 방법으로 이분산성하 GLS 추정치, GLS.se 구하기
y_str = ifelse(x==0, y/sqrt(var.U), y/sqrt(var.D))
x_str = ifelse(x==0, x/sqrt(var.U), x/sqrt(var.D))
I_str = ifelse(x==0, 1/sqrt(var.U), 1/sqrt(var.D))

lm.fit = lm(y_str ~ x_str + I_str - 1)
summary(lm.fit) # gls 출력 결과
delta_hat = summary(lm.fit)$coef[2,1];delta_hat
GLS.se =  summary(lm.fit)$coef[2,2];GLS.se

# 2. 시뮬레이션
 # 1) 
set.seed(1)
 # 2) 데이터 발생
n.data = 1000

beta=c()
alpha =1; beta0 = 1; beta[1] = 1
ols.beta1 = c();ols.se1=c(); t.value=c()

x=rnorm(n.data, 0, 1)
a=rnorm(n.data, 0, 1)
u=x^alpha * a
y = beta0 + x*beta + u
lm.fit2 = lm(y~x)
ols.beta1 = summary(lm.fit2)$coef[2,1]
ols.se1 = summary(lm.fit2)$coef[2,2]
library(sandwich)
hc.se1 = sqrt(vcovHC(lm.fit2)[2,2])

 # 3) 500번 반복해서 olse 평균, 표준편차, ols.se의 평균, hc.se의 평균 구하기
n.sim = 500
ols.beta = c(); ols.se=c(); beta=1
for(t in 1:n.sim){
        x=rnorm(1000, 0, 1)
        a=rnorm(1000, 0, 1)
        u=x^alpha * a
        y = beta0 + x*beta + u
        lm.fit2 = lm(y~x)
        ols.beta[t] = summary(lm.fit2)$coef[2,1]
        ols.se[t] = summary(lm.fit2)$coef[2,2]
        hc.se[t] = sqrt(vcovHC(lm.fit2)[2,2])
        t.value[t] = ols.beta[t]/ols.se[t]
}

mean(ifelse(abs(t.value)>1.96, 1, 0)) # 기각율100%  H0:beta=0
mean(ols.beta) # olse beta의 평균
sd(ols.beta) # olse beta의 표준편차
mean(ols.se) # ols se의 평균
mean(hc.se) # hc se의 평균

 # 4) alpha 모른다고 가정하고 glse beta_hat 500번 구하라. 500개 glse beta_hat의 평균과 표준편차 구하기.
glse.beta = c(); gls.se=c()
for(t in 1:n.sim){
        # 등분산 변환
        Y_str = y/sqrt(abs(x))
        X_str = x/sqrt(abs(x))
        Int_str = 1/sqrt(abs(x))
        U_str = u/sqrt(abs(x))
        
        lm.fit3 = lm(Y_str~X_str+Int_str+U_str - 1)
        glse.beta[t] = summary(lm.fit3)$coef[2,1]
        gls.se[t] = summary(lm.fit3)$coef[2,2]
   
}

mean(glse.beta) ; sd(glse.beta)
















