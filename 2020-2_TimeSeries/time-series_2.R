
## 1

set.seed(1)
n=500; phi.1=0.6; phi.2=0.27
a = rnorm(n)
y=c(); y[1]=0; y[2]=0
for (t in 3:n) {
  y[t] = phi.1*y[t-1]+phi.2*y[t-2] + a[t]
}
plot(y, type = "l", main = "lines")
mean(y); var(y) 


rho=c(); M = 50
rho[1]=1
rho[2]=phi.1/(1-phi.2)
for (k in 3:M) {
  rho[k]=phi.1*rho[k-1] + phi.2*rho[k-2]}
plot(rho, type = "l", main = "lines")

# ACF
acf(y)
pacf(y)
