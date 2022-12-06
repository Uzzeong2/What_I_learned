Kilometres = seq(1,5); Zone=seq(1,7); Bonus=seq(1,7); Make=seq(1,9)
temp <- expand.grid(Kilometres, Zone, Bonus, Make)
colnames(temp) = c('Kilometres', 'Zone', 'Bonus', 'Make')
temp$Kilometres = as.factor(temp$Kilometres); temp$Zone = as.factor(temp$Zone)
temp$Bonus = as.factor(temp$Bonus); temp$Make = as.factor(temp$Make)
head(temp)


library(ggplot2)

dat <- read.csv('C://Users/hyeli/Documents/hw6/swedish_car_insurance(1977).csv')
colnames(dat)[1] <- 'Kilometres'
dat$Kilometres <- ordered(dat$Kilometres)
dat$Zone <- as.factor(dat$Zone)
dat$Bonus <- ordered(dat$Bonus)
dat$Make <- as.factor(dat$Make)
dat$lninsured <- log(dat$Insured)
dat$lninsured[is.infinite(dat$lninsured)==T] = 0

dat$Claims/dat$Insured

dat$c_i = dat$Claims/dat$Insured
dat$c_i[is.na(dat$c_i)==T] == 0

glm_n <- glm(Claims/Insured ~ offset(lninsured) + Kilometres + Zone + Bonus + Make, family='poisson', data=dat, na.action='na.omit')
glm_y<- glm(Payment/Claims ~ Kilometres + Zone + Bonus + Make + Zone*Bonus, family = Gamma(link="log"), data = dat, weight=Claims)

p_ftn <- function(dataa){
  en <- predict(glm_n, data.frame(dataa,'lninsured'=mean(dat$lninsured)), type='response')
  ey <- predict(glm_y, dataa, type='response')
  p <- en * ey
  return(p)
}


dat$finalp <- p_ftn(dat[,1:4])

loss_rate <- dat$Payment / (dat$Insured * dat$finalp)
head(loss_rate)
ggplot(data.frame(loss_rate), aes(x=loss_rate)) + geom_histogram(fill='steelblue', alpha=0.8) +
  ggtitle('Histogram of Loss rate') + xlab('Loss rate')

truncated_loss <- loss_rate[which(loss_rate<=0.1)]
ggplot(data.frame(truncated_loss), aes(x=truncated_loss)) + geom_histogram(fill='steelblue', alpha=0.8) +
  ggtitle('Histogram of Loss rate (<= 0.1)') + xlab('Loss rate (<= 0.1)')

ggplot(data.frame(loss_rate), aes(x=loss_rate)) + geom_boxplot() +
  ggtitle('Boxplot of Loss rate')

total_loss_rate <- sum(dat$Payment) / (sum(dat$Insured * dat$finalp))
total_loss_rate

simple_p <- dat$Payment / dat$Insured
dat$finalp

idx = seq(1, length(simple_p))
ggplot(data.frame(idx, simple_p, dat$finalp)) + geom_line(aes(x=idx, y=simple_p), color='blue') + 
  geom_line(aes(x=idx, y=dat$finalp))

ggplot(data.frame(simple_p), aes(x=simple_p)) + geom_density(fill='steelblue', alpha=0.8) +
  ggtitle('Simplely Calculated Loss rate') + xlab('Loss rate')
ggplot(data.frame(dat$finalp), aes(x=finalp)) + geom_density(fill='steelblue', alpha=0.8) +
  ggtitle('Loss rate calculated by GLM') + xlab('Loss rate')
