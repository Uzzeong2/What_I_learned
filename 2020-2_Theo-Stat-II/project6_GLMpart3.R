library(ggplot2)

dat <- read.csv('C:/Users/yjk9/Documents/R/이통2/car_insurance.csv')
colnames(dat)[1] <- 'Kilometres'
dat$Kilometres <- ordered(dat$Kilometres)
dat$Zone <- as.factor(dat$Zone)
dat$Bonus <- ordered(dat$Bonus)
dat$Make <- as.factor(dat$Make)
dat$lninsured <- log(dat$Insured)
dat$lninsured[is.infinite(dat$lninsured)==T] = 0

# 1인당 연간 평균사고빈도
glm_n <- glm(Claims/Insured ~ offset(dat$lninsured) + Kilometres + Zone + Bonus + Make, family='poisson', data=dat, na.action='na.omit')

# 1건당 평균사고심도
glm_y<- glm(Payment/Claims ~ Kilometres + Zone + Bonus + Make + Zone*Bonus, family = Gamma(link="log"), data = dat, weight=Claims)

# 적정보험료 공식
p_ftn <- function(dataa){
  en <- predict(glm_n, dataa, type='response')
  ey <- predict(glm_y, dataa, type='response')
  p <- en * ey
  return(p)
}

# 추정한 적정보험료
finalp <- p_ftn(dat[,c(1:4,8)])
glm_n
glm_y
glm_p<- glm(p ~ glm_n + glm_p, family=Gamma, data = dat)

coef(summary(p_ftn))

# b)

# 적정보험료 final_p plot
idx = seq(1, length(dat$finalp))
ggplot(data.frame(idx, dat$finalp)) + 
  geom_line(aes(x=idx, y=dat$finalp), color="#0072B2") + ggtitle('Optimal Premium')

# 손해율
loss_rate <- dat$Payment / (dat$Insured * dat$finalp)
summary(loss_rate)
loss_rate

# 손해율 히스토그램
ggplot(data.frame(loss_rate), aes(x=loss_rate)) + geom_histogram(alpha=0.5, color='blue') +
  ggtitle('Histogram of Loss rate') + xlab('Loss rate')

# 손해율 박스플롯
ggplot(data.frame(loss_rate), aes(x=loss_rate)) + geom_boxplot() +
  ggtitle('Boxplot of Loss rate')

# 2미만 손해율 데이터 추출
loss_rate2 <- loss_rate[which(loss_rate <= 2)]
loss_rate2
summary(loss_rate2)

# 2미만 손해율 박스플롯
ggplot(data.frame(loss_rate2), aes(x=loss_rate2)) + geom_boxplot() +
  ggtitle('Boxplot of Loss rate less than 2')

summary(loss_rate2)


# c)

# 전체손해율
total_loss_rate <- sum(dat$Payment) / (sum(dat$Insured * dat$finalp))
total_loss_rate

# 단순 계산 보험료
simple_p <- dat$Payment / dat$Insured 
dat$finalp



# d)

# 추정한 적정보험료와 단순계산 보험료 비교 ggplot
idx = seq(1, length(simple_p))
ggplot(data.frame(idx, simple_p, dat$finalp)) + geom_line(aes(x=idx, y=simple_p), color='blue') + 
  geom_line(aes(x=idx, y=dat$finalp), color='red') + annotate('text', x=500, y=12500, label='Red : Optimal premium', color='red', size=5) +
  annotate('text', x=500, y=11500, label='Blue : Simply Calculated Premium', color='blue', size=5)

