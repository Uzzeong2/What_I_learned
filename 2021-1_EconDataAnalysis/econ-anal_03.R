---
title: "경제자료분석 HW3"
author: "202STG01 고유정"
date: '2021 5 2 '
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
## 1 ##
library(MASS)
#rho=0.5; n=100
beta1.hat.x2.omitted <- function(n,rho) {
  set.seed(1)
  Sigma=matrix(c(1,rho,rho,1),2,2); Mu = rep(0,2)
  X=mvrnorm(n,Mu,Sigma); x1=X[,1]; x2=X[,2]
  u=rnorm(n); y=1 + x1 + x2 + u
  ols_misspecified = lm(y~x1)
  return(ols_misspecified$coef[2])}
beta1.hat.x2.omitted(100,0.5)  
beta1.hat.x2.omitted(1000,0.5)  
beta1.hat.x2.omitted(10000,0.5)  
beta1.hat.x2.omitted(100000,0.5)  
beta1.hat.x2.omitted(1000000,0.5)  

## 2 ##
ciga <- read.csv("ciga.csv", header = TRUE)
attach(ciga)

d.log.q = log_Q[year==1995]- log_Q[year==1985]
d.log.p = log_P[year==1995] - log_P[year==1985]
d.log.i = log_I[year==1995] - log_I[year==1985]
d.lm = lm(d.log.q ~ d.log.p+d.log.i)
summary(d.lm)

lower = mean(d.log.q) -1.96*sd(d.log.q)
upper = mean(d.log.q) +1.96*sd(d.log.q)
CI = c(lower,upper)
CI
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
