
library(tidyverse) ; library(gridExtra) ;library(rmutil) ; library(numDeriv)
library(shiny); library(DT); library(ggplot2); library(shinydashboard)
library(plotly); library(semantic.dashboard)

#part(3)


ui <- dashboardPage(
  
  
  dashboardHeader("적정보험료 산정"),
  dashboardSidebar(
    
    fileInput(inputId="files", label="Choose file"),
    conditionalPanel(
      "output.fileUploaded == true",
      selectInput(inputId="check", label="Variables", choices=""),
      textInput(inputId="n",label="가격 범위를 선택하세요.", value="6"),
      textInput(inputId="N",label="현재까지 가입자를 입력하세요.", 
                value="10000"))),
  
  dashboardBody(
    
    box(textInput(inputId="deduct",label="공제액을 입력하세요.",
                  value="200000")),
    box(textInput(inputId="limit",label="보상한도를 선택하세요.",
                  value="100000000")),
    box(selectInput(inputId="dist",label="Claim Size Distribution",
                  choices=c("Log-normal","Log-logit","Log-laplace","Log-gumbel","Pareto"))),
    
    box(actionButton("go", label = "Go!"))
      #valueBoxOutput("적정보험료"))   
      
  )
    

   
)



# server.R
server <- function(input, output,session) {
  qq <- function(dist,data,price,n){
    freq <- data
    groupx <- as.numeric(price[1:n])
    r <- cumsum(data)[1:n]
    pr <- r / (sum(freq[1:(n+1)]) + 1)
    
    if(dist=="Log-normal"){
      xr <- log(groupx)
      inverser <- qnorm(pr)
      lm <- lm(xr ~ inverser)
      coef <- lm$coefficients
    }
    else if(dist=="Log-logit"){
      xr <- log(groupx)
      inverser <- log(pr/(1-pr))
      lm <- lm(xr ~ inverser)
      coef <- lm$coefficients
    }
    else if(dist=="Log-laplace"){
      xr <- log(groupx)
      inverser <- -sign(pr-0.5)*log(1-2*abs(pr-0.5))
      lm <- lm(xr ~ I(inverser))
      coef <- lm$coefficients
    }
    else if(dist=="Log-gumbel"){
      xr <- log(groupx)
      inverser <- -log(-log(pr))
      lm <- lm(xr ~ I(inverser))
      coef <- lm$coefficients
    }
    else{
      lambda <- function(par) {
        xr <- log(1 + groupx / par) 
        inverser <- -log(1 - pr)
        lm <- lm(xr ~ inverser + 0)
        return(-summary(lm)$r.squared)}
      
      lambda <- optimize(lambda, c(0, sum(freq)*10))$minimum
      
      xr <- log(1 + groupx / lambda) 
      inverser <- -log(1 - pr)
      lm <- lm(xr ~ inverser + 0)
      coef <- c(lambda, 1/lm$coefficients[1])
    }
    
    p <- if(dist!="Pareto"){ggplot() + geom_point(aes(x=inverser,y=xr))+
        ggtitle(paste(dist, "Q-Q plot")) +
        xlab("Theoreticacl Quantile")+
        ylab("Empirical Quantile")+
        geom_abline(aes(slope=lm$coef[2],intercept=lm$coef[1],color="red"))+
        theme_bw()+theme(legend.position="none")+
        labs(subtitle=paste("mu=",round(lm$coef[1],3),"  sigma=",round(lm$coef[2],3),
                            "  R.squared",round(summary(lm)$r.squared,3)))}
    else{
      ggplot() + geom_point(aes(x=inverser,y=xr))+
        ggtitle(paste(dist, "Q-Q plot")) +
        xlab("Theoreticacl Quantile")+
        ylab("Empirical Quantile")+
        geom_abline(aes(slope=lm$coef[1],color="red",intercept=0))+
        theme_bw()+theme(legend.position="none")+
        labs(subtitle=paste("lambda=",round(lambda,3),"  alpha=",round(1/lm$coef[1],3),
                            "  R.squared",round(summary(lm)$r.squared,3)))
    }
    
    list(p=p, estimate=coef)
  }
  
  premium <- function(dist,data,price,n,N,A,B){
    if(dist=="Log-logit"){
      mu <- qq(dist,data,price,n)$estimate[1]
      sigma <- qq(dist,data,price,n)$estimate[2]
      
      f <- function(x) {exp(-(log(x) - mu)/sigma) * (1/x/sigma)/(1 + exp(-(log(x) - mu)/sigma))^2}
      F <- function(x) { 1 / (1 + exp(-(log(x)-mu)/sigma)) }
      xf<-function(x) {x * f(x)}
      
      Ey <- function(A,B){
        integrate(xf, A, A + B)$value - A * (F(A + B)-F(A)) + B * (1 - F(A + B)) 
      }
      
      En <- sum(data[1:(n+1)])/N
      pre <- Ey(A,B)*En
    }
    else if(dist=="Log-normal"){
      mu <- qq(dist,data,price,n)$estimate[1]
      sigma <- qq(dist,data,price,n)$estimate[2]
      
      f <- function(x) { dnorm(log(x), mean = mu, sd = sigma, log = FALSE)/x }
      F <- function(x) { pnorm(log(x),mean = mu, sd = sigma) }
      xf<-function(x) {x * f(x)}
      
      Ey <- function(A,B){
        integrate(xf, A, A + B)$value - A * (F(A + B)-F(A)) + B * (1 - F(A + B)) 
      }
      
      En <- sum(data[1:(n+1)])/N
      pre <- Ey(A,B)*En
    }
    else if(dist=="Pareto"){
      lambda <- qq(dist,data,price,n)$estimate[1]
      alpha <- qq(dist,data,price,n)$estimate[2]
      
      f <- function(x) { alpha * (lambda^alpha) * (lambda + x)^(- alpha - 1) }
      F <- function(x) { 1 - (lambda / (lambda + x))^alpha }
      xf<-function(x) {x * f(x)}
      
      Ey <- function(A,B){
        integrate(xf, A, A + B)$value - A * (F(A + B)-F(A)) + B * (1 - F(A + B)) 
      }
      
      En <- sum(data[1:(n+1)])/N
      pre <- Ey(A,B)*En
    }
    else if(dist=="Log-laplace"){
      mu <- qq(dist,data,price,n)$estimate[1]
      sigma <- qq(dist,data,price,n)$estimate[2]
      
      F <- function(x) { plaplace(log(x),m=mu,s=sigma) }
      f <- function(x) {grad(F,x)}
      
      xf<-function(x) {x * f(x)}
      
      Ey <- function(A,B){
        integrate(xf, A, A + B)$value - A * (F(A + B)-F(A)) + B * (1 - F(A + B)) 
      }
      
      En <- sum(data[1:(n+1)])/N
      pre <- Ey(A,B)*En
    }
    else{
      mu <- qq(dist,data,price,n)$estimate[1]
      sigma <- qq(dist,data,price,n)$estimate[2]
      
      tau <- 1/sigma
      c <- exp(mu * tau)
      
      f <- function(x) {c * tau * exp(-c/x^tau)/x^(tau + 1)}
      F <- function(x) { exp(-c/x^tau) }
      
      xf<-function(x) {x * f(x)}
      
      Ey <- function(A,B){
        integrate(xf, A, A + B)$value - A * (F(A + B)-F(A)) + B * (1 - F(A + B)) 
      }
      
      En <- sum(data[1:(n+1)])/N
      pre <- Ey(A,B)*En
      
      
    }
    return(pre=pre)
  }
  
  inFile <- reactive({
    if (is.null(input$file)) {} else {
      input$file
    }
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(inFile()))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$text1 <- renderText({
    paste("공제액(A)을", input$deduct, "원,"," 보상한도(B)를", input$limit, "원으로 설정하셨습니다.")
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
    } else {
      read.csv(inFile()$datapath)
    }
  })
  
  observe({
    updateSelectInput(
      session,
      "check",
      choices=names(myData()))
  })
  
  
  selectedData <- reactive({ 
    if( is.null(inFile())){
    } else{
      myData() %>% select(x=input$check)}
  })
  
  selectedData2 <- reactive({ 
    if( is.null(inFile())){
    } else{
      myData() %>% select(x=price)}
  })
  
  pre <- reactive({
    if( is.null(inFile())){
    } else{
      premium(input$dist,selectedData()[,1],
              selectedData2()[,1],as.numeric(input$n),as.numeric(input$N),
              as.numeric(input$deduct),as.numeric(input$limit))
    }
  })
  
  txt <- eventReactive(input$go, {
    return(paste("고객님의 적정보험료는", round(pre()), "원 입니다."))
  })
  
  output$text <- renderText({
    txt()
  })
  
}



shinyApp(ui = ui, server = server)
