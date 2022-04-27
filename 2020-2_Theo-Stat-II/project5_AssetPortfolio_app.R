#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# hw5 rshiny optimal portfoio

getwd()
setwd("C:/Users/yjk9/Documents/R/theo_stat")
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(GGally)
library(DMwR)
library(survival)
library(extraDistr)
library(rmarkdown)
library(survival) 
library(ggplot2) 
library(survminer)
library(shinythemes)
library(devtools)
library(dashboardthemes)
library(shinydashboardPlus)
library(leaflet)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

Sys.setlocale("LC_ALL", "korean")
## Sidebar Code

ui = dashboardPage(skin='purple',
                  
    
    dashboardHeader(title = "Optimal Portfolio"),
    
    # sidebar menu
    dashboardSidebar(
        sidebarMenu(
            
            id = "tabs",
            # Charts
            menuItem("Data", icon = icon("th"),
                     fileInput("file1", label = h4("File input"), 
                               accept = ".csv"),
                     fileInput("file2", label = NULL, 
                               accept = ".csv"),
                     fileInput("file3", label = NULL, 
                               accept = ".csv"),
                     fileInput("file4", label = NULL, 
                               accept = ".csv"),
                     fileInput("file5", label = NULL, 
                               accept = ".csv")
            ),
            textInput("year","start of investment(year)", "2013"),
            textInput("V0","amount of investment(V0, unit:10million)", "1"),
            sliderInput("theta0", "weight of deposit", 0, 1,value=0.2, step=0.05),
            sliderInput("istar", "target return(%)", 1, 100,value=7, step=1),
            textInput("M","investment period(M, year)", "5"),
            textInput("N","sample size(N, year)", "5"),
            actionButton("go", label = "Go!")
        )
    ),


    
    dashboardBody(
        
        fluidRow(
            gradientBox(title = div("Value of Optimal Portfolio",style="font-size:130%"),width=7,gradientColor = "purple",  footer= plotOutput("plot", height = 270)),
            gradientBox(title = div("Criterion",style="font-size:130%"),width=5, gradientColor = "maroon", footer=div(tableOutput("criterion"),style="font-size:100%"))
        ),
        
        fluidRow(
            gradientBox(title = div("Optimal Portfolio table",style="font-size:130%"), width=7,gradientColor = "purple", footer=div(tableOutput("thetas"),style="font-size:130%")),
            gradientBox(title=div("Validation",style="font-size:130%"),width=5,gradientColor="green",footer=div(tableOutput("validation"),style="font-size:100%"))
        )
        
    ))
    
    
    
    # App Start
server = shinyServer(function(input, output){
    SE <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath)
    })
    
    KA <- reactive({
        req(input$file2)
        read.csv(input$file2$datapath)
    })
    
    KT <- reactive({
        req(input$file3)
        read.csv(input$file3$datapath)
    })
    
    GAS <- reactive({
        req(input$file4)
        read.csv(input$file4$datapath)
    })
    
    NAVER <- reactive({
        req(input$file5)
        read.csv(input$file5$datapath)
    })
            
    i <- read_csv("ecosshiny.csv")
            
    investor <- eventReactive(input$go, {
        as.numeric(c(input$theta0,input$V0,input$istar,input$year,input$M,input$N))})
    
    portfolio2 <- function(SE, KA, KT, GAS, NAVER,investor){
        rtfx <- function(x) {
            diff(x)/x[1:(length(x)-1)]
        }
        pit <- data.frame(p1t=(SE$High+SE$Low)/2,p2t=(KA$High+KA$Low)/2,p3t=(KT$High+KT$Low)/2,
                          p4t=(GAS$High+GAS$Low)/2,p5t=(NAVER$High+NAVER$Low)/2)
        rit <- data.frame(date=i$Date[2:166],r0t=(((1+i$market_interest/100)^(1/12))-1)[2:166],r1t=rtfx(pit$p1t)[1:165],r2t=rtfx(pit$p2t)[1:165],
                          r3t=rtfx(pit$p3t)[1:165],r4t=rtfx(pit$p4t)[1:165],r5t=rtfx(pit$p5t)[1:165])
        V0 <- investor[2]
        m <- 12*investor[5]
        n <- 12*investor[6]
        i12star <- (1+investor[3]/100)^(1/12)-1
        r <- i12star
                
        rttrain <- rit %>% filter(substr(date,1,4)<investor[4],
                                  substr(date,1,4)>=(investor[4]-investor[6]))
        rttest <- rit %>% filter(substr(date,1,4)>=investor[4],
                                 substr(date,1,4)<(investor[4]+investor[5]))
                
        theta <- c(investor[1]+0.00001,rep((1-investor[1])/5,4))
        Sharpe <- function(theta) {
            theta2 <- c(theta,1-sum(theta))
            rpt <- as.matrix(rttrain[,-1]) %*% matrix(theta2,ncol=1)
            rp.bar <- sum(rpt)/(n)
            sp <- sd(rpt[1:n])
            Z <- (rp.bar - r) / sp
            return(Z)
        }
        #https://stackoverflow.com/questions/16345271/setting-constraints-in-constroptim
        constraints <- matrix(c(1, 0, 0, 0, 0,
                                -1, 0, 0, 0, 0,
                                0, 1, 0, 0, 0, 
                                0, -1, 0, 0, 0,
                                0, 0, 1, 0, 0, 
                                0, 0, -1, 0, 0,
                                0, 0, 0, 1, 0, 
                                0, 0, 0, -1, 0,
                                0, 0, 0, 0, 1, 
                                0, 0, 0, 0, -1, 
                                1, 1, 1, 1, 1, 
                                -1, -1, -1, -1, -1), ncol = 5, byrow = T)
            
        optim.result <- constrOptim(theta, Sharpe, NULL, 
                                    ui = constraints, ci = c(investor[1],-1,0,-1,0,-1,0,-1,0,-1,0,-1), control = list(fnscale = -1))
        
        sharpe.ratio <- round(optim.result$value,3)
        theta <- c(round(optim.result$par,3),round(1-sum(optim.result$par),3))
        
                
        rpt <- as.matrix(rttrain[,-1]) %*% matrix(theta,nrow=6)
                
        criterion.mean <- mean(rpt)
        criterion.sd <- sd(rpt)
        annualized.expected.mean <- mean(rpt)*12
        annualized.expected.sd <- sqrt(12)*sd(rpt)
                
        rpt.star <- as.matrix(rttest[,-1]) %*% matrix(theta,nrow=6)
                
        annualized.realized.mean <- mean(rpt.star)
        annualized.realized.sd <- sqrt(12)*sd(rpt.star)
                
        Vt <- V0 * cumprod(1 + rpt)
        Vt.star <- V0 * cumprod(1 + rpt.star)
                
        p <- ggplot(data=data.frame(Vt=c(Vt,Vt.star),t=c(1:length(Vt),1:length(Vt.star)),
                                    label=c(rep("Test data",length(Vt)),rep("Train data",length(Vt.star)))))+
            geom_line(aes(t,Vt,color=label))+
            theme_bw()+theme(legend.position=c(0.15,0.87),legend.text = element_text(size=20))+
            scale_color_manual(name=NULL,values=c(1,2))
                
        thetaoutput <- data.frame(Deposit=theta[1],SE=theta[2],KA=theta[3],
                                    KT=theta[4],GAS=theta[5],NAVER=theta[6],row.names="theta")
        criterionoutput <- data.frame(average_monthly_return=round(criterion.mean,4),monthly_sd=round(criterion.sd,4),
                                              Sharpe_Ratio=round(sharpe.ratio,4),row.names=NULL)
        validationoutput <- data.frame(expected_mean=round(c(annualized.expected.mean,annualized.expected.sd),4),
                                          realized_mean= round(c(annualized.realized.mean,annualized.realized.sd),4),
                                          row.names=c("annual average return","annual standard deviation"))
                
        list(criterion=criterionoutput,
             validation=validationoutput,
             theta=thetaoutput,p=p)
    } 
            
    result <- eventReactive(input$go, {
        portfolio2(SE(),KA(),KT(),GAS(),NAVER(),investor())})
    
    p <- eventReactive(input$go, {result()$p})
    thetas <- eventReactive(input$go, {result()$theta})
    criterion <- eventReactive(input$go, {result()$criterion})
    validation <- eventReactive(input$go, {result()$validation})
            
    output$plot <- renderPlot({ p() })
    output$thetas <- renderTable({ thetas() })
            
    output$criterion <- renderTable( {
        df <- structure(list(`criterion` = structure(1:3, .Label = c("monthly average return", "monthly standard deviation", "Sharpe Ratio"), class = "factor"), 
                             `Value` =criterion()[1,]), class = c("tbl_df", "tbl", "data.frame"), .Names = c("Criterion", "Value") , row.names = c(NA,  -3L))
    }
    , width = '100%'
    )
        
    output$validation <- renderTable( {
        df1 <- structure(list(`aa` = structure(1:2, .Label = c("annual average return", "annual standard deviation"), class = "factor"), 
                              `expected_mean` =validation()[1,],`realized_mean`=validation()[2,]), class = c("tbl_df", "tbl", "data.frame"), 
                         .Names = c("","expected mean", "realized mean") , row.names = c(NA,  -2L))
    }
    , width = '100%'
    )
            
    output$monthmean <- renderText({ criterion()[1,1] })
    output$monthsd <- renderText({criterion()[1,2]})
    output$Sharpe <- renderText({criterion()[1,3]})
    output$expectedyearmean <- renderText({validation()[1,1]})
    output$expectedyearsd <- renderText({validation()[2,1]})
    output$realyearmean <- renderText({validation()[1,2]})
    output$realyearsd <- renderText({validation()[2,2]})
            
        })




# Run the application 
shinyApp(ui = ui, server = server)
