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

ui <- dashboardPage(
    
    #Header
    dashboardHeader(title = "Life Insurance Calculation"),
    
    #sidebar
    dashboardSidebar(width = 300,
                     sidebarMenu(
                         menuItem("Premium", tabName = "dashboard", icon = icon("dashboard"))
                     )),
    
    #body
    dashboardBody(
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #f4b943;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #f4b943;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #f4b943;
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #f4b943;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ff0000;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ff69b4;
                                }
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }
                                '))),
        
        fluidRow(
            box(title = "Your information", background = "teal", solidHeader = TRUE, width =6,
                checkboxGroupInput(inputId = 'sex',label = h6('성별'), choices = list('man' = 'man','woman' = 'woman'), selected=2),
                numericInput(inputId = 'age', label = h6('나이'), value=30)),
            
            box(h2(strong('Calculated Premium')), background = "maroon", solidHeader = TRUE, width=6,
                gaugeOutput("gauge1"))),
        
        fluidRow(
            box(title = "Product information", background = "teal", solidHeader = TRUE, width =6,
                numericInput(inputId = 'irate', label = h6('연이자율(%)'), value=0.03),
                numericInput(inputId = 'n', label=h6('납입만기(년)'), value=20),
                numericInput(inputId = 'm', label=h6('지급개시 시점(=은퇴시점)'), value=30),
                numericInput(inputId = 'b', label=h6('지급만기 m년 이전 사망 시 지급 보험금(만원)'), value=5000),
                actionButton("go", label = "Go!")),
            
            box(h2(strong('Result of premium')),background = "maroon", solidHeader = TRUE, width=6,
                tableOutput('contents'))
        )
        
    ))
        
    
   
server <- function(input, output){
    dat  = read.csv("C:/Users/yjk9/Documents/R/theo_stat/hw3_shiny1/life_graph.csv")
    premium <- function(sex, age, irate, n, m, b){
        r = log(1+irate)
        if (sex=='man'){
            lx = dat$lx
        }else if(sex=='woman'){
            lx = dat$lx.1
        }
        
        lxa_list = lxA_list = c()
        
        for (j in 1:(n-1)){
            lxa_list[j] = lx[age+j] / lx[j] * exp(-r*j)
        }
        ax = 1/2 + sum(lxa_list) + 1/2*lx[age+n]/lx[age]*exp(-r*n) 
        
        
        for (j in 1:m){
            lxA_list[j] = lx[age+j]/lx[age] * exp(-r*j)
        }
        Ax = 1 - r * (1/2 + sum(lxA_list) + (lx[age+m] / lx[age])*exp(-r*(m+1))/(1-exp(-r)))
        p = b * Ax / ax
        
        return(p/12)
    }
    
    pre <- reactive({
        premium(input$sex, as.numeric(input$age), as.numeric(input$irate), as.numeric(input$n),
                as.numeric(input$m), as.numeric(input$b))
    })
    
    text <- eventReactive(input$go, {
        return(paste("고객님의 적정 월납 보험료는", round(pre(), 2), "만원 입니다."))
    })
    
    output$gauge1 = renderGauge({gauge(round(pre(),2),min=0,max=100)})
    output$contents <- renderText({
        text()
    })
}



shinyApp(ui = ui, server = server)

