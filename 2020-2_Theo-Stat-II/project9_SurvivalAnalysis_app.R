#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

    
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
    


ui = dashboardPage( 
        
    #Header
    dashboardHeader(
        title = "Probability of Heart Disease",
        titleWidth = 450
    ),
        
    dashboardSidebar( width = 0),
    
      
    dashboardBody(
            
        shinyDashboardThemes(
            theme = "purple_gradient"
        ),
            
       
        fluidRow(
            box(title = "Information", solidHeader = T, status = "danger",
                textInput("name","your name"),
                radioButtons("sex","sex",choiceValues = c(1,0), choiceNames = c("Female","Male"),inline=T),
                numericInput("age","age",value=45,min=30, max=70),
                numericInput("sbp","systolic blood pressure (mmHg)",min=80,max=270,value=132),
                numericInput("dbp","diastolic blood pressure (mmHg)",min=40,max=150,value=80),
                numericInput("scl","serum cholesterol (mg/100ml)",min=115,max=568,value=228),
                numericInput("bmi","BMI",min=16,max=40, value= 25),
                width = 6),
                
            box(title = "Instruction", solidHeader = T, status = "danger",
                textOutput("value"), width = 6)
            ),
                
        
       
        fluidRow(
            box(title = "Heart Risk(Cox PHM)", solidHeader = T, status = "danger",
                tableOutput("text1"),
                gaugeOutput("gauge1") ,width=6),
            box(title = "Heart Risk(ALT)", solidHeader = T, status = "danger",
                tableOutput("text2"),
                gaugeOutput("gauge2") ,width=6)
                
        )
            
    )
)
    
server = function(input, output) { 
    options(warn=-1)
    Sys.setlocale("LC_ALL", "korean")

    #data preprocessing and modelling 
    normalize <- function(x){
        return((x- min(x)) /(max(x)-min(x)))
    }
        
    data = read.csv("C:/Users/yjk9/Documents/R/theo_stat/hw9/data.csv")
    data = data[,-1]
    data = data[complete.cases(data),]
    data$bmi = ifelse(data$bmi>40, 40, data$bmi)
    data.mm = data
    data.mm$sbp = normalize(data$sbp)
    data.mm$dbp = normalize(data$dbp)
    data.mm$scl = normalize(data$scl)
    data.mm$bmi = normalize(log(data$bmi))
      
    data.mm$ages = ifelse(data.mm$age<40 ,30, 
                          ifelse(data.mm$age<50,40,
                                 ifelse(data.mm$age<60,50,
                                        ifelse(data.mm$age<70,60,data.mm$age))))
    data.mm$ages2 = (ifelse(data.mm$age>=45 , 2, 1))
    data.mm$age = normalize(data$age)
        
    data.mm = data.mm %>% tibble() %>% mutate(month = as.factor(month),
                                              sex= as.factor(sex),
                                              ages = as.factor(ages),
                                              ages2 = as.factor(ages2))
        
    max.value = apply(data,MARGIN = 2,max)
    min.value = apply(data,MARGIN = 2,min)
        
    #COX PHM best model AIC 22737
    cox0 = coxph(formula = Surv(followup, chdfate) ~ sbp + dbp + scl + age + 
                     bmi + sex + ages + ages2 + sbp:scl + sbp:age + sbp:ages + 
                     dbp:scl + dbp:ages2 + scl:ages + age:bmi + age:sex +  
                     bmi:ages2, data = data.mm)   
    #ALT best model
    reg3 <- survreg(Surv(followup, chdfate) ~ sbp + 
                        age + bmi + sex + ages + sbp:dbp + sbp:scl + sbp:age + sbp:ages + 
                        dbp:scl + ages:scl + ages:bmi + age:sex, data = data.mm)
    
        
 
    sample = reactive({
        tibble(age = as.numeric(input$age),
               sex = as.factor(input$sex),
               sbp = as.numeric(input$sbp),
               dbp = as.numeric(input$dbp),
               scl = as.numeric(input$scl),
               bmi = as.numeric(input$bmi))
    })
        
    trans = function(sample){
          sample = sample%>% mutate( bmi = ifelse(sample$bmi > 40, 40, sample$bmi))
          sample = sample %>% mutate( sbp = (sbp-min.value[1])/(max.value[1]-min.value[1]),
                                      dbp = (dbp-min.value[2])/(max.value[2]-min.value[2]),
                                      scl = (scl-min.value[3])/(max.value[3]-min.value[3]),
                                      bmi = (bmi-min.value[5])/(max.value[5]-min.value[5]),
                                      ages = as.factor(ifelse(age<40,30,
                                                              ifelse(age<50,40,
                                                              ifelse(age<60,50,60)))),
                                      ages2 = as.factor(ifelse(age >=45,2,1)),
                                      followup=3650, chdfate=1) %>%
            mutate(age = (age-min.value[4])/(max.value[4]-min.value[4]))
    }
    
    #sample = data[1,] %>% select(sbp,dbp,scl,age,bmi,sex) %>% mutate(sex=as.factor(sex))
    #res1 = (round((1-predict(cox0, type="survival",newdata=trans(sample)) )*100,4))
    #res2 = round(predict(reg3,newdata=trans(sample))*100,4)
    res1 = reactive(round((1-predict(cox0, type="survival",newdata=trans(sample())) )*100,1))
    res2 = reactive(round(( pgumbel(log(3650)-predict(reg3,newdata=trans(sample()),type='lp'),0,1) )*120,1))
        
    #res = reactive({
        #if (input$method == "Cox PHM") {res1()}
        #else if(input$method == "ALT") {res2()}
    #})
        
    output$text1 = renderText(c(input$name, paste(res1()), "%"))
    output$gauge1 = renderGauge({gauge(res1(),min=0,max=100)})
    output$text2 = renderText(c(input$name,paste(res2()), "%"))
    output$gauge2 = renderGauge({gauge(res2(),min=0,max=100)})
    output$value = renderText({
        paste("This app is designed to predict the probability of heart disease based on age, sex, bmi, systolic blood pressure, diastolic blood pressure and serum cholesterol within 10 years.")
    })
    }



# Run the application 
shinyApp(ui = ui, server = server)
