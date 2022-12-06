library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(GGally)
library(ggplot2)
library(DMwR)
library(randomForest)
library(ROCR)
library(ROSE)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)
library(DiagrammeR)
library(MASS)
library(corrr)
library(gam)
library(mgcv)
library(survival)




ui <- dashboardPage(
  
    
    
    dashboardHeader(title = "기업부도확률 계산"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("변수", tabName = "Parameter", icon = icon("dashboard")),
            menuItem("결과", tabName = "Output", icon=icon("th"), badgeLabel = "new", badgeColor = "green")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "Parameter", h2("Parameter"),
                    fluidRow(
                        box(textInput("x1","총자산투자효율", 19)),
                        
                        box(textInput("x2","매출채권증가율", 2)),
                        box(textInput("x3","재고자산증가율", -33)),
                        
                        box(textInput("x4","경영자본순이익율", 0.5)),
                        box(textInput("x5","금융비용/총부채비율",4.9)),
                        box(textInput("x6","금융비용/총비용비율",5.5)),
                        box(textInput("x7","자기자본순이익율",3.9)),
                        box(textInput("x8","자본금순이익율",3.9)),
                        box(textInput("x10","총자본순이익률", 1.7)),
                        box(textInput("x12","총자산사업이익률", 3.6))
                    ),
                    
                    fluidRow(
                        box(textInput("x13","고정부채비율", 146.4)),
                        box(textInput("x14","고정비율", 186)),
                        box(textInput("x15","부채비율", 146.6)),
                        box(textInput("x16","부채총계/자산총계비율", 59.44)),
                        box(textInput("x19","유동비율", 3.48)),
                        box(textInput("x20","유보액/총자산비율", 0.67)),
                        box(textInput("x22","차입금의존도", 59.38)),
                        box(textInput("x23","고정자산/차입금비율",127.04)),
                        
                        box(textInput("x25","고정재무비보상배율", 1.6)),
                        box(textInput("x26","총차입금/(총차입금+자기자본)비율",59.4)),
                        box(textInput("x27","총CF/차입금비율", 5)),
                        box(textInput("x28","CF/차입금비율", -21.2))
                    ),
                    
                    fluidRow(
                        box(textInput("x29","순운전자본/총자본비율", 24.5)),
                        box(textInput("x30","유동부채구성비율", 0.07)),
                        box(textInput("x31","현금비율",8072.26)),
                        
                        box(textInput("x33","고정자산회전율", 0.62)),
                        box(textInput("x34","매입채무회전율율", 2218.64)),
                        box(textInput("x35","매출채권회전율", 45.58)),
                        box(textInput("x36","자기자본회전율", 1.19)),
                        box(textInput("x37","자본금회전율", 1.19)),
                        box(textInput("x38","재고자산회전율",16.92)),
                        box(textInput("x39","총자본회전율",0.5))
                    ),
                    
                    fluidRow(
                        box(textInput("x40","기업나이", "4673")),
                        box(selectInput("x41", label = h5("업종"),
                                        choices = list("경공업", "중공업", "건설업", "도소매", "서비스"),
                                        selected = "경공업")),
                        box(selectInput("x42", label = h5("규모"), 
                                        choices = list("외감", "비외감1", "비외감2", "소호", "개인"), 
                                        selected = "외감"))
                    ),
                    
                    fluidRow(
                        box(textInput("x43","로그매출액", 14.12)),
                        box(textInput("x44","로그자산", 14.87))
                    )
            ),
            
            tabItem(tabName = "Output", h2("Output"),
                    fluidRow(
                       box(title="기업정보", solidHeader = T, status = "danger",
                          tableOutput("table"),
                          width=10)),
                    fluidRow(
                      box(title = "분석방법", solidHeader = T, status = "primary", width = 3,
                          selectInput("method", "분석방법", choices = c("GLM", "GAM", "COX", "LDA", "KNN", "Random Forest", "SVM", "Xgboost"))),
                        
                      box(h5("예측결과", solidHeader = T, status = "primary", width = 7,
                          textOutput("text")))
                    )
            
        )    
        
    )
    
 )
 
) 


server <- shinyServer(function(input, output) {
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(tidyverse)
  library(GGally)
  library(ggplot2)
  library(DMwR)
  library(randomForest)
  library(ROCR)
  library(ROSE)
  library(caret)
  library(e1071)
  library(xgboost)
  library(Matrix)
  library(DiagrammeR)
  library(MASS)
  library(corrr)
  library(gam)
  library(mgcv)
  library(survival)
    
    
    options(warn=-1)
    data <- read_csv("C:/Users/yjk9/Documents/R/이통2/data_newnew.csv")
    train <- data[data$ID<=3168,]
    
    train_mat = data.matrix(train)
    train_mat %>% dim()
    train_mat[,35] = as.factor(train$x41)
    train_mat[,36] = as.factor(train$x42)
    
    var = names(data)[2:39]
    
    scaling <- read_csv("C:/Users/yjk9/Documents/R/이통2/scale.csv")[,-1]
    transform <- function(data){
      data = data %>% dplyr::mutate(x1 = (ifelse(x1>100, 100, x1)),
                                    x1 = sign(x1) * abs(x1) ^(1/3),
                                    x2 = (ifelse(x2 > 450000000, 450000000, x2)),
                                    x2 = sign(x2) * abs(x2) ^(1/7),
                                    x3 = ifelse(x3 > 10000000, sort(x3, decreasing=T)[124], x3),
                                    x3 = sign(x3)*abs(x3)^(1/7),
                                    x4 = ifelse(x4 < -200, -200, x4), 
                                    x4 = ifelse(x4 > 200, 200, x4),
                                    x4 = ifelse(x4 > 200, 200, x4),
                                    x4 = sign(x4)*abs(x4)^(1/3),
                                    x5 = log(x5), 
                                    x5 = ifelse(x5 < -4, -4, x5), 
                                    x6 = log(x6), 
                                    x6 = ifelse(x6 <= -5, -5, x6),
                                    x7 = ifelse(x7 < -2000, -2000, x7), 
                                    x7 = ifelse(x7 > 2000, 2000, x7),
                                    x7 = sign(x7)*abs(x7)^(1/3),
                                    x8 = ifelse(x8 > 20000, sort(x8, decreasing = T)[18], x8),
                                    x8 = sign(x8)*abs(x8)^(1/3),
                                    x10 = ifelse(x10 > 200, 200, x10),
                                    x10 = sign(x10)*abs(x10)^(1/3),
                                    x12 = sign(x12)*abs(x12)^(1/3),
                                    x13= ifelse(x13 > 10000,9206.752,x13),
                                    x13= log(x13+0.01),
                                    x14= ifelse(x14 > 100000,47227.27,x14),
                                    x14= log(x14+0.01),
                                    x15= ifelse(x15 > 400000,118446.28,x15),
                                    x15= log(x15 + 0.01),
                                    x16= ifelse(x16 > 900,798.56,x16),
                                    x16= sqrt(x16),
                                    x19= ifelse(x19 > 100000,98714.29,x19),
                                    x19= log(x19 + 0.01),
                                    x20= ifelse(x20 < -100,-99.977,x20),
                                    x20 = ifelse(x20 < -200,min(x20[x20 >= -200]),x20),
                                    x22 = sqrt(ifelse(x22 > 300, max(x22[x22 <= 300]),x22)),
                                    x23 =  ifelse(x23 > 200000, max(x23[x23 <= 200000]),x23),
                                    x23 = log(ifelse(x23==0,0.01,x23)),
                                    x25 = ifelse(x25 > 400, max(x25[x25 <= 400]),x25),
                                    x25 = sign(x25)*abs(x25)^(1/3),
                                    x26 = sqrt(ifelse(x26 > 600, max(x26[x26 <= 600]),x26)),
                                    x27 = ifelse(x27 > 10000, max(x27[x27 <= 10000]),x27),
                                    x27 = (ifelse(x27 < -600, max(x27[x27 >= -600]),x27))^(1/3),
                                    x28 = ifelse(x28 > 30000, max(x28[x28<=30000]),x28),
                                    x28 = sign(x28)*abs(x28)^(1/3),
                                    x29 = ifelse(x29 > 10000, max(x29[x29<=10000]),x29),
                                    x29 = sign(x29)*abs(x29)^(1/3),
                                    x30 = log(ifelse(x30==0,0.0008,x30)),
                                    x31= ifelse(x31>200,200,x31),
                                    x33= ifelse(x33>50,50,x33),
                                    x34= ifelse(x34>5000,5000,data$x34),
                                    x34= log(x34),
                                    x35= ifelse(x35>5000,5000,data$x35),
                                    x35= log(x35+0.01),
                                    x36= ifelse(x36>500,500,data$x36),
                                    x36= log(data$x36 + 0.01),
                                    x37= ifelse(x37>500,500,data$x37),
                                    x37= sqrt(x37),
                                    x38= ifelse(x38>2000,2000,data$x38), 
                                    x38= log(x38),
                                    x39= log(data$x39),
                                    x40 = log(data$x40)
      )
      for(i in 1:length(colnames(scaling))){
        col = colnames(scaling)[i]
        data[,col] = scale(data[,col],scaling[1,col],scaling[2,col])
      }
      return(data)
    }
    
    ## glm  ##
    glm.probit <- glm(formula = delta ~ x2 + x4 + x5 + x10 + x16 + x20 + x27 + 
                          x29 + x30 + x31 + x40 + x42 + x44 + x1 + x2:x29 + x5:x16 + 
                          x5:x27 + x5:x29 + x5:x31 + x10:x29 + x10:x42 + x16:x20 + 
                          x16:x40 + x20:x30 + x20:x42 + x27:x31 + x27:x40 + x29:x31 + 
                          x30:x40 + x40:x42 + x40:x44 + x42:x44 + x20:x1 + x40:x1 + 
                          x44:x1, family = binomial(link = probit), data = train)
    
    
    ## gam ##
    gam.probit <- gam(delta ~ x1 + s(x2) + s(x4) + s(x5) + s(x10) + x13 + x22 + s(x23) + 
                          s(x25) + s(x28) + s(x30) + x31 + x33 + s(x39) + s(x40) +s(x43),
                      family = binomial(link=probit) , data = train)
    
    
    
    ## cox ##
    cox.ph <- coxph(formula = Surv((exp(x40) + y)/365, delta) ~ x2 + x4 + x5 + 
                      x10 + x16 + x20 + x27 + x30 + x31 + x42 + x44 + x22 + x6 +
                      x7 + x14 + x19 + x23 + x33 + x41 + x43 + x1 + x8 + x13 + 
                      x15 + x28 + x25 + x34 + x36 + x37 + x39 + x29 + x12 + x5:x16 + 
                      x5:x27 + x10:x30 + x10:x31 + x42:x44 + x23:x1 + x4:x23 + 
                      x14:x23 + x23:x15 + x8:x28 + x14:x28 + x15:x28 + x19:x28 + 
                      x28:x25 + x30:x28 + x33:x28 + x33:x13 + x13:x36 + x5:x14 + 
                      x5:x23 + x5:x36 + x5:x37 + x5:x39 + x5:x41 + x5:x43 + x2:x29 + 
                      x7:x29 + x27:x1 + x27:x6 + x10:x27 + x27:x12 + x27:x25 + 
                      x27:x30 + x27:x34 + x27:x43 + x27:x44, data = train)
    
    
    train$delta = 1
    train$y = 365
    
    
    
    ## LDA ##
    #lda <- lda(formula=as.factor(delta)~., data=train[,-c(1,40)])
    
    
    ## KNN ##
    knn <- knn3(as.factor(delta)~., data=train[,-c(1,40)], k=5)
    
    ## rf ##    
    rf <- randomForest(factor(delta)~., data=train_mat[,var], mtry= floor(sqrt(length(var))),
                       importance=T)
    
    
    ## svm ##
    svm <- svm(factor(delta)~., data=train[,var], probability=T)
    
    
    ## XGB ##
    #dtrain = xgb.DMatrix(data=train_mat[,2:38],label =train_mat[,39])
    #bst <- xgboost(data = dtrain, max.depth = 32, 
                   #eta = 1, nthread = 2, nrounds = 3, objective = "binary:logistic")
    
    
    # sample
    
    sample = reactive({
         
        tibble(x1= as.numeric(input$x1),
               x2 = as.numeric(input$x2),
               x3 = as.numeric(input$x3),
               x4 = as.numeric(input$x4),
               x5 = as.numeric(input$x5),
               x6 = as.numeric(input$x6),
               x7 = as.numeric(input$x7), 
               x8 = as.numeric(input$x8),
               x10 = as.numeric(input$x10),
               x12 = as.numeric(input$x12),
               x13= as.numeric(input$x13),
               x14= as.numeric(input$x14),
               x15= as.numeric(input$x15),
               x16= as.numeric(input$x16),
               x19= as.numeric(input$x19),
               x20= as.numeric(input$x20),
               x22 = as.numeric(input$x22),
               x23 = as.numeric(input$x23),
               x25 = as.numeric(input$x25),
               x26 = as.numeric(input$x26),
               x27 = as.numeric(input$x27),
               x28 = as.numeric(input$x28),
               x29 = as.numeric(input$x29),
               x30 = as.numeric(input$x30),
               x31 = as.numeric(input$x31),
               x33 = as.numeric(input$x33),
               x34 = as.numeric(input$x34),
               x35 = as.numeric(input$x35),
               x36 = as.numeric(input$x36),
               x37 = as.numeric(input$x37),
               x38 = as.numeric(input$x38), 
               x39 = as.numeric(input$x39),
               x40 = as.numeric(input$x40),
               x43 = as.numeric(input$x43),
               x44 = as.numeric(input$x44),
               x41 = as.factor(as.character(input$x41)),
               x42 = as.factor(as.character(input$x42)))
               
  })
    
   
    
    res1= reactive({round(predict(glm.probit, transform(sample()),type="response")*100,2)})
    res2 = reactive({round(predict(gam.probit,transform(sample()),type="response")*100,2)})
    
    res3 = reactive({
       ifelse(predict(cox.ph, type="expected",newdata=transform(trans1(sample())) >1, 100,
                     round(predict(cox.ph,type="expected",newdata=transform(sample())),3)*100))})
    res4 = reactive({round(predict(lda, transform(sample()),type="prob")$posterior[,2],4)*100})
    res5 = reactive({round(predict(knn,transform(sample()),type="prob")[,2],4)*100})
    res6 = reactive({round(predict(rf, transform(sample()),type="prob")[,2])})
    res7 = reactive({round(attr(predict(svm,transform(sample()), probability=T),'probabilities'),4)})
    #res8 = reactive({round(predict(bst, xgb.DMatrix(data=transform(sample()), label=1)),4)*100})
    
    res = reactive({
      if (input$method == "GLM"){res1()}
        else if(input$method == "GAM") {res2()}
        else if(input$method == "COX") {res3()}
        else if(input$method == "KNN") {res4()}
        else if(input$method == "KNN") {res5()}
        else if(input$method == "Random Forest") {res6()}
        else if(input$method == "SVM") {res7()}
        
    })
    
    
    ## 고객정보 ##
    output$table = renderTable({
      tibble("총자산투자효율"=input$x1, 
             "매출채권증가율"=input$x2, 
             "재고자산증가율"=input$x3,
             "경영자본순이익율"=input$x4, 
             "금융비용/총부채비율"=input$x5, 
             "금융비용/총비용비율"=input$x6,
             "자기자본순이익율"=input$x7,
             "자본금순이익율"=input$x8, 
             "총자본순이익률"=input$x10, 
             "총자산사업이익률"=input$x12,
             "고정부채비율"=input$x13, 
             "고정비율"=input$x14, 
             "부채비율"=input$x15, 
             "부채총계/자산총계비율"=input$x16,
             "유동비율" = input$x19, 
             "유보액/총자산비율"= input$x20, 
             "차입금의존도" = input$x22, 
             "고정자산/차입금비율"=input$x23, 
             "고정재무비보상배율"=input$x25, 
             "총차입금/(총차입금+자기자본)비율"=input$x26, 
             "총CF/차입금비율"=input$x27, 
             "CF/차입금비율"=input$x28, 
             "순운전자본/총자본비율"=input$x29, 
             "유동부채구성비율"=input$x30, 
             "현금비율"=input$x31,
             "고정자산회전율"=input$x33, 
             "매입채무회전율율"=input$x34, 
             "매출채권회전율"=input$x35, 
             "자기자본회전율"=input$x36,
             "자본금회전율"=input$x37, 
             "재고자산회전율"=input$x38, 
             "총자본회전율"=input$x39, 
             "기업나이"=as.character(input$x40),
             "업종"=as.character(input$x41), 
             "규모"=as.character(input$x42), 
             "로그매출액"=input$x43, 
             "로그자산" = input$x44)})
    
    output$table2 = renderTable({
      tibble(x1= as.numeric(input$x1),
             x2 = as.numeric(input$x2),
             x3 = as.numeric(input$x3),
             x4 = as.numeric(input$x4),
             x5 = as.numeric(input$x5),
             x6 = as.numeric(input$x6),
             x7 = as.numeric(input$x7), 
             x8 = as.numeric(input$x8),
             x10 = as.numeric(input$x10),
             x12 = as.numeric(input$x12),
             x13= as.numeric(input$x13),
             x14= as.numeric(input$x14),
             x15= as.numeric(input$x15),
             x16= as.numeric(input$x16),
             x19= as.numeric(input$x19),
             x20= as.numeric(input$x20),
             x22 = as.numeric(input$x22),
             x23 = as.numeric(input$x23),
             x25 = as.numeric(input$x25),
             x26 = as.numeric(input$x26),
             x27 = as.numeric(input$x27),
             x28 = as.numeric(input$x28),
             x29 = as.numeric(input$x29),
             x30 = as.numeric(input$x30),
             x31 = as.numeric(input$x31),
             x33 = as.numeric(input$x33),
             x34 = as.numeric(input$x34),
             x35 = as.numeric(input$x35),
             x36 = as.numeric(input$x36),
             x37 = as.numeric(input$x37),
             x38 = as.numeric(input$x38), 
             x39 = as.numeric(input$x39),
             x40 = as.numeric(input$x40),
             x43 = as.numeric(input$x43),
             x44 = as.numeric(input$x44),
             x41 = as.factor(as.character(input$x41)),
             x42 = as.factor(as.character(input$x42)))
      
    })
    
    
    ## 예측결과 ##
    output$text = renderText(c("해당 기업이 1년 이내에 부도가 날 확률은", paste(res()), "입니다."))
    
})




# Run the application 
shinyApp(ui,server)

