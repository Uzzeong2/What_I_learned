
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(gam)
library(mgcv)
library(survival)
library(randomForest)
library(ROCR)
library(ROSE)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)
library(DiagrammeR)
library(rsconnect)
library(lubridate)


# Define UI for application that draws a histogram
Sys.setlocale("LC_ALL", "korean")

ui = dashboardPage(
    #Header    
    dashboardHeader(title = "생명 보험 해지 확률 계산", titleWidth = 300),
    
    #Sidebar
    dashboardSidebar(width = 300,
                     textInput("name","가입자 이름"),
                     numericInput("age","가입 연령(세)",20),
                     numericInput("payment","보험료(단위: 원)",10000),
                     radioButtons("cycle","납입 주기",
                                  choiceNames = c("1개월","3개월","6개월","12개월"),
                                  choiceValues = c(1,2,3,4),
                                  inline = T),
                     sliderInput("pd_y","납입기간 (연)",min=0,max=100,value=20),
                     sliderInput("number","최종 납입 횟수",min=0,max=200,value=100),
                     radioButtons("revival","부활 유무",
                                  choiceNames = c("한번 이상","없음"),
                                  choiceValues = c(1,0),
                                  inline = T),
                     dateInput("p_date","계약 일자",format = "yyyy-mm-dd",
                               value = "2001-03-03"),
                     dateInput("expiration","지급 만기 일자",format = "yyyy-mm-dd",
                               value = "2010-03-03"),
                     selectInput("type_m","상품 분류",
                                 choices=c("암","유병자","신생아","의료실비","정기/종신")),
                     selectInput("type_s","상세 분류",
                                 choices = c("갱신형","비갱신형","치매/간병",
                                             "정기","종신","성인","아동","유아","기타")),
                     selectInput("t_method","수금 방법",
                                 choices = c("계좌이체","휴대폰결제","지로납부","무통장입금","카드자동납부"))
    ),
    
    #Body
    dashboardBody(
        tags$head(tags$style(HTML(
            '.main-header .logo {font-size : 28px;}',
            '.main-sidebar {font-size : 16px;}',
            '.box {font-size : 17px;}'
        ))),
        
        fluidRow(
            box(title = "고객정보", solidHeader = T, status = "danger",
                tableOutput("table"),
                width=10)),
        fluidRow(
            box(title = "분석방법", solidHeader = T, status = "primary", width = 3,
                selectInput("method", "분석 방법",
                            choices=c("GLM","GAM","CoxPHM","LDA","KNN","Random Forest","SVM","XGB"))),
            box(title = "예측 결과", solidHeader = T, status = "primary", width = 7,
                textOutput("text"))
        ),
        fluidRow(title = "다른 방법 이용 시 예측 결과"
                 
        )
    ))
server = function(input, output){
    
    library(lubridate)
    library(shiny)
    library(shinydashboard)
    library(DT)
    library(tidyverse)
    library(gam)
    library(mgcv)
    library(survival)
    library(randomForest)
    library(ROCR)
    library(ROSE)
    library(caret)
    library(e1071)
    library(xgboost)
    library(Matrix)
    library(DiagrammeR)
    library(rsconnect)
    
    options(warn=-1)
    train1= read_csv("train1.csv")
    train1 =  train1 %>% dplyr::mutate(cycle = as.factor(cycle),
                                       t_method=as.factor(t_method),
                                       payment=log(payment),
                                       revival=as.factor(revival),
                                       type_m=as.factor(type_m),
                                       type_s=as.factor(type_s),
                                       no_exp=as.factor(no_exp),
                                       pay_end=as.factor(pay_end),
                                       paidratio1 = as.factor(ifelse(paid_ratio==1,1,0)))
    
    trans= function(sample){
        
        sample = sample %>% 
            dplyr::mutate(expiration=ifelse(substr(p_date,6,8)==229 & (expiration %in% c(20180229, 20150229, 20250229, 20260229, 20110229, 20210229, 99990229)),expiration-1,expiration)) %>%
            dplyr::mutate(p_date=ymd(p_date),expiration = ymd(expiration),p_date2=p_date,expiration2 = expiration)
        
        #날짜 sample %>% mutate(p_date=ymd(p_date),expiration = ymd(expiration))
        
        sample = sample %>% separate(p_date2, into=c("p_date_y","mm","dd")) %>%
            separate(expiration2, into=c("expiration_y","mm2","dd2")) %>% dplyr::mutate(p_date_y=as.numeric(p_date_y),
                                                                                        expiration_y=as.numeric(expiration_y),
                                                                                        mm=as.numeric(mm),
                                                                                        dd=as.numeric(dd),
                                                                                        timedif = expiration-p_date)
        
        sample$cycle = ifelse(sample$cycle==1,1,ifelse(sample$cycle==2,3,ifelse(sample$cycle==3,6,12)))
        
        
        sample = sample %>% dplyr::mutate(cycle_yr = ifelse(cycle==1, 12, ifelse(cycle ==3, 4, ifelse(cycle==6,2, 1))))
        
        # 무만기 여부
        sample$no_exp <- as.factor(ifelse(year(sample$expiration)==9999,1,0))
        
        # 만기 연도 보정
        sample <- sample %>% dplyr::mutate(expiration_y=ifelse(expiration_y==9999,(80-age)+p_date_y,expiration_y)) %>%
            dplyr::mutate(expiration=ymd(paste0(expiration_y, mm2, dd2)))
        
        # 파생변수
        # 나이 그룹화
        sample$age_L <- as.numeric(substr(sample$age,1,1))
        
        # 납입 완료 시점
        year <- sample$p_date_y+sample$pd_y
        sample$pay_expiration <- ifelse(paste0(as.character(year),sample$mm2,sample$dd2)=="20100229",'20100228',paste0(as.character(year),sample$mm2,sample$dd2))
        sample$pay_expiration <- ymd(ifelse(sample$pay_expiration=='20100229','20100228',sample$pay_expiration))
        
        # 납입 기한을 채운 사람
        sample$pay_end <- ifelse(sample$pay_expiration<=as_date('2001-09-30'),1,0)
        
        
        # 최종 계약 일수(총 가입 일수)
        sample$timedif <- as.numeric(sample$expiration-sample$p_date)
        
        # 가입일수 - 계약일로부터 자료시점까지의 기간, 만료일이 9월 30일 이전이면 만료일 기준으로
        sample$join_days <- as.numeric(ifelse(sample$expiration<=as_date('2001-09-30'),sample$expiration-sample$p_date,as_date('2001-09-30')-sample$p_date))
        # 가입 개월 수
        sample$join_months <- round(sample$join_days/30.5)
        
        # 보험금지급까지 남은 일수,남은 계약 일수
        sample$left_end_days <- sample$timedif-sample$join_days
        
        # 최종 납입까지의 남은 일수
        sample$left_pay_days <- as.numeric(ifelse(sample$pay_end==1,0,sample$pay_expiration-as_date('2001-09-30')))
        
        # 전체 계약기간에서 join한 비율 - 낸 일수/총 계약일수
        sample$join_ratio <- as.numeric(sample$join_days)/sample$timedif
        
        # 납입 기한 내에서 join한 비율(납입 기한을 다 채운 경우 1로 봄)
        sample$pay_join_ratio <- ifelse(sample$pay_end==1,1,sample$join_days/(sample$pd_y*365))
        
        # 남은 일수 비율 - 남은 계약 일수/가입한 일수
        sample$left_ratio <- as.numeric(sample$left_end_days)/sample$timedif
        
        # 납입 완료까지 총 지급해야 하는 횟수 중에 지금까지 지급한 비율
        sample$pay_ratio <- sample$number/(sample$cycle_yr*sample$pd_y)
        sample$pay_ratio <- ifelse(sample$pay_ratio>1,1,sample$pay_ratio)
        
        # 지금까지 총 지급해야하는 횟수 중에 지금까지 지급한 비율
        sample$paid_ratio <- ifelse(sample$pay_expiration<=as_date('2001-09-30'),sample$number/(sample$pd_y*sample$cycle_yr),
                                    sample$number/(floor(sample$join_days/30.5/as.numeric(sample$cycle)+1)))
        sample$paid_ratio <- ifelse(sample$paid_ratio>1,1,sample$paid_ratio)
        
        # 총 납입 보험료
        sample$total_payment <- sample$payment * sample$number
        
        # 최종 납입 기간
        # 납입 월: paid_months 납입 횟수 * 납입 주기 (1,3,6,12개월 단위) 
        # 미납 개월 수 unpaid_months = 가입 기간(월단위) - 납입 월
        
        
        rest = ifelse(9-sample$mm>=0,9-sample$mm,21-sample$mm )
        mod = rest%%as.numeric(as.character(sample$cycle))
        
        
        sample$mod = mod
        
        sample = sample %>% dplyr::mutate( paid_months = ifelse(mod!=0,as.numeric(as.character(cycle))*(number-1)+mod,as.numeric(as.character(cycle))*(number-1)+as.numeric(as.character(cycle)))) %>%
            dplyr::mutate(unpaid_months = join_months-paid_months)
        sample$unpaid_months = ifelse(sample$unpaid_months<0,0,sample$unpaid_months)  
        
        sample = sample %>% dplyr::mutate(unpaid_ratio = unpaid_months/join_months,index=1)
        join_months = sample$join_months
        # 분석용 sample
        sample = sample %>% dplyr::select(-mm2,-dd,-dd2,-mod,-index)
        sample = sample %>% dplyr::mutate(t_method=as.factor(t_method),
                                          payment=log(payment),
                                          cycle = as.factor(input$cycle),
                                          revival=as.factor(revival),
                                          type_m=as.factor(type_m),
                                          type_s=as.factor(type_s),
                                          mm=as.factor(mm),
                                          no_exp=as.factor(no_exp),
                                          age_L=as.factor(age_L),
                                          pay_end=as.factor(pay_end),
                                          join_days=log(join_days),
                                          join_months=log(join_months),
                                          left_pay_days=ifelse(left_pay_days==0,log(1),log(left_pay_days)),
                                          join_ratio=ifelse(join_ratio==0,log(0.001),log(join_ratio)),
                                          pay_join_ratio=log(pay_join_ratio),
                                          left_ratio=ifelse(left_ratio==0,log(0.001),left_ratio),
                                          pay_ratio=log(pay_ratio),
                                          total_payment=log(total_payment),
                                          paid_months=log(paid_months),
                                          unpaid_months=ifelse(unpaid_months==0,log(0.001),log(unpaid_months)),
                                          unpaid_ratio=ifelse(unpaid_ratio==0,log(0.001),log(unpaid_ratio)),
                                          payment_per=log(payment/as.numeric(cycle)),
                                          paidratio1 = as.factor(ifelse(paid_ratio==1,1,0)),
                                          payment_per=log(payment/as.numeric(cycle))) %>%
            dplyr::select(-c(p_date, expiration,pay_expiration))
        
    }
    
    #GLM
    glm1 <- glm(formula = y ~ paid_ratio + timedif + payment_per + age + 
                    cycle + pay_join_ratio + pay_ratio + join_days + left_end_days + 
                    payment + paid_months + total_payment + unpaid_months + number + 
                    timedif:join_days + total_payment:unpaid_months+unpaid_ratio, family = binomial(link = probit), 
                data = train1)
    gam1 <- gam(y ~age + cycle + payment + number +  
                    timedif + no_exp + pay_end + 
                    join_days + left_end_days + 
                    pay_join_ratio  + pay_ratio + paid_ratio +
                    total_payment + s(paid_months) + s(unpaid_months) + s(unpaid_ratio) +
                    payment_per , 
                family = binomial(link=probit) , data = train1)
    
    train1$y <- as.numeric(as.character(train1$y))
    
    #Coxph
    sur1 <- coxph(Surv(join_months, y) ~ age + payment + timedif + join_days + 
                      left_end_days + pay_join_ratio + pay_ratio + paid_ratio + total_payment +
                      unpaid_ratio + payment_per + paid_months + unpaid_months +
                      unpaid_ratio:paid_months + unpaid_ratio:unpaid_months + 
                      paid_months:unpaid_months,
                  data=train1)
    
    #LDA
    lda_var = c('y', 'age', 'cycle', 'pd_y', 't_method', 'payment',
                'timedif', 'unpaid_ratio', 'no_exp', 'pay_end', 'join_days', 'left_ratio') ### 변수 리스트
    library(MASS)
    lda1 <- lda(formula=y~., data=train1[,lda_var])
    
    #knn
    knn_var = c('y', 'age', 'cycle', 'pd_y', 't_method', 'payment', 'revival',
                'timedif', 'paid_ratio', 'no_exp', 'pay_end', 'join_days', 'left_ratio') ### 변수 리스트
    knn1 <- knn3(y~., data=train1[,knn_var], k=25)
    
    #random forest
    train2 = train1
    train2$y = as.factor(train2$y)
    rf_var = c('y', 'age', 'cycle', 'pd_y', 't_method', 'payment', 'revival',
               'timedif', 'paid_ratio', 'no_exp', 'pay_end', 'join_days', 'left_ratio') ### 
    rf1 <- randomForest(y~., data=train2[,rf_var], mtry=floor(sqrt((length(rf_var)-1))), importance=T)
    
    #SVM
    svm_var = c('y', 'age', 'cycle', 'pd_y', 't_method', 'payment', 'revival',
                'timedif', 'unpaid_ratio', 'no_exp', 'pay_end', 'join_days', 'left_ratio') ### 변수 리스트
    svm <- svm(y~., data=train2[,svm_var], probability=T)
    
    #XGB
    
    xg_var = c('y', 'age', 'cycle', 'pd_y', 't_method', 'payment', 'revival',
               'timedif', 'unpaid_ratio', 'no_exp', 'pay_end', 'join_days', 'left_ratio') ### 변수 리스트
    
    x_train = sparse.model.matrix(y~., data=train2[,xg_var])[,-1];
    y_train = as.numeric(train2$y)-1
    xgb_train <- xgb.DMatrix(data=as.matrix(x_train), label=y_train)
    xgb <- xgboost(data=xgb_train, max.depth=2, eta=1, nthread=2, nrounds=2, objective='binary:logistic')
    
    
    #input values for prediction
    
    #        })
    sample = reactive({
        tibble(age=as.numeric(input$age), 
               cycle = as.numeric(input$cycle), 
               pd_y = as.numeric(input$pd_y),
               t_method = as.numeric(sort(order(input$type_m))), 
               payment = as.numeric(input$payment),
               revival = as.numeric(input$revival), 
               number = as.numeric(input$number),
               p_date = as.numeric(gsub(pattern = "-",replacement="",x=as.character(input$p_date))),
               expiration = as.numeric(gsub(pattern = "-",replacement="",x=as.character(input$expiration))),
               type_m = as.numeric((factor(input$type_m))),
               type_s = as.numeric((factor(input$type_s))))
    })
    trans1 = function(x){ x %>% dplyr::mutate(y=1)}
    
    res1= reactive({round(predict(glm1,trans(sample()),type="response")*100,2)})
    res2 = reactive({round(predict(gam1,trans(sample()),type="response")*100,2)})
    
    res3 = reactive({
        ifelse( predict(sur1,type="expected",newdata=trans(trans1(sample()))) >1, 100,
                round(predict(sur1,type="expected",newdata=trans(trans1(sample()))),3)*100)
    })
    res4 = reactive({round(predict(lda1,trans(sample()),type="prob")$posterior[,2],4)*100})
    res5 = reactive({round(predict(knn1,trans(sample()),type="prob")[,2],4)*100})
    res6 = reactive({round(predict(rf1, trans(sample()),type="prob")[,2])})
    res7 = reactive({round(attr(predict(svm,trans(sample()), probability=T),'probabilities'),4)})
    res8 = reactive({round(predict(xgb, xgb.DMatrix(data=trans(sample()), label=1)),4)*100})
    
    res = reactive({
        if (input$method == "GLM") {res1()}
        else if(input$method == "GAM") {res2()}
        else if(input$method == "CoxPHM") {res3()}
        else if(input$method == "LDA") {res4()}
        else if(input$method == "KNN") {res5()} 
        else if(input$method == "Random Forest") {res6()}
        else if(input$method == "SVM") {res7()}
        else if(input$method == "XGB") {res8()}
    })
    #    if (input$method == "GAM") res = res2()
    
    #    else if (input$method=='GAM') {res = reactive({round(predict(gam1,trans(sample()),type="response")*100,3)})}
    # else if(input$method=="CoxPh") {res() = reactive({round(predict(sur,trans(sample()),type="response")*100,3)})}
    #else {res == 0}
    
    
    #고객정보
    output$table = renderTable({
        tibble("이름"=input$name, "연령"=paste(input$age,"세"), 
               "보험료" = paste(input$payment,"원"), 
               "납입주기" = ifelse(input$cycle==1,"1개월",ifelse(input$cycle==2,"3개월",ifelse(input$cycle == 3,"6개월","12개월"))),
               "납입기간" = paste(input$pd_y,"년"),
               "최종납입횟수" = paste(input$number,"회"),
               "부활유무" = ifelse(input$revival==1,"한번 이상","없음"),
               "계약일자" = as.character(input$p_date),"지급만기일자" = as.character(input$expiration),
               "상품분류" = input$type_m,"상세분류" = input$type_s, "수금방법" = input$t_method)})
    
    output$table2 = renderTable({
        tibble(age=as.numeric(input$age), 
               cycle = as.numeric(input$cycle), 
               pd_y = as.numeric(input$pd_y),
               t_method = as.numeric(sort(order(input$type_m))), 
               payment = as.numeric(input$payment),
               revival = as.numeric(input$revival), 
               number = as.numeric(input$number),
               p_date = as.numeric(gsub(pattern = "-",replacement="",x=as.character(input$p_date))),
               expiration = as.numeric(gsub(pattern = "-",replacement="",x=as.character(input$expiration))),
               type_m = as.numeric((factor(input$type_m))),
               type_s = as.numeric((factor(input$type_s))))
    })
    #예측결과
    output$text = renderText(c(input$name,"고객이 3개월 이내에 상품을 해지할 확률은",paste(res()), "% 입니다."))
    
}

shinyApp(ui = ui, server = server)

