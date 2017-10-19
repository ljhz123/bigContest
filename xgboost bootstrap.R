############################## package install #################################
setwd('C:\\Users\\User\\Desktop\\base_MICE_Tlink_regular_scale_index')
if (!require(ade4)) { install.packages("ade4") ; library(ade4) }
install.packages("reshape2")
install.packages("dplyr")
install.packages("xgboost")
install.packages("caret")
install.packages("mlr")
install.packages("DMwR")
install.packages("BBmisc")
library(reshape2)
library(dplyr)
library(xgboost)
library(caret)
library(mlr)
library(DMwR)
################################### read.data ###################################
data_Train_All = read.table('base_MICE_Tlink_regular_scale_AllTrain.csv', header = T, sep = ',', stringsAsFactors= T)

##############smote 데이터생성비율 구하기 ##############################
# sample = sample(nrow(data_Train_All),nrow(data_Train_All)*0.7) #train index
# vtest = which(!(c(1:nrow(data_Train_All)%in%sample))) #validation index 
# data_Train_All$TARGET = as.factor(data_Train_All$TARGET)
# data_Train_all_vali = data_Train_All[sample,]
# data_Train_all_test = data_Train_All[vtest,]
# #data_Train_smoted <- SMOTE(TARGET~., data_Train_all_vali,perc.over = 1000,perc.under=100)
# data_Train_smoted <- SMOTE(TARGET~., data_Train_all_vali)

data_Train_All = data_Train_All[,!(colnames(data_Train_All)%in%c('X'))]
data_Answer = read.table('base_MICE_Tlink_regular_Answer.csv', header = T, sep = ',', stringsAsFactors= T)
data_Answer = data_Answer[,!(colnames(data_Answer)%in%c('X'))]
colnames(data_Answer) = colnames(data_Train_All)

### F measure Function

F_measure = function(y, pred_y){
  result <- table(y, pred_y)
  Precision <- result[4] / (result[3] + result[4])
  Recall <- result[4] / (result[2] + result[4])
  f.value <- 2 * (Precision * Recall) / (Precision + Recall)
  return(f.value)
  print(f.value)
}

###########################################################################################################
###########################################################################################################

All_proc = function(data_Train_Val,pred_y_test, m){
  
  for(i in 1:m){#부스트렙횟수
    ###########################
    
    sample = sample(nrow(data_Train_Val),nrow(data_Train_Val)*0.7)
    vtest = which(!(c(1:nrow(data_Train_Val)%in%sample))) 
    data_Train_Val$TARGET = as.factor(data_Train_Val$TARGET)
    data_Train_all_vali = data_Train_Val[sample,]
    data_Train_all_test = data_Train_Val[vtest,]
    data_Train_smoted <- SMOTE(TARGET~., data_Train_all_vali)  #부스트렙 할때마다 SMOTE 다시 설정
    
    table(data_Train_smoted$TARGET)
  
  
    colnames(data_Train_all_test) = colnames(data_Train_smoted)
    data_Train_all_test$TARGET = as.factor(data_Train_all_test$TARGET)
  
    ###########################
    data_Train =  data_Train_smoted #train random sample
    data_vali1 = data_Train_all_test
    #data_vali2 = data_Test2
    
    ############################### XGBOOST Tuning #################################
    
    # Step1: task and learner
    
    train.task <- makeClassifTask(data = data_Train, target = 'TARGET')
    
    lrn.main <- makeLearner('classif.xgboost', predict.type = "response")
    
    lrn.main$par.vals <- list( objective="binary:logistic", eval_metric="error", booster="gbtree", nrounds=5)
    
    ps.set <- makeParamSet(
      makeIntegerParam("max_depth",lower = 1, upper = 20), 
      makeIntegerParam("min_child_weight",lower = 1, upper = 20)
    )
    
    
    # Step2: search and validation
    ctrl <- makeTuneControlRandom(maxit = 20)
    rdesc <- makeResampleDesc('CV', iters = 5L, stratify = T)
    
    
    # Step3: tune
    tune.lrn.main <- tuneParams(lrn.main, task = train.task, resampling = rdesc,
                                par.set = ps.set, control = ctrl,
                                measures = acc)
    
    
    lrn.main <- setHyperPars(lrn.main, par.vals = tune.lrn.main$x)
    
    
    #################################################################################
    lrn.main$par.vals <- list(objective="binary:logistic", eval_metric="error", booster="gbtree", nrounds=5,
                              max_depth = lrn.main$par.vals$max_depth, min_child_weight = lrn.main$par.vals$min_child_weight)
    
    
    ps.set <- makeParamSet(
      makeDiscreteParam("gamma",values = c(0, 0.01, 0.02, 0.03, 0.04))
    )
    
    
    # Step2: search and validation
    ctrl <- makeTuneControlRandom(maxit = 20)
    rdesc <- makeResampleDesc('CV', iters = 5L, stratify = T)
    
    
    # Step3: tune
    tune.lrn.main <- tuneParams(lrn.main, task = train.task, resampling = rdesc,
                                par.set = ps.set, control = ctrl,
                                measures = acc)
    
    
    lrn.main <- setHyperPars(lrn.main, par.vals = tune.lrn.main$x)
    
    
    
    #################################################################################
    lrn.main$par.vals <- list(objective="binary:logistic", eval_metric="error", booster="gbtree", nrounds=5,
                              max_depth = lrn.main$par.vals$max_depth, min_child_weight = lrn.main$par.vals$min_child_weight,
                              gamma = lrn.main$par.vals$gamma
    )
    
    
    ps.set <- makeParamSet(
      makeDiscreteParam("subsample",values = c(0.6, 0.7, 0.8, 0.9)), 
      makeDiscreteParam("colsample_bytree",values = c(0.6, 0.7, 0.8, 0.9))
    )
    
    
    # Step2: search and validation
    ctrl <- makeTuneControlRandom(maxit = 20)
    rdesc <- makeResampleDesc('CV', iters = 5L, stratify = T)
    
    
    # Step3: tune
    tune.lrn.main <- tuneParams(lrn.main, task = train.task, resampling = rdesc,
                                par.set = ps.set, control = ctrl,
                                measures = acc)
    
    
    lrn.main <- setHyperPars(lrn.main, par.vals = tune.lrn.main$x)
    
    
    
    #################################################################################
    lrn.main$par.vals <- list(objective="binary:logistic", eval_metric="error", booster="gbtree", nrounds=5,
                              max_depth = lrn.main$par.vals$max_depth, min_child_weight = lrn.main$par.vals$min_child_weight,
                              gamma = lrn.main$par.vals$gamma, 
                              subsample = lrn.main$par.vals$subsample, colsample_bytree = lrn.main$par.vals$colsample_bytree
    )
    
    
    ps.set <- makeParamSet(
      makeDiscreteParam("eta", values = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35))
    )
    
    # Step2: search and validation
    ctrl <- makeTuneControlRandom(maxit = 20)
    rdesc <- makeResampleDesc('CV', iters = 5L, stratify = T)
    
    
    # Step3: tune
    tune.lrn.main <- tuneParams(lrn.main, task = train.task, resampling = rdesc,
                                par.set = ps.set, control = ctrl,
                                measures = acc)
    
    
    lrn.main <- setHyperPars(lrn.main, par.vals = tune.lrn.main$x)
    
    
    
    ############################## Train and predict ################################
    
    # Step1: predictions on main
    
    lrn.main$par.vals <- list(objective="binary:logistic", eval_metric="error", booster="gbtree", nrounds = 100, nthread = 4,
                              max_depth = lrn.main$par.vals$max_depth, min_child_weight = lrn.main$par.vals$min_child_weight,
                              gamma = lrn.main$par.vals$gamma, 
                              subsample = lrn.main$par.vals$subsample, colsample_bytree = lrn.main$par.vals$colsample_bytree,
                              eta = lrn.main$par.vals$eta
    )
    
    global.main.train <- train(lrn.main, train.task)
    
    tt_test1 <- predict(global.main.train, newdata =  data_Train_all_test[, -1])
    tt_answer <- predict(global.main.train, newdata =  data_Answer[, -1])
    count=1
    for(j in 1:nrow(data_Train_Val)){
      if(j%in%vtest)
      {
        pred_y_test1[j,i] <- tt_test1$data$response[count]
        count= count+1
      }
      
    }
    for(k in 1:nrow(data_Answer))
    {
      pred_y_answer[k,i] = tt_answer$data$response[k]
    }
    
    print(i)
    print(table(pred_y_test1[,i]))
    
    print(F_measure(data_Train_All[vtest,]$TARGET, pred_y_test1[!is.na(pred_y_test1[,i]),i]))
    
  }  
  
  assign('pred_y_test1', pred_y_test1, envir = .GlobalEnv)
  assign('pred_y_answer', pred_y_answer, envir = .GlobalEnv)
  
}

###########################################################################################################
###########################################################################################################
m = 30 # 부스트렙횟수 설정
pred_y_test1 = matrix(rep(NA, m * nrow(data_Train_All)), ncol = m) #부스트렙 결과저장
pred_y_answer = matrix(rep(NA, m * nrow(data_Answer)), ncol = m)
#pred_y_test2 = matrix(rep(0, m * nrow(data_Test2)), ncol = m)

#XGBOOST 함수실행
All_proc(data_Train_All,pred_y_test, m)

pred_y_test2 = pred_y_test1 #복사본 
#각각 F값구하기 and f score 평균
MEAN = c() 
for(i in 1:ncol(pred_y_test2))
{
  fs = F_measure(data_Train_All[!is.na(pred_y_test2[,i]),]$TARGET, pred_y_test2[!is.na(pred_y_test2[,i]),i]) %>%print
  MEAN = rbind(MEAN,fs)
}  
#each bootstrap f score 평균
sum(MEAN)/ncol(pred_y_test2)
fscore = c()
pred_y_test2 = pred_y_test2-1

##가중치부여 부분
# for(j in 1:length(MEAN))
# {
#   pred_y_test2[,j] = pred_y_test2[,j]*MEAN[j]/mean(MEAN)
# }

###############배깅 최적의i 값 구하기 ########################
for(i in seq(0.01,0.99,0.01)){
  
  selected_count = apply(pred_y_test2,1,function(c)sum(!is.na(c)))
  
  
  
  prediction_test1 = rowSums((pred_y_test2),na.rm=TRUE)
  prediction_test1 = prediction_test1/selected_count
  prediction_test1 = 1 * (prediction_test1 >= i)
  prediction_test1
  
  print(table(data_Train_All$TARGET, prediction_test1))
  print(F_measure(data_Train_All$TARGET, prediction_test1))
  fscore = rbind(fscore,F_measure(data_Train_All$TARGET, prediction_test1))
  
  if(max_F1 < F_measure(data_Train_All$TARGET, prediction_test1)){
    max_F1 = F_measure(data_Train_All$TARGET, prediction_test1)
    max_p1 = i}
}

x = seq(0.01,0.99,0.01)
y = fscore
plot(x,y) #최적의 p값
max(y) #최고의 fscore
###########################부스트렙 테이블 저장하기 ######################
Bootstraped = cbind(data_Train_All$TARGET,pred_y_test1)
write.csv(Bootstraped,'정호1.csv')

##########################정답셋 저장 및 정답출력 #######################
pred_y_answer2 = pred_y_answer
selected_count = apply(pred_y_answer2,1,function(c)sum(!is.na(c)))
#pred_y_answer2 = pred_y_answer2-1
# for(j in 1:length(MEAN))
# {
#   pred_y_answer[,j] = pred_y_answer[,j]*MEAN[j]/sum(MEAN)  
# }




answer_boot = rbind(pred_y_answer2,c(MEAN))
write.csv(answer_boot,'정호1정답.csv')

# 여러 컴퓨터로 부스트렙데이터 수집(200회이상)

