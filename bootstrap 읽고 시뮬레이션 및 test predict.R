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
library(dplyr)
setwd('C:\\Users\\User\\Desktop\\base_MICE_Tlink_regular_scale_index')
df = read.table('통합scale2.csv', header = T, sep = ',', stringsAsFactors= T)

str(df)
df = df[!(colnames(df) %in% c('X'))]
df_target = df$X.1

pred_y_test1 = df[,-1]
pred_y_test2 = pred_y_test1
##################################################
F_measure = function(y, pred_y){
  result <- table(y, pred_y)
  Precision <- result[4] / (result[3] + result[4])
  Recall <- result[4] / (result[2] + result[4])
  f.value <- 2 * (Precision * Recall) / (Precision + Recall)
  return(f.value)
  print(f.value)
}
####################################################

MEAN = c()
for(i in 1:ncol(pred_y_test1))
{
  fs = F_measure(df_target[!is.na(pred_y_test1[,i])], pred_y_test1[!is.na(pred_y_test1[,i]),i]) %>%print
  MEAN = rbind(MEAN,fs)
}
####MEAN F값 출력

#each bootstrap f score 평균

sum(MEAN)/length(MEAN)

#변수초기화
max_p1 = 0;max_F1 = 0
fscore = c()
pred_y_test2 = pred_y_test2-1
str(pred_y_test2)

###################
## 모델돌려보기 
## 모델에서 최적의 p값찾ㄱ
pred = c()
for(i in seq(0.01,0.99,0.01)){
  print(i)
  selected_count = apply(pred_y_test2,1,function(c)sum(!is.na(c)))
  
  
  prediction_test1 = rowSums((pred_y_test2),na.rm=TRUE)
  prediction_test1 = prediction_test1/selected_count
  pred = rbind(pred,prediction_test1)
  prediction_test1 = 1 * (prediction_test1 >= i)
  prediction_test1
  
  print(table(df_target, prediction_test1))
  print(F_measure(df_target, prediction_test1))
  fscore = rbind(fscore,F_measure(df_target, prediction_test1))
  
  #if(max_F1 < F_measure(df_target, prediction_test1)){
   # max_F1 = F_measure(df_target, prediction_test1)
    #max_p1 = i}
}

plot(seq(0.01,0.99,0.01),fscore) #seq별 fscore plot 그리기
max_p = which.max(fscore) #max_p 최적의 p값 구하기 
index = seq(0.01,0.99,0.01)[max_p]
fscore[index*100] # 최적의 f값

########################시뮬레이션 ################################
sss=c()
fscore_2000 = c()
for(s in 1:200)
{
  sample2 = sample(1:nrow(pred_y_test2),2000)
  temp = pred_y_test2[sample2,]
  fscore_2000 = c()
  pred= c()
  pred = c()
  for(i in seq(0.01,0.99,0.01)){
    print(i)
    selected_count = apply(temp,1,function(c)sum(!is.na(c)))
    
    
    prediction_test1 = rowSums(temp,na.rm=TRUE)
    prediction_test1 = prediction_test1/selected_count
    pred = c(pred,prediction_test1)
    prediction_test1 = 1 * (prediction_test1 >= i)
    prediction_test1
    
    print(table(df_target[sample2], prediction_test1))
    print(F_measure(df_target[sample2], prediction_test1))
    fscore_2000= rbind(fscore_2000,F_measure(df_target[sample2], prediction_test1))
    
    #if(max_F1 < F_measure(df_target, prediction_test1)){
    # max_F1 = F_measure(df_target, prediction_test1)
    #max_p1 = i}
  }
  plot(seq(0.01,0.99,0.01),fscore_2000)
  p = 0.75
  sss = c(sss,fscore_2000[p*100])
  
}
sss
mean(sss)
var(sss)
t.test(sss)
boxplot(sss)
##############################################
##############weight##################
pred_y_test3 = pred_y_test2
for(j in 1:length(MEAN))
{
  pred_y_test3[,j] = pred_y_test2[,j]*MEAN[j]/mean(MEAN)
}
str(pred_y_test3)
fscore_weight = c()
for(i in seq(0.01,0.99,0.01)){
  print(i)
  selected_count = apply(pred_y_test3,1,function(c)sum(!is.na(c)))
  
  
  prediction_test1 = rowSums((pred_y_test3),na.rm=TRUE)
  prediction_test1 = prediction_test1/selected_count
  prediction_test1 = 1 * (prediction_test1 >= i)
  prediction_test1
  
  print(table(df_target, prediction_test1))
  print(F_measure(df_target, prediction_test1))
  fscore_weight = rbind(fscore_weight,F_measure(df_target, prediction_test1))
  
  #if(max_F1 < F_measure(df_target, prediction_test1)){
  # max_F1 = F_measure(df_target, prediction_test1)
  #max_p1 = i}
}
x = seq(0.01,0.99,0.01)
y2 = fscore_weight
plot(x,y2) #최적의 p값
max(y2) #최고의 fscore
hist(prediction_test1)
#######################weight 시뮬레이션 ######################
sss_weight=c()
for(s in 1:200)
{
  sample2 = sample(1:nrow(pred_y_test3),2000)
  temp = pred_y_test3[sample2,]
  fscore_2000_weight = c()
  pred= c()
  pred = c()
  for(i in seq(0.01,0.99,0.01)){
    print(i)
    selected_count = apply(temp,1,function(c)sum(!is.na(c)))
    
    
    prediction_test1 = rowSums(temp,na.rm=TRUE)
    prediction_test1 = prediction_test1/selected_count
    pred = c(pred,prediction_test1)
    prediction_test1 = 1 * (prediction_test1 >= i)
    prediction_test1
    
    print(table(df_target[sample2], prediction_test1))
    print(F_measure(df_target[sample2], prediction_test1))
    fscore_2000= rbind(fscore_2000,F_measure(df_target[sample2], prediction_test1))
    
  }
  plot(seq(0.01,0.99,0.01),fscore_2000_weight)
  p = 0.75
  sss_weight = c(sss_weight,fscore_2000_weight[p*100])
  
}
sss_weight
mean(sss_weight)
var(sss_weight)
t.test(sss_weight)
boxplot(sss_weight)
##########################정답셋 저장 및 정답출력 #######################

selected_count = apply(pred_y_answer,1,function(c)sum(!is.na(c)))
pred_y_answer = pred_y_answer-1
####가중치 부여부분
# for(j in 1:length(MEAN))
# {
#   pred_y_answer[,j] = pred_y_answer[,j]*MEAN[j]/sum(MEAN)  
# }
####
prediction_answer = rowSums((pred_y_answer),na.rm=TRUE)
prediction_answer = prediction_answer/selected_count
prediction_answer= 1 * (prediction_answer >= i)
prediction_answer

write.csv(prediction_answer,'이것이정답.csv')

