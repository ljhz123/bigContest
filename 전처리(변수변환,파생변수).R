####################################데이터로드##################################
setwd('C:\\Users\\lee\\Documents\\R')
library(dplyr)
data_set = read.table('Data_set.csv', header = T, sep = ',', stringsAsFactors= F)
data_set_test = read.table('test_set.csv', header = T, sep = ',', stringsAsFactors= F)
data_set_full = rbind(data_set,data_set_test)
data_set = data_set_full
data_set_2 = data_set

########################################대출건수######################################
count = data_set_2[,c('BNK_LNIF_CNT','CPT_LNIF_CNT','SPART_LNIF_CNT','ECT_LNIF_CNT','CRDT_LOAN_CNT')] %>% apply(1,sum)

BN_m = data_set_2$BNK_LNIF_CNT %>% mean
CPT_m = data_set_2$CPT_LNIF_CNT %>% mean
SPART_m = data_set_2$SPART_LNIF_CNT %>% mean
ECT_m = data_set_2$ECT_LNIF_CNT%>% mean
CRDT_m = data_set_2$CRDT_LOAN_CNT %>% mean

###############################################################################
t = ((data_set_2$BNK_LNIF_CNT)/BN_m) 
t2 =  t/count
t2[which(t2%>%is.nan)] = 0
data_set_2$BNK_LNIF_CNT = t2
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)

t = ((data_set_2$CPT_LNIF_CNT)/CPT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$CPT_LNIF_CNT = t2
# t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)


t = ((data_set_2$SPART_LNIF_CNT)/SPART_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$SPART_LNIF_CNT = t2
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(breaks=20,freq=F)
t2[data_set_2$TARGET==0] %>% hist(breaks=20,freq=F)

t = ((data_set_2$ECT_LNIF_CNT)/ECT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$ECT_LNIF_CNT = t2
str(data_set_2)
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)

t = ((data_set_2$CRDT_LOAN_CNT)/CRDT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$CRDT_LOAN_CNT = t2
str(data_set_2)

###logistic
# fit = glm(data_set_2$TARGET~.,data_set_2[,c('BNK_LNIF_CNT','CPT_LNIF_CNT','SPART_LNIF_CNT','ECT_LNIF_CNT')],family = "binomial")
# fit$coefficients
# weight = fit$coefficients[2:5]
# score =  data_set_2$BNK_LNIF_CNT*weight[1]+
#   data_set_2$CPT_LNIF_CNT*weight[2]+
#   data_set_2$SPART_LNIF_CNT*weight[3]+
#   data_set_2$ECT_LNIF_CNT*weight[4]
# data_set_2$score = score
# score[data_set_2$TARGET==1] %>% hist(freq=F)
# score[data_set_2$TARGET==0] %>% hist(freq=F)
###############################################################################
###############다시설정#########################
t = ((data_set$BNK_LNIF_CNT)^2/BN_m) 
t2 =  t/count
t2[which(t2%>%is.nan)] = 0
data_set_2$BNK_LNIF_CNT = t2
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)

t = ((data_set$CPT_LNIF_CNT)^2/CPT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$CPT_LNIF_CNT = t2
# t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)


t = ((data_set$SPART_LNIF_CNT)^2/SPART_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$SPART_LNIF_CNT = t2
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(breaks=20,freq=F)
t2[data_set_2$TARGET==0] %>% hist(breaks=20,freq=F)

t = ((data_set$ECT_LNIF_CNT)^2/ECT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$ECT_LNIF_CNT = t2
str(data_set_2)
t2 %>% hist
t2[data_set_2$TARGET==1] %>% hist(freq=F)
t2[data_set_2$TARGET==0] %>% hist(freq=F)

t = ((data_set$CRDT_LOAN_CNT)^2/CRDT_m) 
t2 = t/count 
t2[which(t2%>%is.nan)] = 0
data_set_2$CRDT_LOAN_CNT = t2
str(data_set_2)
#######################################금액데이터######################################
data_set_2$TOT_LNIF_AMT = log(data_set$TOT_LNIF_AMT*1000+1)
data_set_2$TOT_CLIF_AMT = log(data_set$TOT_CLIF_AMT*1000+1)
data_set_2$BNK_LNIF_AMT = log(data_set$BNK_LNIF_AMT*1000+1)
data_set_2$CPT_LNIF_AMT = log(data_set$CPT_LNIF_AMT*1000+1)
data_set_2$CB_GUIF_AMT =  log((data_set$CB_GUIF_AMT)*1000+1)
data_set_2$CUST_JOB_INCM = log(data_set$CUST_JOB_INCM*10000+1)
data_set_2$HSHD_INFR_INCM = log(data_set$HSHD_INFR_INCM*10000+1)
data_set_2$MATE_JOB_INCM = log(data_set$MATE_JOB_INCM*10000+1)
data_set_2$TOT_CRLN_AMT = log((data_set$TOT_CRLN_AMT)+1)
data_set_2$TOT_REPY_AMT = log((data_set$TOT_REPY_AMT)+1)
data_set_2$STLN_REMN_AMT = log(data_set_2$STLN_REMN_AMT+1)
data_set_2$LT1Y_STLN_AMT = log(data_set_2$LT1Y_STLN_AMT+1)
data_set_2$GDINS_MON_PREM = log(data_set$GDINS_MON_PREM+1)
data_set_2$SVINS_MON_PREM = log(data_set$SVINS_MON_PREM+1)
data_set_2$FMLY_GDINS_MNPREM = log(data_set$FMLY_GDINS_MNPREM+1)
data_set_2$FMLY_SVINS_MNPREM = log(data_set$FMLY_SVINS_MNPREM+1)
data_set_2$MAX_MON_PREM = log(data_set$MAX_MON_PREM+1)
data_set_2$TOT_PREM = log(data_set$TOT_PREM+1)
data_set_2$FMLY_TOT_PREM = log(data_set$FMLY_TOT_PREM+1)
data_set_2$FYCM_PAID_AMT = log(data_set_2$FYCM_PAID_AMT+1)
data_set_2$ARPU[data_set_2$ARPU != -1] = log(as.integer(data_set$ARPU[data_set_2$ARPU != -1]))
data_set_2$ARPU[data_set_2$ARPU==-Inf]=0
data_set_2$MON_TLFE_AMT =log(data_set$MON_TLFE_AMT+1)
data_set_2$MOBL_FATY_PRC = log(data_set$MOBL_FATY_PRC+1)
data_set_2$CRMM_OVDU_AMT = log(data_set$CRMM_OVDU_AMT+1)
data_set_2$LT1Y_MXOD_AMT = log(data_set$LT1Y_MXOD_AMT+1)
data_set_2$MOBL_PRIN = log(data_set$MOBL_PRIN+1)

# ttt1 = cbind(data_set_2$CRMM_OVDU_AMT,data_set_2$LT1Y_MXOD_AMT) %>% apply(1,max)
# ttt1[data_set_2$TARGET==1] %>% hist(breaks=20,freq=F)
# ttt1[data_set_2$TARGET==0] %>% hist(breaks=20,freq=F)
####################################유지개월수#########################################
data_set_2$CRDT_OCCR_MDIF = (ceiling(data_set$CRDT_OCCR_MDIF/12)) # 0 : not or null 1: 0year 13:1year...
data_set_2$SPTCT_OCCR_MDIF = ceiling(data_set$SPTCT_OCCR_MDIF/12)
data_set_2$CTCD_OCCR_MDIF = ceiling(data_set$CTCD_OCCR_MDIF/12)
###################################나이##############################################
data_set_2$LAST_CHLD_AGE[which(data_set$LAST_CHLD_AGE=='NULL')] = 0 
data_set_2$LAST_CHLD_AGE = as.integer(data_set_2$LAST_CHLD_AGE) 

#####################################factor처리######################################
data_set_2$OCCP_NAME_G = as.factor(data_set$OCCP_NAME_G)
data_set_2$MATE_OCCP_NAME_G = as.factor(data_set$MATE_OCCP_NAME_G)
data_set_2$AGE = as.factor(data_set$AGE)
data_set_2$SEX = as.factor(data_set$SEX)
#data_set_2$TEL_MBSP_GRAD[(data_set_2$TEL_MBSP_GRAD)%in%c("")] = 'X'
data_set_2$TEL_MBSP_GRAD[(data_set_2$TEL_MBSP_GRAD)%in%c("X")] = ""
data_set_2$TEL_MBSP_GRAD = as.factor(data_set_2$TEL_MBSP_GRAD)
data_set_2$CBPT_MBSP_YN = as.factor(data_set$CBPT_MBSP_YN)
#data_set_2$PAYM_METD[which(data_set$PAYM_METD%in%c(""))]='X' 
data_set_2$PAYM_METD[(data_set_2$PAYM_METD)%in%c("X")] = ""
data_set_2$PAYM_METD = as.factor(data_set_2$PAYM_METD)
data_set_2$LINE_STUS = as.factor(data_set$LINE_STUS)
data_set_2$LT1Y_PEOD_RATE = as.factor(data_set_2$LT1Y_PEOD_RATE)
####################################날짜데이터######################################
month = c()
year = c()
for(i in 1:length(data_set$MIN_CNTT_DATE))
{
  if(data_set$MIN_CNTT_DATE[i]!=0)
  {
    month[i] =  data_set$MIN_CNTT_DATE[i]%%100
    year[i] = data_set$MIN_CNTT_DATE[i]%/%100
  }
  else{
    month[i] = 0
    year[i]=0
  }
  
}
for(i in 1:length(month))
{
  if(month[i]%in%c(1,2,3))
  {
    month[i] =  1
  }else if(month[i]%in%c(4,5,6))
  {
    month[i] =  2
  }else if(month[i]%in%c(7,8,9))
  {
    month[i] =  3
  }else if(month[i]%in%c(10,11,12))
  {
    month[i] =  4
  }
}
vec = c()
for (i in 1:length(month))
{
  year[i] = 2016-year[i]
  month[i] = 2-month[i]
}
str(month)
for (i in 1:length(month))
{
  if(year[i]==2016 & month[i]==2){
    vec[i] = 0
    
  }
  else if(as.integer(month[i])<0)
  {
    vec[i] = as.integer(paste0(as.integer(year[i])-1,as.integer(month[i])+4))
  }else {
    vec[i] = as.integer(paste0(year[i],month[i]))  
  }
  
}
vec2 = vec/10
vec2[is.na(vec2)] = 0##or year.25(50,75,00) ##na값을 양의무한대로 두는것도가능함
for(i in 1:length(vec2))
{
  if((vec2[i]*10)%%10 == 0)
  {
    vec2[i] = (vec2[i]*10)%/%10+0
  }else if((vec2[i]*10)%%10 == 1)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.25
  }else if((vec2[i]*10)%%10 == 2)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.5
  }else if((vec2[i]*10)%%10 == 3)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.75
  }
}
data_set_2$MIN_CNTT_DATE = vec2

month = c()
year = c()
for(i in 1:length(data_set_2$TEL_CNTT_QTR))
{
  if(data_set_2$TEL_CNTT_QTR[i]!=0)
  {
    month[i] =  data_set_2$TEL_CNTT_QTR[i]%%10
    year[i] = data_set_2$TEL_CNTT_QTR[i]%/%10
  }
  else{
    month[i] = 0
    year[i]=0
  }
  
}

vec = c()
for (i in 1:length(month))
{
  year[i] = 2016-year[i]
  month[i] = 2-month[i]
}
str(month)
for (i in 1:length(month))
{
  if(year[i]==2016 && month[i]==2){
    vec[i] = 0
  }
  else if(as.integer(month[i])<0)
  {
    vec[i] = as.integer(paste0(as.integer(year[i])-1,as.integer(month[i])+4))
  }else {
    vec[i] = as.integer(paste0(year[i],month[i]))  
  }
  
}

vec2 = vec/10
vec[is.na(vec)] = 0##or year.25(50,75,00) ##na값을 양의무한대로 두는것도가능함
for(i in 1:length(vec2))
{
  
  
  if((vec2[i]*10)%%10 == 0)
  {
    vec2[i] = (vec2[i]*10)%/%10+0
  }else if((vec2[i]*10)%%10 == 1)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.25
  }else if((vec2[i]*10)%%10 == 2)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.5
  }else if((vec2[i]*10)%%10 == 3)
  {
    vec2[i] = (vec2[i]*10)%/%10+0.75
  }
}

data_set_2$TEL_CNTT_QTR = vec2
#####################################파생변수##########################################
vec1 = rep(0,nrow(data_set))
vec2 = rep(0,nrow(data_set))
vec3 = rep(0,nrow(data_set))
vec4 = rep(0,nrow(data_set))
for ( i in 1:nrow(data_set))
{
  if(data_set_2$BNK_LNIF_CNT[i] !=0)
  {
    vec1[i]= 1
  }
  if(data_set_2$CPT_LNIF_CNT[i] !=0)
  {
    vec2[i]= 1
  }
  if(data_set_2$SPART_LNIF_CNT[i] !=0)
  {
    vec3[i]= 1
  }
  if(data_set_2$ECT_LNIF_CNT[i] !=0)
  {
    vec4[i]= 1
  }
  cat(i,"\n")
}
vec5 = vec1*1000+vec2*100+vec3*10+vec4 
vec5.factor = as.character(vec5)
vec5.prop = (vec1+vec2+vec3+vec4)/4
vec5.prop %>% table
data_set_2$vec.factor = vec5.factor
data_set_2$vec.prop = vec5.prop
data_set_2$vec.factor[which(data_set_2$TARGET==1)] %>% table %>% barplot(las=2)
data_set_2$vec.factor[which(data_set_2$TARGET==0)] %>% table %>% barplot(las=2)
#
for (i in 1:length(data_set_2$BNK_LNIF_CNT))
{
  data_set_2$TOT_LNIF_CNT[i] = (data_set$BNK_LNIF_CNT[i]+data_set$CPT_LNIF_CNT[i]+data_set$SPART_LNIF_CNT[i]+data_set$ECT_LNIF_CNT[i])
}
hist(data_set_2$TOT_LNIF_CNT[which(data_set$TARGET==1)],xlim = c(0,max(data_set_2$TOT_LNIF_CNT)),freq = F)
hist(data_set_2$TOT_LNIF_CNT[which(data_set$TARGET==0)],xlim = c(0,max(data_set_2$TOT_LNIF_CNT)),freq = F)

###############
data_set_2$is.same.CRDT_SPTCT = rep(1,nrow(data_set))
for(i in 1:nrow(data_set))
{
  if(data_set$CRDT_OCCR_MDIF[i]==0 && data_set$SPTCT_OCCR_MDIF[i]==0)
  {
    cat(i,"\n")
    data_set_2$is.same.CRDT_SPTCT[i] = 0  
  }#############원래 0 0 이면 같은걸로 안취급
  data_set_2$diff_occr_mdif[i] = data_set_2$CRDT_OCCR_MDIF[i]-data_set_2$SPTCT_OCCR_MDIF[i]
}
data_set_2$is.same.CRDT_SPTCT = as.factor(data_set_2$is.same.CRDT_SPTCT)

###############
data_set_2$is.same.repy.crln= rep(1,nrow(data_set))
for(i in 1:nrow(data_set))
{
  if(data_set$TOT_CRLN_AMT[i]==0 & data_set$TOT_REPY_AMT[i]==0)
  {
    cat(i,"\n")
    data_set_2$is.same.repy.crln[i] = 0  
  }#############원래 0 0 이면 같은걸로 안취급
}
data_set_2$diff_repy.crln = data_set$TOT_CRLN_AMT-data_set$TOT_REPY_AMT


##############
data_set_2$is.strt.cg.0 = rep(0,nrow(data_set))
data_set_2$diff.card.grad = data_set$STRT_CRDT_GRAD-data_set$LTST_CRDT_GRAD
for(i in 1:nrow(data_set))
{
  if((data_set_2$STRT_CRDT_GRAD[i] ==0 ))
  {
    data_set_2$is.strt.cg.0[i] = 1
  }
  cat(i,'\n')
}

data_set_2$vec.factor = as.factor(data_set_2$vec.factor)
data_set_2$vec.prop = (data_set_2$vec.prop)
data_set_2$is.same.CRDT_SPTCT = as.factor(data_set_2$is.same.CRDT_SPTCT)
data_set_2$is.same.repy.crln = as.factor(data_set_2$is.same.repy.crln)
data_set_2$is.strt.cg.0 = as.factor(data_set_2$is.strt.cg.0)

##DTI 계산 (부채비용/소득)
# data_set_2$DTI2 = (data_set$TOT_CRLN_AMT
#                 +data_set$TOT_LNIF_AMT*1000
#                 +data_set$LT1Y_STLN_AMT)/data_set$CUST_JOB_INCM
for(i in 1:nrow(data_set_2))
{
  if(data_set_2$CUST_JOB_INCM[i]==0)
  {
    data_set_2$DTI[i] = 50000
  }else 
  {
    data_set_2$DTI[i] = (data_set$TOT_CRLN_AMT[i])/(data_set$CUST_JOB_INCM[i])
  }
      cat(i,'DTI생성중\n')
}
data_set_2$DTI[which(data_set_2$DTI %>% is.nan)] = 0
data_set_2$DTI[which(data_set$TARGET==0)] %>% hist(xlim=c(0,50000),breaks = 500,freq=F)
data_set_2$DTI[which(data_set$TARGET==1)] %>%hist(xlim=c(0,50000),breaks = 500,freq=F)

data_set_2$PropBNK.SPART = data_set_2$BNK_LNIF_CNT/(data_set_2$BNK_LNIF_CNT+data_set_2$SPART_LNIF_CNT)
data_set_2$PropBNK.SPART[which(data_set_2$PropBNK.SPART %>%is.na)]=0 
data_set_2$PropBNK.SPART[data_set_2$TARGET==1] %>% hist(freq=F)
data_set_2$PropBNK.SPART[data_set_2$TARGET==0] %>% hist(freq=F)



#####################################train test 구분 #########################
str(data_set_2)
for (i in 1:ncol(data_set_2))
{
  if(data_set_2[,i]%>%class == 'factor')
  {
    print(colnames(data_set_2)[i])
    print(unique(data_set_2[,i]))
  }
}
train_idx = which(!(data_set_2$TARGET %>% is.na) )
test_idx = which(data_set_2$TARGET %>% is.na)

#data_set_2 = data_set_2[which(!(data_set_2%>% colnames %in% c("CUST_ID")))] 
data_set_2$TARGET = data_set_2$TARGET %>% as.factor() 
data_set_train = data_set_2[train_idx,]
data_set_test = data_set_2[test_idx,]

str(data_set_2)

write.csv(data_set_2,'data_set_2_1010_sq_first.csv',row.names = F)
