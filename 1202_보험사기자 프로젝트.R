rm(list=ls())

#원본 데이터 읽기
data_cust <- read.csv("CUST_DATA.csv", header=TRUE, sep=",",
                      encoding="CP949", fileEncoding="UCS-2")
data_claim <- read.csv("CLAIM_DATA.csv", header=TRUE, sep=",",
                       encoding="CP949", fileEncoding="UCS-2")

#정상인과 사기자의 수 확인
(count_siu <-table(data_cust$SIU_CUST_YN))

names(count_siu) <- c("분석대상", "정상인", "사기자")
pie(count_siu,
    cex=0.8,
    main="사기자 수",
    labels=paste(names(count_siu), "\n",
                 count_siu, "명", "\n",
                 round(count_siu/sum(count_siu)*100), "%"))

#전처리 - 나이를 연령대로 변환
age_to_gen <-function(data) {data=data %/% 10}
data_cust$AGE <-sapply(data_cust$AGE, age_to_gen)

#전처리 - 사기자여부(SIU_CUST_YN)을 Y=1, N=0으로 변환
yn_to_10 <- function(data) {
  if(data=='Y') {data = 1}
  else if(data=='N') {data = 0}
  else {data = ''}
  }

data_cust$SIU_CUST_YN <- sapply(data_cust$SIU_CUST_YN, yn_to_10)

#전처리 - 보험설계사 경력(FP_CAREER) 여부를 Y=1, N=0으로 변환
data_cust$FP_CAREER <-sapply(data_cust$FP_CAREER, yn_to_10)

#전처리 - NA 값을 0으로 변환
na_to_0 <- function(data) {
  if(is.na(data)) {data = 0}
  else {data = data}
}

data_cust$RESI_TYPE_CODE <- sapply(data_cust$RESI_TYPE_CODE, na_to_0)
data_cust$TOTALPREM <- sapply(data_cust$TOTALPREM, na_to_0)

#전처리 - 지역(CTPR)을 코드값으로 인코딩, 지역이름 따로 저장
is.factor(data_cust$CTPR) #범주형 변수인지 확인 - 아님

(area_name <- levels(as.factor(data_cust$CTPR)))
data_cust$CTPR <- as.numeric(as.factor(data_cust$CTPR))

#전처리 - 최소신용도(MINCRDT), 최대 신용도(MAXCRDT) 변수의 NA 값을 6으로 변환
#변수 설명서에 따라.
na_to_6 <- function(data){
  if(is.na(data)) {return(6)}
  else {return(data)}
}

data_cust$MINCRDT <- sapply(data_cust$MINCRDT, na_to_6)
data_cust$MAXCRDT <- sapply(data_cust$MAXCRDT, na_to_6)


#전처리-직업(OCCP_GRP_1)에서 첫 문자만 빼내서 직업코드로 바꾸기
occp_grp_1_to_num <- function(data){
  data <- substr(data, 1, 1)
  if(data=='') {data=0}
  else {data=as.integer(data)}
}
data_cust$OCCP_GRP_1 <- sapply(data_cust$OCCP_GRP_1, occp_grp_1_to_num)
data_cust$MATE_OCCP_GRP_1 <- sapply(data_cust$MATE_OCCP_GRP_1,
                                    occp_grp_1_to_num)

#전처리-결혼여부(WEDD_YN) 변수가 널스트링("")일 경우 N으로 변경
#결혼여부의 결측치 수, 막내자녀연령, 자녀수 정보를 토대로, 결혼여부 겨측치를 N으로 추정.
#그후 다시 결혼여부를 Y=1, n=0으로 인코딩
nullstring_to_N <- function(data){
  if(data=='') {data='N'}
  else {data=data}
}
data_cust$WEDD_YN <- sapply(data_cust$WEDD_YN, nullstring_to_N)
data_cust$WEDD_YN <- sapply(data_cust$WEDD_YN, yn_to_10)

#전처리 완료한 데이터셋 저장
write.csv(data_cust, "data_cust_1-1.csv", row.names=FALSE)


###### 4) 데이터 탐색 후 변수 제거 ###########
data_cust <- read.csv("data_cust_1-1.csv", header=TRUE)

data_cust <- subset(data_cust, select=-c(OCCP_GRP_2, MATE_OCCP_GRP_2))
data_cust <- subset(data_cust, select=-c(MAX_PAYM_YM, MAX_PRM))
data_cust <- subset(data_cust, select=-CUST_RGST)
data_cust <- subset(data_cust, select=-c(CHLD_CNT, LTBN_CHLD_AGE))
data_cust <- subset(data_cust, select=-JPBASE_HSHD_INCM)

#데이터 임시 저장
write.csv(data_cust, "data_cust_2.csv", row.names=FALSE)


################ 2.4. 결측치 추정 및 파생변수 추가

# 1) 결측치 추정

# 소득이 0인 사람의 예상소득을 같은 직업을 가진 사람의 소특 평균으로 추정.
data_cust <- read.csv("data_cust_2.csv", header=TRUE)

(cust_incm_avg_by_occp <- tapply(data_cust$CUST_INCM,
                                 data_cust$OCCP_GRP_1, mean, na.rm=TRUE))

(cust_incm_avg_by_occp <- round(cust_incm_avg_by_occp))

zero_to_mean <- function(occp, incm) {
  if(is.na(incm))
    return (cust_incm_avg_by_occp[occp+1])
  else
    return (incm)
}

data_cust$CUST_INCM <- mapply(zero_to_mean,
                              data_cust$OCCP_GRP_1, data_cust$CUST_INCM)



# 2) 파생변수 추가
# 가정 : 고객별 평균 입원일수가 사기자 여부에 영향
# 보험 청구 데이터에서 고객별 평균 입원일을 계산하여 기존 데이터와 병합.
# 고객별 평균 입원일수 파생변수 추가.

data_claim <- read.csv("CLAIM_DATA.csv", header=TRUE, sep=",",
                       encoding="CP949", fileEncoding="UCS-2")
hosp_day_per_cust <- aggregate(data_claim$VLID_HOSP_OTDA,
                               by=list(data_claim$CUST_ID), mean)
names(hosp_day_per_cust) <- c("CUST_ID", "HOSP_DAYS")
hosp_day_per_cust$HOSP_DAYS <- round(hosp_day_per_cust$HOSP_DAYS)
data_cust <- merge(data_cust, hosp_day_per_cust)


#사고원인 구분 코드
#지급청구 원인 사유 코드
#를 조합하여 고객별로 사고구분 및 청구사유 횟수 계산
table(data_claim$ACCI_DVSN, data_claim$DMND_RESN_CODE)

acci_dmnd_count <- table(data_claim$CUST_ID, data_claim$ACCI_DVSN,
                         data_claim$DMND_RESN_CODE)
acci_dmnd_count <- as.data.frame(acci_dmnd_count)
names(acci_dmnd_count) <-c("CUST_ID", "ACCI_DVSN", "DMND_RESN_CODE",
                           "value")

# 피벗테이블 만들기 - 
#사고원인과 지급청구 원인을 조합한 코드를 열 이름으로 갖고,
#횟수를 값으로 갖도록 데이터 구조 변경
install.packages("reshape2")
library(reshape2)

acci_dmnd_count <- dcast(data=acci_dmnd_count,
                         CUST_ID ~ ACCI_DVSN + DMND_RESN_CODE, fun=sum)
data_cust <- merge(data_cust, acci_dmnd_count) # 기존 데이터와 병합합
data_cust <- data_cust[, sapply(data_cust,  #분산이 0인 열 삭제
                                function(v) var(v, na.rm=TRUE)!=0)] 
write.csv(data_cust, "data_cust_3.csv", row.names=FALSE)


#################### 2.5 모델링

# 1) 불필요한 변수 삭제
data_cust <- read.csv("data_cust_3.csv", header=TRUE)
data_cust <- subset(data_cust,
                    select=-c(RESI_COST, RESI_TYPE_CODE, TOTALPREM,
                              MINCRDT, MAXCRDT))

write.csv(data_cust, "data_cust_4_manual.csv", row.names=FALSE)

# 2) 데이터 샘플링
# 사기여부 판별된 데이터와, 그렇지 않은 데이터 분리하여
# 트레이닝 데이터셋과 테스트 데이터셋으로 나누기
data_cust <- read.csv("data_cust_4_manual.csv", header=TRUE)
cust_train <- subset(data_cust, subset=!(data_cust$SIU_CUST_YN==""))
cust_train <- cust_train[order(cust_train$CUST_ID),] #트레이닝 셋 정렬
nrow(cust_train)

cust_test <- subset(data_cust, subset=(is.na(data_cust$SIU_CUST_YN)))
cust_test <- cust_test[order(cust_test$CUST_ID),] #테스트셋 정렬
nrow(cust_test)

write.csv(cust_train, "data_train_set.csv", row.names=FALSE)
write.csv(cust_test, "data_test_set.csv", row.names=FALSE)


# 3) 모델생성
# 트레이닝 셋 : cust_train
# 테스트 셋 : cust_test

#데이터 샘플링
cust_data1 <- subset(cust_train, select=-c(CUST_ID, DIVIDED_SET)) #train
cust_data2 <- subset(cust_train, select=-c(CUST_ID, DIVIDED_SET, SIU_CUST_YN)) #test

#7:3으로 트레이닝 셋을 다시 샘플링
idx <- sample(1:nrow(cust_data1), nrow(cust_data1)*0.7)
train <- cust_data1[idx,] #트레이닝에서 70%
test <- cust_data1[-idx,] #트레이닝에서 30%

# 모델링
# 1) 의사결정나무 모델 
library(party)
tree_model <- ctree(SIU_CUST_YN~., data=train)
pred_tr <- predict(tree_model, test)

# 2) 랜덤 포레스트
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(SIU_CUST_YN~., data=train, ntree=100, na.action=na.omit)
pred_rf <- predict(rf_model, test)

# 3) xgboost
install.packages("xgboost")
library(xgboost)

train_mat <- as.matrix(train[,-1])
train_lab <- train$SIU_CUST_YN
dtrain <- xgb.DMatrix(data=train_mat, label=train_lab)

xgb_model <- xgboost(data=dtrain, eta=1, objective='binary:logistic',
                     nround=60, nthread=8)

test_mat <- as.matrix(test[,-1])
test_lab <- test$SIU_CUST_YN

pred_xg <- predict(xgb_model, test_mat)


#모델 평가 - f1 score
f1score <- function(m){
  ac <- (m[1,1]+m[2,2])/sum(m)
  pr <- m[2,2]/(m[1,2]+m[2,2])
  re <- m[2,2]/(m[2,1]+m[2,2])
  f1 <- 2*pr*re/(pr+re)
  print(ac)
  print(pr)
  print(re)
  return(f1)
}
         
xgb <- table(test_lab, round(pred_xg))
tr <- table(test$SIU_CUST_YN, round(pred_tr))
rf <- table(test$SIU_CUST_YN, round(pred_rf))

f1score(xgb) #xgboost
f1score(tr) #decision tree
f1score(rf) #random forest


#랜덤 포레스트로 최종 예측 결과
pred_rf <- predict(rf_model, cust_test)
result <- as.data.frame(round(pred_rf))
table(result)

write.csv(result, "result.csv", row.names=FALSE)