#사용자기반 CF
library(recommenderlab)
data("Jester5k")

set.seed(1) #train/test set <- 시드: 재사용 가능한 결과 생성
which_train <- sample(x=c(TRUE, FALSE), size = nrow(Jester5k), replace = TRUE,
                      prob = c(0.8,0.2))
head(which_train)
rec_data_train <- Jester5k[which_train,]
rec_data_test <- Jester5k[!which_train,]
dim(rec_data_train)
dim(rec_data_test)

#사용자 기반 협업 모델 생성
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models
recc_model <- Recommender(data = rec_data_train, method = "UBCF")
recc_model

#테스트셋 예측
n_recommended <- 10 #사용자마다 10개의 추천 생성
recc_predicted <- predict(object = recc_model, newdata = rec_data_test,n = n_recommended)
recc_predicted

rec_list <- sapply(recc_predicted@items, function(x){
  colnames(Jester5k)[x]
  })
rec_list

#모든 사용자들에 대해 몇 개의 추천이 생성되는지 확인
number_of_items = sort(unlist(lapply(rec_list, length)), decreasing = TRUE)
table(number_of_items)

#데이터 세트 분석
table(rowCounts(Jester5k)) # 평가자 수
model_data = Jester5k[rowCounts(Jester5k)<80] #80개 이상 평가한 사용자의 레코드삭제
library(dplyr)
model_data <- na.omit(model_data)
dim(model_data)
boxplot(rowMeans(model_data))
#이상치제거
boxplot(rowMeans(model_data[rowMeans(model_data)>=-5 & rowMeans(model_data)<=7]))
#평균값이 매우 낮거나 높은 사용자 제외
model_data = model_data [rowMeans(model_data)>=-5 & rowMeans(model_data)<= 7]
dim(model_data)
#평균 분포 조사
image(model_data, main = "Rating distribution of 100 users")

#K-교차 검즘을 토한 추천 모델 평가
items_to_keep <- 30
rating_thredshold <- 3
n_fold <- 5
eval_sets <- evaluationScheme(data = model_data, 
                              method = "cross-validation", 
                              train = percentage_training, 
                              given = items_to_keep,
                              goodRating = rating_thredshold,
                              k = n_fold)
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets
getData(eval_sets, "train") #훈련용 세트 확인

#사용자 기반 협업 필터링 평가
model_to_evaluate <- "UBCF" #매개변수를 사용자 기반 협업 필터링으로 설정
model_parameters <- NULL # 기본 설정인 NULL로 설정
#모델구축
eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 10 # 추천될 아이템 수 설정
eval_prediction <- predict(object = eval_recommender, newdata = getData(eval_sets,"known"),
                           n = items_to_recommend, type = "ratings")
eval_prediction
#모델정확도 측정
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = 
                                          getData(eval_sets,"unknown"), byUser =TRUE)
head(eval_accuracy)
apply(eval_accuracy,2,mean) #전체적인 정확도
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = 
                                          getData(eval_sets,"unknown"), byUser =FALSE)
head(eval_accuracy) #전체적인 정확도: 평균제곱근오차(RMSE), 평균절대오차(MAE)
#정밀도, 재현율, F1 측정 값을 포함하는 혼동 행렬 생성
results <- evaluate(x = eval_sets, method = model_to_evaluate, n=seq(10, 100, 10))
#인덱스 요약
colums_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[,colums_to_sum] 
head(indices_summed)

plot(results, annotate = TRUE, main = "ROC curve") 
#ROC플롯: 참긍정률과 거짓긍정률의 관계 -> 균형을 유지하는 값 nn=30 선택
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall") 

