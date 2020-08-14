#아이템 기반 CF
library(recommenderlab)
data("Jester5k")
model_data = Jester5k[rowCounts(Jester5k) < 80]
model_data

#이상치제거
boxplot(rowMeans(model_data))
dim(model_data[rowMeans(model_data) < -5]) #갯수파악
dim(model_data[rowMeans(model_data) > 7])
model_data = model_data[rowMeans(model_data)>=-5 & rowMeans(model_data)<=7]#제거
model_data

#IBCF 추천인 모델 구축
which_train <- sample(x = c(TRUE, FALSE), size = nrow(model_data),
                      replace = TRUE, prob = c(0.8, 0.2)) #TRUE인 요소의 80:20 논리 객체 생성
class(which_train)
head(which_train) 
model_data_train <- model_data[which_train,] #훈련셋 생성
dim(model_data_train)
model_data_test <- model_data[!which_train, ]
dim(model_data_test)

#모델 구축
model_to_evaluate <- "IBCF"
model_parameters <- list(k = 30) #k=유사도값 계산을 위한 이웃의 수
model_recommender <- Recommender(data = model_data_train,
                                 method = model_to_evaluate, 
                                 parameter = model_parameters)
model_details = getModel(model_recommender)
str(model_details)

#추천 생성
items_to_recommend <- 10 #추천 수 생성
model_prediction <- predict(object = model_recommender, newdata=
                              model_data_test, n = items_to_recommend)#예측
model_prediction
print(class(model_prediction))
model_prediction@items[[1]] #예측 확인
recc_user_1 = model_prediction@items[[1]] #아이템 레이블 추가
jokes_user_1 <- model_prediction@itemLabels[recc_user_1]
jokes_user_1

#모델 평가
n_fold <- 4 #k-1 교차검정
items_to_keep <- 15 # 추천생성에 사용되는 아이템의 최소 갯수
rating_thredshold <- 3 # 좋은 평가로 간주되는 최소 평가
#테스트 셋 생성
eval_sets <- evaluationScheme(data = model_data, method = "cross-validation",
                              k = n_fold, given = item_to_keep, goodRating = rating_thredshold)
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets
model_to_evaluate <- "IBCF"
model_parameters <- NULL #코사인 유사도를 계산하기 위한 이웃의 수 정의
getData(eval_sets, "train") #훈련 세트를 추출하고 recommender()메소드로 전달

eval_recommender <- Recommender(data = getData(eval_sets, "train"), method =
                                  model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 10 # 추천에 사용될 아이템 수 설정정

eval_prediction <- predict(object = eval_recommender, 
                           newdata =getData(eval_sets, "known"), 
                           n = items_to_recommend, type = "ratings")
eval_prediction


#예측 모델 정확도

eval_accuracy <- calcPredictionAccuracy(  x = eval_prediction, 
                                          data = getData(eval_sets, "unknown"), 
                                          byUser = TRUE)
head(eval_accuracy)

eval_accuracy <- calcPredictionAccuracy(  x = eval_prediction, 
                                          data = getData(eval_sets, "unknown"), 
                                          byUser = FALSE)
head(eval_accuracy)

#플롯을 사용한 모델 정확도
# n=아이템 간의 유사도를 계산하기 위한 최근접 이웃 수 정의
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10,100,10))
results@results[1]  #각 폴드에 대한 모델 정확도 확인
#4개의 폴드 합산
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed) # 30과 40 에서 정확도가 좋음을 확인
plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")

#IBCF 매개변수 튜닝
vector_k <- c(5, 10, 20, 30, 40) #k 값 설정
model1 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = 
                                                 list(method = "cosine", k=k))})
names(model1) <- paste0("IBCF_cos_k_", vector_k)
names(model1)
model2 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "pearson", k =k))})
names(model2) <- paste0("IBCF_pea_k", vector_k)
names(model2)
models = append(model1,model2) # 모델 조합합
models
n_recommendations <- c(1,5, seq(10,100,10)) # 추천의 갯수 설정
list_results <- evaluate(x = eval_sets, method = models, n= n_recommendations)#4-폴드 구축
plot(list_results, annotate = c(1,2), legend = "topleft")
title("ROC curve")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")
