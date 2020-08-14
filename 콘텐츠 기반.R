#콘텐츠 기반 추천
raw_data = read.csv("C:/Users/KIM/Documents/Python Scripts/recommendation/사용자기반 CF/udata.csv",
                    sep="\t", header=F)
colnames(raw_data) = c("UserID", "MovieID", "Rating", "TimeStamp")
ratings = raw_data[,1:3]
head(ratings)
str(ratings)

movies = read.csv("C:/Users/KIM/Documents/Python Scripts/recommendation/사용자기반 CF/uitem.csv",
                  sep="|", header=F)
colnames(movies) = c("MovieID","MovieTitle","ReleaseDate","VideoReleaseDate","IMDbURL","Unknown","Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
movies = movies[,-c(2:5)]
str(movies)

ratings = merge(x = ratings, y = movies, by = "MovieID", all.x = TRUE) #평가 데이터와 아이템 데이터 병합
nrat = unlist(lapply(ratings$Rating, function(x)
  {
  if(x>3) {return(1)}
  else {return(0)}
}))#이진 범위 변수 변환(평가:1~3->0/4~5->1)
head(nrat)
ratings = cbind(ratings,nrat) #nrat과 데이터프레임의 원본 평가 결합
scaled_ratings = ratings[,-c(3,4)] #기존 평가 변수 제거
scaled_ratings = scale(scaled_ratings[,-c(1,2,21)])  #데이터 표준화:열 평균을 해당 열에서 제거
scaled_ratings = cbind(scaled_ratings,ratings[,c(1,2,23)])
set.seed(7) #훈련세트:테스트세트=80:20
which_train <- sample(x = c(TRUE, FALSE), size = nrow(scaled_ratings),
                      replace = TRUE, prob = c(0.8,0.2))
model_data_train <- scaled_ratings[which_train, ]                      
model_data_test <- scaled_ratings[!which_train, ]

#randomforest 모델 구축
library(randomForest)
fit = randomForest(as.factor(nrat)~., data = model_data_train[,-c(19,20)])
 #nrat 변수 factor 형식으로 전환
fit
summary(fit)
predictions <- predict(fit, model_data_test[,-c(19,20,21)], type="class")

#정밀도와 재현율 방식으로 모델 평가
cm = table(predictions,model_data_test$nrat)  #혼동 행렬 생성
cm
(accurary <- sum(diag(cm)) / sum(cm))
(precision <- diag(cm) / rowSums(cm))
(recall <- diag(cm) / colSums(cm))

#사용자 ID 에 대한 상위 N개 추천 생성
#활성화된 사용자에 의해 평가되지 않은 영화를 포함하는 데이터프레임 생성
totalMovieID = unique(movies$MovieID) #고유한 Movieids 추출
nonratedmoviedf = function(userid){
  ratedmovies = raw_data[raw_data$UserID==userid, ]$MovieID
  
}
#1.활성화된 사용자에의해 평가되지 않은 모든 영화를 포함하는 df 생성
 #활성사용자에 의해 평가되지 않은 영화의 데이터프레임을 생성하고 평가를 0으로 설정
nonratedmoviedf = function(userid){
  ratedmovies = raw_data[raw_data$UserID==userid, ]$MovieId
  non_ratedmovies = totalMovieID[!totalMovieID %in%
                                   ratedmovies]
  df = data.frame(cbind(rep(userid),non_ratedmovies,0))
  names(df) = c("UserID", "MovieID","Rating")
  return(df)
}

activeusernonratedmoviedf = nonratedmoviedf(943)
#2.활성화 사용자 df를 위한 프로필 구축
activeuserratings = merge(x = activeusernonratedmoviedf, y = 
                            movies, by = "MovieID", all.x = TRUE)
#3.평가 예측 및 10개의 추천 생성 및 정렬
predictions <- predict(fit, activeuserratings[,-c(1:4)],
                       type="class")
#df 생성
recommend = data.frame(movieID = 
                         activeuserratings$MovieID, predictions)
#모델에서 예측 결과가 0인 값 제거, 유효사용자 선호 아이템 추출
recommend = recommend[which(recommend$predictions == 1),]
recommend
