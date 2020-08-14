#상황 정의(타임스탬프이용) 
raw_data = read.csv("C:/Users/EunBi/Documents/R script/ml-100k/udata.csv",
                    sep="\t", header=F)
colnames(raw_data) = c("UserId", "MovieID", "Rating", "TimeStamp")

movies = read.csv("C:/Users/EunBi/Documents/R script/ml-100k/uitem.csv",
                  sep="|", header=F)
colnames(movies) = c("MovieID","MovieTitle","ReleaseDate","VideoReleaseDate","IMDbURL","Unknown","Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
movies = movies[,-c(2:5)]
ratings_ctx = merge(x = raw_data, y = movies, by = "MovieID", all.x = TRUE)

#사용자 상황 프로필 생성
ts = ratings_ctx$TimeStamp #평가 데이터 세트에서 타임스탬프 추출
hours <- as.POSIXlt(ts, origin = "1960-10-01")$hour #날짜객체변환 및 하루중 시간 추출
ratings_ctx = data.frame(cbind(ratings_ctx,hours)) #평가 데이터 세트에 시간 추가

#ID가 943인 사용자 프로필 생성
UCP =  ratings_ctx[(ratings_ctx$UserId == 943), ][,-c(2,3,4,5)]
 # 943사용자의 평가 정보 추출(사용자ID, 영화ID, 평가, 타임스탬프 열 제거)
UCP_pref = aggregate(.~hours, UCP[,-1], sum)
 # 모든 아이템 열 연산: 시간대 아이템 특징에 대한 선호도 계산
 # hour을 기준으로 모든 열의 합계 계산
UCP_pref_sc = cbind(context = UCP_pref[,1],t(apply(UCP_pref[,-1], 1,
                                                   function(x)(x-min(x))/(max(x)-min(x)))))
 #0~1으로 일반화 

#상황인식추천 생성(콘텐츠기반 추천시스템 이용)
#고객 특징 프로필 생성
ratings = raw_data[,1:3]
ratings = merge(x = ratings, y = movies, by = "MovieID", all.x =TRUE)
nrat = unlist(lapply(ratings$Rating, function(x)
  {
  if(x>3) {return(1)}
  else {return(0)}
}))
head(nrat)
 #평가를 이진번위 변수로 변환
 #unlist 함수: 리스트 -> 벡터화, lapply함수: 행렬의 행/열으로 특정 함수 적용
ratings = cbind(ratings,nrat)
 #범주회된 df와 원본 평가 결합
scaled_ratings = ratings[,-c(3,4)] 
 #기존 평가 점수 제거
scaled_ratings = scale(scaled_ratings[,-c(1,2,21)]) 
scaled_ratings = cbind(scaled_ratings, ratings[,c(1,2,23)])
 #데이터 표준화-> scale함수:열평균을 각 열에서 제거
set.seed(7)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(scaled_ratings),
                      replace = TRUE, prob = c(0.8, 0.2))
model_data_train <- scaled_ratings[which_train, ]
model_data_test <- scaled_ratings[!which_train, ]
 #훈련셋 테스트셋 
install.packages("randomForest")
library(randomForest)
fit = randomForest(as.factor(nrat)~., data = model_data_train[,-c(19,20)])
 #이진분류 randomforest 사용<- nart 변수를 factor로 전환
