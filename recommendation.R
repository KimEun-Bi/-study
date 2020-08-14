#기본추천엔진구축
install.packages("dplyr")
install.packages("path/to/file/dplyr_0.5.0.tar.gz", reepos=NULL)
ratings = read.csv("C:/Users/KIM/Documents/R/movie_rating.csv")
head(ratings)
dim(ratings)
str(ratings)
install.packages("reshape2")
library(reshape2)
movie_ratings = as.data.frame(acast(ratings, title~critic, value.var="rating"))
View(movie_ratings)

#유사도계산
sim_users = cor(movie_ratings[,1:6],use="complete.obs")

#사용자의 등급 예측_평가하지 않은 데이터 추출
install.packages("data.table")
library("data.table")
rating_critic = setDT(movie_ratings[colnames(movie_ratings)[6]],keep.rownames = TRUE)[]
names(rating_critic) = c('title','ratiing')
View(rating_critic)

#사용자의 등급 예측_평가되지 않은 영화들만 분리
#- na 제거
titles_na_critic = rating_critic$title[is.na(rating_critic$ratiing)]
titles_na_critic
#- 영화를 평가한 리뷰어 선정
ratings_t = ratings[ratings$title %in% titles_na_critic,]
View(ratings_t)
#- 유사도 열 추가
x = (setDT(data.frame(sim_users[,6]),keep.rownames = TRUE)[])
names(x) = c('critic','similarity')
ratings_t =  merge(x = ratings_t, y = x, by = "critic", all.x = TRUE)
View(ratings_t)

#등급과 유사도 값을 곱한 후, 새로운 변수에 추가
ratings_t$sim_rating = ratings_t$rating*ratings_t$similarity
View(ratings_t)

# toby의 예상등급 구하기
install.packages("dplyr")
library("dplyr")
result = ratings_t%>% group_by(title)%>%
  summarise(sum(sim_rating)/sum(similarity))
result

#함수확장
generateRecommendations <- function(userID){
  rating_critic = setDT(movie_ratings[colnames(movie_ratings)[userID]], keep.rownames = TRUE)[]
  names(rating_critic) = c('title','rating')
  titles_na_critic =
    ratings_t = ratings[ratings$title %in% titles_na_critic,]
  x = (setDT(data.frame(sim_users[,userID], keep.rownames = TRUE)[])
       names(x) = c('critic','similarity')
       ratings_t = merge(x = ratings_t, y= x, by = "critic", all.x = TRUE)
  ratings_t$sim_rating = ratings_t$rating*ratings_t$similarity
  result = ratings_t %>% group_by(title) %>% summarise(sum(sim_rating)/sum(similarity))
  return(result)
}

generateRecommendations <- function(userId){
  rating_critic  = setDT(movie_ratings[colnames(movie_ratings)[userId]],keep.rownames = TRUE)[]
  names(rating_critic) = c('title','rating')
  titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
  ratings_t =ratings[ratings$title %in% titles_na_critic,]
  #add similarity values for each user as new variable
  x = (setDT(data.frame(sim_users[,userId]),keep.rownames = TRUE)[])
  names(x) = c('critic','similarity')
  ratings_t =  merge(x = ratings_t, y = x, by = "critic", all.x = TRUE)
  #mutiply rating with similarity values
  ratings_t$sim_rating = ratings_t$rating*ratings_t$similarity
  #predicting the non rated titles
  result = ratings_t %>% group_by(title) %>% summarise(sum(sim_rating)/sum(similarity))
  return(result)
}

#유클리드
x1 <- rnorm(30)
x2 <- rnorm(30)
Euc_dist = dist(rbind(x1,x2),method = "euclidean")

#코사인
vec1 = c(1,1,1,0,0,0,0,0,0,0,0,0)
vec2 = c(0,0,1,1,1,1,1,0,1,0,0,0)
install.packages("lsa")
library(lsa)
cosine(vec1, vec2)

#자카드 유사도
install.packages("clusteval")
library(clusteval)
cluster_similarity(vec1, vec2, similarity = "jaccard")

#피어슨 상관계수
Coef = cor(mtcars, method="pearson")

#MF
install.packages("Biobase")
install.packages("NMF")
install.packages("recommenderlab")
library(recommenderlab)
library(NMF)
# MF
library(recommenderlab)
data("MovieLense")
dim(MovieLense)
# NMF를 사용해 MF에 적용
mat = as(MovieLense,"matrix")
mat[is.na(mat)] = 0
res = nmf(mat,10)
res
# 적합한 값
r.hat <- fitted(res)
dim(r.hat)
p <- basis(res)
dim(p)
q <- coef(res)
dim(q)

sampleMat <- function {(n <- 1:n; 1/outer(i -1, i, "+")}
original.mat
)

library(MASS)
data("Boston")  
set.seed(0)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(Boston),
                      replace = TRUE, prob = c(0.8, 0.2))
train <- Boston[which_train, ]
test <- Boston[!which_train, ]
lm.fit = lm(medv~. , data=train)
summary(lm.fit)
pred = predict(lm.fit, test[,-14])
