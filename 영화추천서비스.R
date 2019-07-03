# 논문


# 현재 할 일
# -  rJava 다시 설치시도해보기
# - 시각화 글자 깨짐현상



# 크롤링부분 안해도됨


library(tidyverse)
library(rJava)  # 왜 안되냐..!!!
library(NLP4kec)
library(tm)
library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(slam)
library(RcppMeCab)
library(Rmpfr)




getwd()
setwd("/Users/moon-il/Desktop/학교/논문/자료/")
#Sys.setlocale('LC_ALL','C')
load(file = 'result.RData')
str(result)






result$review[2]

# (1) 평점 4.5이상만 추출
result <- result %>% 
  dplyr::filter(rate >= 4.5)

result2 <- result %>% 
  dplyr::filter(rate >= 5.0)

str(result2)

# (2) 열명 변경
rownames(result) <- seq(from = 1, to = nrow(result), by = 1)

# (3) review 타입 변경
result$review <- as.character(result$review)


theme_set(theme_gray(base_family="AppleGothic"))

result %>% group_by(movie_name) %>% summarise(n = n()) %>%
  arrange(desc(n)) %>% head(1000) %>%
  ggplot(mapping = aes(x = reorder(movie_name, -n), y = n, fill = movie_name)) +
  geom_bar(stat = 'identity') +
  ggtitle('Reviews Top10 Movie') +
  geom_text(aes(label = n), vjust = -0.1) +
  xlab("Movie name") + ylab("Review Count")

# 문제 : 이름이 한글이 깨져서 나옴

theme_set(theme_gray(base_family="AppleGothic"))

result %>% filter(rate >= 5.0) %>% group_by(movie_name) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% head(10) %>% 
  ggplot(mapping = aes(x = reorder(movie_name, -n), y = n, fill = movie_name)) +
  geom_bar(stat = 'identity') + xlab("Movie name") + ylab("Review Count") +
  ggtitle('Movies rated over 4.5?')



result_na_omited <- na.omit(result)
result_final <- result_na_omited

# 생년월일 계산해서 나이대 추출
birth <- str_sub(result_final$birth, 1, 4)
birth <- as.numeric(birth)


# 현재연도에서 생년월일 빼고 1더하기
result_final$age <- 2019 - birth + 1

result_final$age <- ifelse(result_final$age < 20, 10,
                           ifelse(result_final$age < 30, 20,
                                  ifelse(result_final$age < 40, 30,
                                         ifelse(result_final$age < 50, 40,
                                                ifelse(result_final$age < 60, 50,
                                                       ifelse(result_final$age < 70, 60, 70))))))

# 나이대를 factor로 만들어주기
result_final$age <- as.factor(result_final$age)



#install.packages("ggthemes")
library(ggthemes)
result_final %>%
  dplyr::filter(gender != 'not_set', rate >= 4.5) %>%
  group_by(gender, age) %>%
  summarise(n = n()) %>% 
  ggplot(mapping = aes(x = age, y = n, fill = gender)) +
  geom_bar(stat= 'identity', position = 'dodge') +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.1) +
  ggthemes::theme_wsj()




result_final %>% group_by(age) %>% summarise(n = n()) %>% 
  ggplot(mapping = aes(x = age, y = n, fill = age)) +
  geom_bar(stat = "identity") + xlab("age") + ylab("") +
  geom_text(aes(label = n), vjust = -0.1) +
  ggtitle("Age who rated over 4.5") +
  scale_fill_brewer(palette = "Set2") +
  ggthemes::theme_wsj()


result_final %>% filter(gender != 'not_set') %>% group_by(gender) %>%
  summarise(n = n()) %>% 
  ggplot(mapping = aes(x = gender, y = n, fill = gender)) +
  geom_bar(stat = 'identity') + xlab("gender") + ylab("") +
  ggtitle("Gender who rated over 4.5") +
  scale_fill_brewer(palette = 'Set1') +
  geom_text(aes(label = n), vjust = -0.1) +
  ggthemes::theme_wsj()




# data 불러오기
load(file = 'final.RData')

# 구조 확인
head(word, 20)



load(file = 'dtm_clean.RData')

# DTM 확인 
inspect(dtm_clean)




harmonicMean <- function(logLikelihoods, precision = 2000){
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec=precision) + llMed))))
}

seqk <- seq(18, 22, 1)
burnin <- 1000
iter <- 1000
keep <- 50





load(file = 'lda_tm.RData')

term_topic = terms(lda_tm, 30)

print(term_topic)





# 문서별 토픽 번호 저장하기
doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$name = dtm_clean$dimnames$Docs

# 문서별 토픽 확률값 계산하기
doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)

# 최대 확률값 찾기
doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)


#문서별 토픽번호 및 확률값 추출하기
doc_Prob_df$name = doc_topic_df$name # 확률 데이터 프레임에도 나중에 조인을 위해 영화를 할당해준다

id_topic = merge(doc_topic_df, doc_Prob_df, by = "name")

head(id_topic, 10)


# LDA 시각화 
phi = posterior(lda_tm)$term %>% as.matrix

theta = posterior(lda_tm)$topics %>% as.matrix

# vocab는 전체 단어 리스트 입니다.
vocab = colnames(phi)

# 각 단어별 빈도수를 구합니다.
new_dtm = as.matrix(dtm_clean)
freq_matrix = data.frame(word = colnames(new_dtm),
                         Freq = colSums(new_dtm))

theme_set(theme_gray(base_family="AppleGothic"))

freq_matrix %>% 
  arrange(desc(Freq)) %>% 
  head(10) %>% 
  ggplot(mapping = aes(x = reorder(word, -Freq), y = Freq, fill = word)) +
  geom_bar(stat = "identity")

#+ 
#  ggthemes::theme_wsj()






load(file = 'dtm_clean_userDT.RData')

# 토픽별 단어 data.frame()
load(file = 'lda_DT.RData')

# user별 고빈도 사용단어 추출 후, 토픽 추천함수
# user별  1번 이상 사용단어 logical형태로 추출

# Random으로 1명 무작위 추출 
sample_userN <- sample(rownames(dtm_clean_user), 1)
sample_userN 

dtm_clean_user_index <- dtm_clean_user[rownames(dtm_clean_user) == sample_userN,] > 0
a <- dtm_clean_user[rownames(dtm_clean_user) == sample_userN, dtm_clean_user_index]
a

b <- colnames(a)
b

# user별 단어 추출 후 lda_dt와 비교해서 토픽 뽑기
max <- Inf
for(j in 1:20){
  sum <- 0
  for(i in 1:7235){
    if(lda_DT[i,j] %in% b){
      sum <- sum + i * 1 / a[lda_DT[i, j] %>% as.character()]
      av <- sum / length(b)
    }
  }
  if(av < max){
    topic <-  colnames(lda_DT)[j]
    max <- av
  }
}

# lda_tm 각 토픽별로 포함되는 영화 추출하기
doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$name = dtm_clean$dimnames$Docs
doc_topic_df <- doc_topic_df[,c(2,1)]

# 정규표현식 이용하여 나온 토픽의 숫자만 남기기
topic <- topic %>% str_extract(pattern = '\\d+')

# 토픽별 추천 영화 뽑기
Re_MN <- doc_topic_df %>% 
  dplyr::filter(doc_topic == topic) %>% 
  head(5) %>% 
  select(name) %>% as.vector()

cat(sample_userN, '님께 추천해드리는 영화는 아래와 같습니다.\n')
print(Re_MN$name)

