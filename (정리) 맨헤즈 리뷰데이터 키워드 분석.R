################ Marketing Project #####################

# 주제 : 맨헤즈 유의미한 키워드 추출

# 목차
# 1. 데이터 수집
# 2. 텍스트 데이터 파싱
# 3. 데이터 EDA
# 4. 토픽 모델링
# 5. 인사이트 추출
# 6. 마케팅 전략 제안 제시

# 1. 데이터 수집 

# 맨헤즈 REVIEWS 데이터 수집

# 패키지 세팅하기
library(tidyverse)
library(rJava)
library(NLP4kec)
library(tm)
library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(slam)
library(RcppMeCab)
library(Rmpfr)
library(httr) # GET함수에 필요한 패키지
library(rvest) #read_html
library(ggplot2)


# 1- 1). Web Crawling 자동화

# 목표 : 리뷰제목, 리뷰 제품명, 리뷰 제품 가격, 리뷰 텍스트 데이터로 만들기

# 데이터 수집 가능한지 확인하기
res <- GET(url = 'https://www.menhez.co.kr/board/view.php?page=1&bdId=goodsreview&sno=933')
# HTTP 응답 상태코드를 확인합니다. 
status_code(x = res)



result <- data.frame()

collect_review <- function(page_number){
  for(i in 0:14) { 
    # 한 페이지당 15 개의 리뷰
    # 페이지수와 상관이 없이 sno넘버에 따라 달라져도 이상이 없다
    
    reviews2 <- data.frame()
    
    res_1 <- GET(url = 'https://www.menhez.co.kr/board/view.php',
                 query = list(page=page_number,
                              bdId= 'goodsreview',
                              sno = 927-(i+15*(page_number-1))))
    html <- read_html(res_1)
    
    tryCatch({
      
      reviews_title <- html_nodes(x=html,  css = 'div.board_view_tit_review_pc') %>% html_text(trim=TRUE) 
      if(length(reviews_title) == 0) reviews_title <- ""
      
      reviews_item <- html_nodes(x=html,  css = 'span.view_select_item_info > em') %>% html_text(trim=TRUE)
      if(length(reviews_item) == 0) reviews_item <- ""
      
      reviews_item_price <- html_nodes(x=html,  css = 'span.view_select_item_info>strong') %>% html_text(trim=TRUE) 
      reviews_item_price <- gsub(pattern = "[\n\r\t]",   
                                 replacement = "",
                                 x = reviews_item_price) #여백 삭제
      reviews_item_price <- gsub(pattern = "판매금액",   
                                 replacement = "",
                                 x = reviews_item_price) #'판매금액' 삭제
      if(length(reviews_item_price) == 0) reviews_item_price <- 0
      
      reviews_text <- html_nodes(x=html,  css = 'div.seem_cont') %>% html_text(trim=TRUE) 
      reviews_text <- gsub(pattern = "[\n\r\t]",   
                           replacement = "",
                           x = reviews_text) #여백 삭제
      if(length(reviews_text) == 0) reviews_text <- ""
      
      
      reviews2 <- cbind(reviews_title, reviews_item, reviews_item_price,reviews_text)
      
      
      result <- rbind(result, reviews2)
     
    }, error = function(e) cat('-> 에러가 발생했습니다! \n'))
  }
  
  Sys.sleep(0.5)
  # 데이터 수집하는데 바로 넘어갈 수 있어서 대비장치
  
  return(result)
}



review_data <- data.frame()

# 총 60페이지 -> 중간에 그냥 넘어가는 데이터가 있어서 65페이지로 해놓음
for(i in 1:65){
  review_data <- rbind(review_data, collect_review(i))
  cat('리뷰 : ', i, '페이지 데이터 수집 완료 \n')
}

# 빈 행 삭제
review_data2 <- review_data[-c(which(review_data$reviews_title == "")), ]

# 데이터 수집한 파일 Rdata파일로 저장함
save(review_data2, file = "/Users/moon-il/Work_Space/FastCampus_marketing/나의 강점 적용(데이터 분석)/review_data2.Rdata")

# 데이터 수집 완료


# 데이터 정제하기

# 리뷰 데이터 불러오기
setwd("/Users/moon-il/Work_Space/FastCampus_marketing/나의 강점 적용(데이터 분석)")
load(file = 'review_data2.Rdata')

# 제품과 가격만 따로 데이터로 만들어 놓기
review_data_item<- data.frame(review_data2$reviews_item, review_data2$reviews_item_price)
names(review_data_item) <- c("item", "price")





# 보완할점
# 1.. 확실하게 함수를 못 짬 -> 그래서 그나마 커버 완료!!
#   - 리뷰 없음 문제
#   - sno 넘버문제 
#      - sno넘버 기준으로 코딩을 짜서 페이지수와 매치가 안 맞을 수 있다!! 그 이유는 sno넘버 중에 리뷰글 하나가 빠질 경우 숫자가 안 맞게 된다





# 2. 텍스트 데이터 파싱

# 텍스트 데이터 전처리 -> 형태소 분석 -> 말뭉치 생성 -> DTM생성 


# 2-1. 형태소 분석(Parsing)


# 리뷰 제목

# 리뷰 제목과 리뷰 글 한줄로 만들어주기


title <- levels(review_data2$reviews_item)
title <- title[-11]

review_data2 <- as.character(review_data$reviews2)
# character형으로 만들어야 형태소 분석 함수 가능!!

# 형태소 분석 

# 최종 담을 데이터
word <- data.frame()

word_split <- function(data){
  for(i in 1:length(title)){
 
    cat(i, '번째 진행중! \n')
    title_info <- data %>% dplyr::filter(reviews_item == title[i])
    title_info$reviews_title <- as.character(title_info$reviews_title)
    title_info$reviews_text <- as.character(title_info$reviews_text)
    
    parse <- pos(title_info$reviews_title, format = 'data.frame') %>% filter(pos %in% c('NNG', 'VV', 'VA'))
    parse2 <- pos(title_info$reviews_text, format = 'data.frame') %>% filter(pos %in% c('NNG', 'VV', 'VA'))
    parse <- rbind(parse, parse2)
    
    tryCatch({
      # 리뷰 한줄로 이어 붙이기
      review_parse <- ''
      for(j in 1:nrow(parse)){
        review_parse <- str_c(review_parse, parse$token[j], ' ')
      }
    }, error = function(e) cat('-> 에러가 발생했습니다!\n'))
    
    # 뒤에 단어 완성 시켜주는 파싱
    # ex) 알 -> 알다, 잊 -> 잊다
    parse_step <- r_parser_r(contentVector = review_parse, language = 'ko')
    
    # 데이터 합치기
    word <- rbind(word, data.frame(title[i], parse_step))
    
  }
  
  # 데이터 반환
  return(word)
}


word <- word_split(review_data2)

# 파싱 데이터 저장
save(word, file = "/Users/moon-il/Work_Space/FastCampus_marketing/나의 강점 적용(데이터 분석)/word.Rdata")



# 2-2. 불용어 사전 구축

# 불용어 사전

stopWD <- data.frame(stopword = c("없다", "많다", "받다", "가다", "맞다", "있다", "알다", "같다", "좋다", "괜찮다", "만족", "구매", "피부", "바르", "추천", "사용", "화장품", "생각", "얼굴", "정도", "이용", "끌다", "다행", "모르", "진하", "디즈", "감사", "강추", "예정" ))
# 불용어 선택 기준
# 1. 지금 소구포인트를 찾기 위해 
# 2. 일반적인 호감표현은 제거

# 불용어 사전 확인
stopWD$stopword

stopWD$stopword <- as.character(stopWD$stopword)
# character형으로 형변환해줘야 불용어 처리 가능


# 보완할점
# 1. stopWD csv파일 저장하고 불러온다음에 하면 안 됨... -> 커버가능


# 2-3. 말뭉치 만들기
corpus <- word$parse_step %>% VectorSource() %>% VCorpus()

# 2-4. DTM 만들기
dtm <- DocumentTermMatrix(x=corpus, control = list(removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stopwords = stopWD$stopword,
                                                   wordLengths = c(2, Inf)))

colnames(dtm) <- trimws(colnames(dtm)) # terms단어에서 공백 제거

cat('Before : ', dim(dim)[1], '행', dim(dim)[2], '열 차원입니다! \n')

dtm_clean <- removeSparseTerms(dtm, sparse = as.numeric(x = 0.99))
# 희소용어 제거

cat('After : ', dim(dtm_clean)[1], '행', dim(dtm_clean)[2], '열 차원입니다! \n')

# dtm 확인
inspect(dtm_clean)

# 용어가 포함되지 않는 행 제거
rowTotals <- apply(dtm_clean , 1, sum) #Find the sum of words in each Document
dtm_clean <- dtm[rowTotals> 0, ]   




# 3. 데이터 EDA 및 시각화

# 3-1. 데이터 EDA

# 3-2. 데이터 시각화

# 제품 판매 시각화
review_data_item
review_data_item$item <- as.character(review_data_item$item)
review_data_item$price 
s <- gsub("원", "", review_data_item$price)
s <- gsub(",", "", s)
s <- as.integer(s)
review_data_item$price <- s


review_data_item %>% group_by(review_data_item$item)

table(review_data_item$item)
# 페이셜밸런싱 올인원 > 딥모이스쳐 올인원 > 파워 리페어 워터크림 

barplot(table(review_data_item$item))


getwd()

write.csv(review_data_item,
          file      = "review_data_item.csv",
          row.names = FALSE)


# DTM 시각화
library(tidytext)
dtm_data <- tidy(dtm_clean)

write.csv(dtm_data,
          file      = "dtm_data.csv",
          row.names = FALSE)



# 4. 토픽 모델링

#  harmonic mean 방법으로 적절한 k값(토픽수) 찾기

harmonicMean <- function(logLikelihoods, precision = 2000){
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

seqk <- seq(18, 22, 1)
burnin <- 1000
iter <- 1000
keep <- 50


fitted_many <- lapply(seqk, function(k) LDA(dtm_clean, 
                                            k = k,
                                            method = "Gibbs",
                                            control = list(burnin = burnin, iter = iter, keep = keep)))

logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ggplot(data.frame(seqk, hm_many), aes(x= seqk, y = hm_many)) + geom_path(lwd = 1.5) + 
  theme(text = element_text(family = NULL),
        axis.title.y = element_text(vjust = 1, size = 16),
        axis.title.x = element_text(vjust = -.5, size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  xlim(17, 22) + 
  ylim(-9500.00, -9250.00) +
  ggplot2::annotate('text', x = 9, y = 199000,
                    label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) + 
  labs(title = "Latent Dirichlet Allocation Analysis", subtitle = "How many distinct topics?")

# k = 21 -> 토픽 주제수

# 보완할 점
# 주석이 안 보임...ㅠㅠ



# K= 5

topic_LDA <- function(data, k){
  term_tfidf = tapply(data$v / row_sums(data)[data$i], data$j, mean)*log2(nDocs(data) / col_sums(data>0))
  
  control_LDA_Gibbs = list(
    seed = 100,
    verbose = 0,
    save = 0,
    prefix = tempfile(),
    nstart = 1,
    best = TRUE,
    keep = 0,
    estimate.beta = TRUE,
    
    #LDAcontrol
    alpha = 0.1,
    delta = 0.1,
    iter = 5000,
    thin = 2000,
    burnin = 0)
  
  lda_tm = LDA(data, k, method = "Gibbs", control = control_LDA_Gibbs)
  
  term_topic = terms(lda_tm, 30)
  
  save(lda_tm, file='lda_tm.Rdata')
  print(term_topic)
  
}

topic_LDA(dtm_clean, 5)

load(file = 'lda_tm.RData')

term_topic <- terms(lda_tm, 20)

print(term_topic)

# 토픽 모델링 데이터 저장하기
term_topic_data <- data.frame(term_topic)
write.csv(term_topic_data,
          file      = "term_topic_data.csv",
          row.names = FALSE)


# 보완할점
# 1. 전처리 문제...
# -> 제목, 금액, 목록, 작성자, 판매, 날짜, 번호, 삭제, 수정, 같다


# LDA 분석하기

# Topic 1 : 크림, 수분, 재생     -> 수분 크림에 관심있는 고객
# Topic 2 : 후기, 동생, 제품     -> 테스트 겸 한번 사보는 고객
# Topic 3 : 건조, 보습, 건성     -> 건성 화장품에 관심있는 고객
# Topic 4 : 얼굴, 지성, 개기름   -> 지성 화장품에 관심있는 고객
# Topic 5 : 선물, 배송, 빠르다   -> 누군가에게 선물해주기 위해 배송을 할 때




# LDA 시각화

doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$name = dtm_clean$dimnames$Docs

doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)

doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)

doc_Prob_df$name = doc_topic_df$name

id_topic = merge(doc_topic_df, doc_Prob_df, by = 'name')

head(id_topic, 10)

# LDA 시각화

phi = posterior(lda_tm)$term %>% as.matrix

theta = posterior(lda_tm)$topics %>% as.matrix()

vocab = colnames(phi)

new_dtm = as.matrix(dtm_clean)
freq_matrix <- data.frame(word = colnames(new_dtm),
                          Freq = colSums(new_dtm))


## 폰트 설정 패키지 설치

#install.packages("extrafont")
# 
library(extrafont)
# 
# 
# 
# ## 존재하는 모든 폰트 불러오기
# 
# font_import()
# 
# ## 폰트 설정
# 
theme_set(theme_gray(base_family='NanumGothic'))
# 
# ## 혹은
# 
# theme_set(theme_gray(base_family='AppleMyungjo'))
# 
# theme.ti <- element_text(family="AppleMyungjo", face="bold", size=12) #그래프 제목 스타일 변경
# 
# theme.ax <- element_text(family="AppleMyungjo", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
# 
# theme.leti<-element_text(family="AppleMyungjo", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
# 
# theme.lete<-element_text(family="AppleMyungjo") 


freq_matrix %>% 
  arrange(desc(Freq)) %>% 
  head(10) %>% 
  ggplot(mapping = aes(x = reorder(word, -Freq), y = Freq, fill = word)) +
  geom_bar(stat = 'identity') 
# ggthemes::theme_wsj()

# 해석 
# 선물 > 뱁송 > 크림 > 빠르다 > 올인원 > 제품 > 건조 > 수분  




# 5. 인사이트 추출

print(term_topic)


# 소구포인트

# Topic 1 : 크림, 수분, 재생     -> 수분 크림에 관심있는 고객
# Topic 2 : 얼굴, 지성, 개기름   -> 지성 화장품에 관심있는 고객
# Topic 3 : 건조, 보습, 건성     -> 건성 화장품에 관심있는 고객
# Topic 4 : 후기, 동생, 제품     -> 제품에 대해 관심있는 고객 
# Topic 5 : 선물, 배송, 빠르다   -> 누군가에게 선물해주기 위해 배송을 할 때


term_topic <- terms(lda_tm, 20)

print(term_topic)


# 6. 마케팅 전략 제안

# Topic 1    Topic 2    Topic 3  Topic 4  Topic 5 
# [1,] "크림"     "동생"     "건조"   "지성"   "선물"  
# [2,] "수분"     "관리"     "보습"   "개기름" "빠르다"
# [3,] "재생"     "기대"     "건성"   "기름기" "배송"  
# [4,] "올인원"   "끌다"     "가격"   "피지"   "제품"  
# [5,] "보습"     "나쁘다"   "배송"   "흡수"   "올인원"
# [6,] "저녁"     "편하다"   "중건"   "기름"   "남자"  
# [7,] "흡수"     "아침"     "타입"   "조절"   "포장"  
# [8,] "아침"     "자극"     "향도"   "타입"   "친구"  
# [9,] "믿다"     "진하"     "남편"   "분기"   "남친"  
# [10,] "끌다"     "인원"     "순하다" "생기"   "택배"  
# [11,] "남성"     "모르"     "스킨"   "예정"   "이쁘다"
# [12,] "개기름"   "하루"     "이벤트" "번들"   "후기"  
# [13,] "건조"     "느낌"     "저녁"   "기대"   "주문"  
# [14,] "기름종이" "디즈"     "감사"   "친환경" "마음"  
# [15,] "느낌"     "발리"     "강추"   "오후"   "가격"  
# [16,] "바르다"   "저녁"     "매트"   "이틀"   "감사"  
# [17,] "다행"     "기존"     "선물"   "케이스" "고급"  
# [18,] "성분"     "스킨로션" "아빠"   "크림"   "세트"  
# [19,] "세트"     "여름"     "아침"   "기능"   "생일"  
# [20,] "스며들다" "일반"     "의사"   "기초"   "찾다"  


# 각 키워드를 구글 GDN 키워드별 최적화하는데 사용할 예정입니다. 
