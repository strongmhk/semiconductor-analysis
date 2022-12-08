library(multilinguer)
library(stringr) 
library(wordcloud2) 
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(ggthemes)
library(olsrr)
library(ggplot2) 
library(dplyr) 
library(tm)
library(magrittr)
library(patchwork)
library(GGally)
library(gridExtra)
library(knitr)
library(kableExtra)
library(prettydoc)
library(tidyr)



##LG dis

dis_LG <- file("C:/Users/dnjs1/Downloads/Rr/dis_n(LG).txt")
dis_LG_r<-readLines(dis_LG)
str(dis_LG_r)
head(dis_LG_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


d_L_r<- sapply(dis_LG_r, exNouns)
d_L_r[2]


#말뭉치 Corpus 생성

d_L_r_cpus<-Corpus(VectorSource(d_L_r))
d_L_r_cpus


#데이터 전처리

d_L_r_cpus_prepro<-tm_map(d_L_r_cpus,removePunctuation) #문장부호제거

d_L_r_cpus_prepro<-tm_map(d_L_r_cpus_prepro,removeNumbers) #숫자 제거

d_L_r_cpus_prepro<-tm_map(d_L_r_cpus_prepro,tolower) #영문자 소문자 변경

d_L_r_cpus_prepro<-tm_map(d_L_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(d_L_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
d_L_r_cpus_prepro_term<-TermDocumentMatrix(d_L_r_cpus_prepro, control = list(wordLengths=c(4,16)))

d_L_r_cpus_prepro_term

#matrix -> data.frame으로 변경
d_L_r_cpus_df<-as.data.frame(as.matrix(d_L_r_cpus_prepro_term))

dim(d_L_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(d_L_r_cpus_df), decreasing = T)
wordResult


#워드 클라우드 시각화
word_names<-names(wordResult)
wordcloud(names(wordResult[1:30]),wordResult[1:30]) # 상위 30개만 뽑아서 추출

#단어 이름과 빈도수로 df 생성
word_df<-data.frame(word = names(wordResult[1:40]), freq= wordResult[1:40])

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df$word, word_df$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")







##LG led

led_LG <- file("C:/Users/dnjs1/Downloads/Rr/led(LG).txt")
led_LG_r<-readLines(led_LG)
str(led_LG_r)
head(led_LG_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


l_L_r<- sapply(led_LG_r, exNouns)
l_L_r[2]


#말뭉치 Corpus 생성

l_L_r_cpus<-Corpus(VectorSource(l_L_r))
l_L_r_cpus


#데이터 전처리

l_L_r_cpus_prepro<-tm_map(l_L_r_cpus,removePunctuation) #문장부호제거

l_L_r_cpus_prepro<-tm_map(l_L_r_cpus_prepro,removeNumbers) #숫자 제거

l_L_r_cpus_prepro<-tm_map(l_L_r_cpus_prepro,tolower) #영문자 소문자 변경

l_L_r_cpus_prepro<-tm_map(l_L_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(l_L_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
l_L_r_cpus_prepro_term<-TermDocumentMatrix(l_L_r_cpus_prepro, control = list(wordLengths=c(4,16)))

l_L_r_cpus_prepro_term

#matrix -> data.frame으로 변경
l_L_r_cpus_df<-as.data.frame(as.matrix(l_L_r_cpus_prepro_term))

dim(l_L_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(l_L_r_cpus_df), decreasing = T)
wordResult


#워드 클라우드 시각화
word_names<-names(wordResult)
wordcloud(names(wordResult[1:40]),wordResult[1:40]) # 상위 30개만 뽑아서 추출

#단어 이름과 빈도수로 df 생성
word_df<-data.frame(word = names(wordResult[1:40]), freq= wordResult[1:40])

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))

#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df$word, word_df$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


##LG RAM

RAM_LG <- file("C:/Users/dnjs1/Downloads/Rr/RAM(LG).csv")
RAM_LG_r<-readLines(RAM_LG)
str(RAM_LG_r)
head(RAM_LG_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


r_L_r<- sapply(RAM_LG_r, exNouns)
r_L_r[2]


#말뭉치 Corpus 생성

r_L_r_cpus<-Corpus(VectorSource(r_L_r))
r_L_r_cpus


#데이터 전처리

r_L_r_cpus_prepro<-tm_map(r_L_r_cpus,removePunctuation) #문장부호제거

r_L_r_cpus_prepro<-tm_map(r_L_r_cpus_prepro,removeNumbers) #숫자 제거

r_L_r_cpus_prepro<-tm_map(r_L_r_cpus_prepro,tolower) #영문자 소문자 변경

r_L_r_cpus_prepro<-tm_map(r_L_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(r_L_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
r_L_r_cpus_prepro_term<-TermDocumentMatrix(r_L_r_cpus_prepro, control = list(wordLengths=c(4,16)))

r_L_r_cpus_prepro_term

#matrix -> data.frame으로 변경
r_L_r_cpus_df<-as.data.frame(as.matrix(r_L_r_cpus_prepro_term))

dim(r_L_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(r_L_r_cpus_df), decreasing = T)
wordResult


#워드 클라우드 시각화
word_names<-names(wordResult)
wordcloud(names(wordResult[1:40]),wordResult[1:40]) # 상위 40개만 뽑아서 추출

#단어 이름과 빈도수로 df 생성
word_df<-data.frame(word = names(wordResult[1:30]), freq= wordResult[1:30])

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))

#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df$word, word_df$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")




##LG transistor


Tran_LG <- file("C:/Users/dnjs1/Downloads/Rr/Transistor(LG).csv")
Tran_LG_r<-readLines(Tran_LG)
str(Tran_LG_r)
head(Tran_LG_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


t_L_r<- sapply(Tran_LG_r, exNouns)
t_L_r[2]


#말뭉치 Corpus 생성

t_L_r_cpus<-Corpus(VectorSource(t_L_r))
t_L_r_cpus


#데이터 전처리

t_L_r_cpus_prepro<-tm_map(t_L_r_cpus,removePunctuation) #문장부호제거

t_L_r_cpus_prepro<-tm_map(t_L_r_cpus_prepro,removeNumbers) #숫자 제거

t_L_r_cpus_prepro<-tm_map(t_L_r_cpus_prepro,tolower) #영문자 소문자 변경

t_L_r_cpus_prepro<-tm_map(t_L_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(t_L_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
t_L_r_cpus_prepro_term<-TermDocumentMatrix(t_L_r_cpus_prepro, control = list(wordLengths=c(4,16)))

t_L_r_cpus_prepro_term

#matrix -> data.frame으로 변경
t_L_r_cpus_df<-as.data.frame(as.matrix(t_L_r_cpus_prepro_term))

dim(t_L_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(t_L_r_cpus_df), decreasing = T)
wordResult


#워드 클라우드 시각화
word_names<-names(wordResult)
wordcloud(names(wordResult[1:40]),wordResult[1:40]) # 상위 40개만 뽑아서 추출

#단어 이름과 빈도수로 df 생성
word_df<-data.frame(word = names(wordResult[1:40]), freq= wordResult[1:40])

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))

#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df$word, word_df$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


