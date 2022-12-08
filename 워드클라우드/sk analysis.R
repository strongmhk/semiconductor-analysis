##sk dis

dis_sk <- file("C:/Users/dnjs1/Downloads/Rr/dis_sk.txt")
dis_sk_r<-readLines(dis_sk)
str(dis_sk_r)
head(dis_sk_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


d_k_r<- sapply(dis_sk_r, exNouns)
d_k_r[2]


#말뭉치 Corpus 생성

d_k_r_cpus<-Corpus(VectorSource(d_k_r))
d_k_r_cpus


#데이터 전처리

d_k_r_cpus_prepro<-tm_map(d_k_r_cpus,removePunctuation) #문장부호제거

d_k_r_cpus_prepro<-tm_map(d_k_r_cpus_prepro,removeNumbers) #숫자 제거

d_k_r_cpus_prepro<-tm_map(d_k_r_cpus_prepro,tolower) #영문자 소문자 변경

d_k_r_cpus_prepro<-tm_map(d_k_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(d_k_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
d_k_r_cpus_prepro_term<-TermDocumentMatrix(d_k_r_cpus_prepro, control = list(wordLengths=c(4,16)))

d_k_r_cpus_prepro_term

#matrix -> data.frame으로 변경
d_k_r_cpus_df<-as.data.frame(as.matrix(d_k_r_cpus_prepro_term))

dim(d_k_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(d_k_r_cpus_df), decreasing = T)
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







##sk led

led_sk <- file("C:/Users/dnjs1/Downloads/Rr/led_sk.txt")
led_sk_r<-readLines(led_sk)
str(led_sk_r)
head(led_sk_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


l_k_r<- sapply(led_sk_r, exNouns)
l_k_r[2]


#말뭉치 Corpus 생성

l_k_r_cpus<-Corpus(VectorSource(l_k_r))
l_k_r_cpus


#데이터 전처리

l_k_r_cpus_prepro<-tm_map(l_k_r_cpus,removePunctuation) #문장부호제거

l_k_r_cpus_prepro<-tm_map(l_k_r_cpus_prepro,removeNumbers) #숫자 제거

l_k_r_cpus_prepro<-tm_map(l_k_r_cpus_prepro,tolower) #영문자 소문자 변경

l_k_r_cpus_prepro<-tm_map(l_k_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(l_k_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
l_k_r_cpus_prepro_term<-TermDocumentMatrix(l_k_r_cpus_prepro, control = list(wordLengths=c(4,16)))

l_k_r_cpus_prepro_term

#matrix -> data.frame으로 변경
l_k_r_cpus_df<-as.data.frame(as.matrix(l_k_r_cpus_prepro_term))

dim(l_k_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(l_k_r_cpus_df), decreasing = T)
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


##sk RAM

ram_sk <- file("C:/Users/dnjs1/Downloads/Rr/ram_sk.txt")
ram_sk_r<-readLines(ram_sk)
str(ram_sk_r)
headr(ram_sk_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


r_k_r<- sapply(ram_sk_r, exNouns)
r_k_r[2]


#말뭉치 Corpus 생성

r_k_r_cpus<-Corpus(VectorSource(r_k_r))
r_k_r_cpus


#데이터 전처리

r_k_r_cpus_prepro<-tm_map(r_k_r_cpus,removePunctuation) #문장부호제거

r_k_r_cpus_prepro<-tm_map(r_k_r_cpus_prepro,removeNumbers) #숫자 제거

r_k_r_cpus_prepro<-tm_map(r_k_r_cpus_prepro,tolower) #영문자 소문자 변경

r_k_r_cpus_prepro<-tm_map(r_k_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(r_k_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
r_k_r_cpus_prepro_term<-TermDocumentMatrix(r_k_r_cpus_prepro, control = list(wordLengths=c(4,16)))

r_k_r_cpus_prepro_term

#matrix -> data.frame으로 변경
r_k_r_cpus_df<-as.data.frame(as.matrix(r_k_r_cpus_prepro_term))

dim(r_k_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(r_k_r_cpus_df), decreasing = T)
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




##sk transistor


Tran_sk <- file("C:/Users/dnjs1/Downloads/Rr/tran_sk.txt")
Tran_sk_r<-readLines(Tran_sk)
str(Tran_sk_r)
head(Tran_sk_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


t_k_r<- sapply(Tran_sk_r, exNouns)
t_k_r[2]


#말뭉치 Corpus 생성

t_k_r_cpus<-Corpus(VectorSource(t_k_r))
t_k_r_cpus


#데이터 전처리

t_k_r_cpus_prepro<-tm_map(t_k_r_cpus,removePunctuation) #문장부호제거

t_k_r_cpus_prepro<-tm_map(t_k_r_cpus_prepro,removeNumbers) #숫자 제거

t_k_r_cpus_prepro<-tm_map(t_k_r_cpus_prepro,tolower) #영문자 소문자 변경

t_k_r_cpus_prepro<-tm_map(t_k_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(t_k_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
t_k_r_cpus_prepro_term<-TermDocumentMatrix(t_k_r_cpus_prepro, control = list(wordLengths=c(4,16)))

t_k_r_cpus_prepro_term

#matrix -> data.frame으로 변경
t_k_r_cpus_df<-as.data.frame(as.matrix(t_k_r_cpus_prepro_term))

dim(t_k_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(t_k_r_cpus_df), decreasing = T)
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


