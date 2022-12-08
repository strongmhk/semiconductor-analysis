##Samsung dis

dis_s <- file("C:/Users/dnjs1/Downloads/Rr/dis_sam.txt")
dis_s_r<-readLines(dis_s)
str(dis_s_r)
head(dis_s_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


d_s_r<- sapply(dis_s_r, exNouns)
d_s_r[2]


#말뭉치 Corpus 생성

d_s_r_cpus<-Corpus(VectorSource(d_s_r))
d_s_r_cpus


#데이터 전처리

d_s_r_cpus_prepro<-tm_map(d_s_r_cpus,removePunctuation) #문장부호제거

d_s_r_cpus_prepro<-tm_map(d_s_r_cpus_prepro,removeNumbers) #숫자 제거

d_s_r_cpus_prepro<-tm_map(d_s_r_cpus_prepro,tolower) #영문자 소문자 변경

d_s_r_cpus_prepro<-tm_map(d_s_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(d_s_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
d_s_r_cpus_prepro_term<-TermDocumentMatrix(d_s_r_cpus_prepro, control = list(wordLengths=c(4,16)))

d_s_r_cpus_prepro_term

#matrix -> data.frame으로 변경
d_s_r_cpus_df<-as.data.frame(as.matrix(d_s_r_cpus_prepro_term))

dim(d_s_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(d_s_r_cpus_df), decreasing = T)
wordResult


#워드 클라우드 시각화
word_names<-names(wordResult)
wordcloud(names(wordResult[1:30]),wordResult[1:30]) # 상위 30개만 뽑아서 추출

#단어 이름과 빈도수로 df 생성
word_df<-data.frame(word = names(wordResult[1:40]), freq= wordResult[1:40])

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))
brewer.pal(12)

#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df$word, word_df$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")










##samsung led
l_sam <- file("C:/Users/dnjs1/Downloads/Rr/led_sam.txt")
l_sam_r<-readLines(l_sam)
str(l_sam_r)
head(l_sam_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


l_s_r<- sapply(l_sam_r, exNouns)
l_s_r[2]


#말뭉치 Corpus 생성

l_s_r_cpus<-Corpus(VectorSource(l_s_r))
l_s_r_cpus


#데이터 전처리

l_s_r_cpus_prepro<-tm_map(l_s_r_cpus,removePunctuation) #문장부호제거

l_s_r_cpus_prepro<-tm_map(l_s_r_cpus_prepro,removeNumbers) #숫자 제거

l_s_r_cpus_prepro<-tm_map(l_s_r_cpus_prepro,tolower) #영문자 소문자 변경

l_s_r_cpus_prepro<-tm_map(l_s_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(l_s_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
l_s_r_cpus_prepro_term<-TermDocumentMatrix(l_s_r_cpus_prepro, control = list(wordLengths=c(4,16)))

l_s_r_cpus_prepro_term

#matrix -> data.frame으로 변경
l_s_r_cpus_df<-as.data.frame(as.matrix(l_s_r_cpus_prepro_term))

dim(l_s_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(l_s_r_cpus_df), decreasing = T)
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


##samsung RAM

r_sam <- file("C:/Users/dnjs1/Downloads/Rr/ram_sam.txt")
r_sam_r<-readLines(r_sam)
str(r_sam_r)
head(r_sam_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


r_s_r<- sapply(r_sam_r, exNouns)
r_s_r[2]


#말뭉치 Corpus 생성

r_s_r_cpus<-Corpus(VectorSource(r_s_r))
r_s_r_cpus


#데이터 전처리

r_s_r_cpus_prepro<-tm_map(r_s_r_cpus,removePunctuation) #문장부호제거

r_s_r_cpus_prepro<-tm_map(r_s_r_cpus_prepro,removeNumbers) #숫자 제거

r_s_r_cpus_prepro<-tm_map(r_s_r_cpus_prepro,tolower) #영문자 소문자 변경

r_s_r_cpus_prepro<-tm_map(r_s_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(r_s_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
r_s_r_cpus_prepro_term<-TermDocumentMatrix(r_s_r_cpus_prepro, control = list(wordLengths=c(4,16)))

r_s_r_cpus_prepro_term

#matrix -> data.frame으로 변경
r_s_r_cpus_df<-as.data.frame(as.matrix(r_s_r_cpus_prepro_term))

dim(r_s_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(r_s_r_cpus_df), decreasing = T)
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




##samsung transistor


tran_sam <- file("C:/Users/dnjs1/Downloads/Rr/tran_sam.txt")
tran_sam_r<-readLines(tran_sam)
str(tran_sam_r)
head(tran_sam_r)

#명사 추출

exNouns<-function(x){
  paste(extractNoun(as.character(x)), collpase= " ")
}


t_s_r<- sapply(tran_sam_r, exNouns)
t_s_r[2]


#말뭉치 Corpus 생성

t_s_r_cpus<-Corpus(VectorSource(t_s_r))
t_s_r_cpus


#데이터 전처리

t_s_r_cpus_prepro<-tm_map(t_s_r_cpus,removePunctuation) #문장부호제거

t_s_r_cpus_prepro<-tm_map(t_s_r_cpus_prepro,removeNumbers) #숫자 제거

t_s_r_cpus_prepro<-tm_map(t_s_r_cpus_prepro,tolower) #영문자 소문자 변경

t_s_r_cpus_prepro<-tm_map(t_s_r_cpus_prepro,removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거


#전처리 결과 확인
inspect(t_s_r_cpus_prepro)


#단어 선별(1음절 = 2byte, 2음절 ~ 8음절)
t_s_r_cpus_prepro_term<-TermDocumentMatrix(t_s_r_cpus_prepro, control = list(wordLengths=c(4,16)))

t_s_r_cpus_prepro_term

#matrix -> data.frame으로 변경
t_s_r_cpus_df<-as.data.frame(as.matrix(t_s_r_cpus_prepro_term))

dim(t_s_r_cpus_df)

#단어 출현 빈도수 구하기

wordResult<- sort(rowSums(t_s_r_cpus_df), decreasing = T)
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













