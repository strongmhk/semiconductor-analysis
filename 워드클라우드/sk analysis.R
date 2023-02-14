install.pacakges("KoNLP")
install.packages("wordcloud")
install.packages("tm")
install.packages("wordcloud2")
install.packages("ggplot2")




library(KoNLP)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(wordcloud2)


ex_word_freq<-function(x){
  exNouns<-function(y){
    paste(extractNoun(as.character(y)), collapse = " ")  # 명사 추출
  }
  
  
  x<-sapply(x, exNouns)  
  
  x<-Corpus(VectorSource(x)) # 말뭉치 생성
  
  x<-tm_map(x, removePunctuation) #문장부호 제거
  x<-tm_map(x, removeNumbers) #숫자 제거
  x<-tm_map(x, tolower) #영문자 소문자로 변환
  x<-tm_map(x, removeWords, stopwords('english')) #불용어(for, very, and, of, are) 제거
  
  x<-TermDocumentMatrix(x, control = list(wordLengths=c(4,16))) # 1음절 2비트(한글기준) 2음절~8음절
  
  x<-as.data.frame(as.matrix(x))
  
  x<-sort(rowSums(x), decreasing = T)
  
  x<-data.frame(word = names(x[1:40]), freq= x[1:40]) #빈도수 상위 40개
  
  return(x)
}


##sk dis

dis_sk <- file("C:/Users/kim65/OneDrive/바탕 화면/w/sk/dis_sk.txt")
dis_sk<-readLines(dis_sk)

length(dis_sk) # 데이터 개수 확인 


word_df9<-ex_word_freq(dis_sk)


#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df9$word, word_df9$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")







##sk led

led_sk <- file("C:/Users/kim65/OneDrive/바탕 화면/w/sk/led_sk.txt")
led_sk<-readLines(led_sk)

length(ked_sk) # 데이터 개수 확인 


word_df10<-ex_word_freq(led_sk)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df10$word, word_df10$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


##sk RAM

ram_sk <- file("C:/Users/kim65/OneDrive/바탕 화면/w/sk/ram_sk.txt")
ram_sk<-readLines(ram_sk)

length(ram_sk) # 데이터 개수 확인 


word_df11<-ex_word_freq(ram_sk)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df11$word, word_df11$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")




##sk transistor


Tran_sk <- file("C:/Users/kim65/OneDrive/바탕 화면/w/sk/tran_sk.txt")
Tran_sk<-readLines(Tran_sk)

length(Tran_sk) # 데이터 개수 확인 


word_df12<-ex_word_freq(Tran_sk)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df12$word, word_df12$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


