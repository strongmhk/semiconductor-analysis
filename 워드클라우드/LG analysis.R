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

# 단어 빈도 추출 함수

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







##LG dis

dis_LG <- file("C:/Users/kim65/OneDrive/바탕 화면/w/LG/dis_n(LG).txt")
dis_LG<-readLines(dis_LG)

length(dis_LG)


word_df1<-ex_word_freq(dis_LG)


#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))

#단어 구름 시각화
x11() 
wordcloud(word_df1$word, word_df1$freq, scale= c(5,1), random.order=F, rot.per =.1, colors = pal, family= "malgun")







##LG led

led_LG <- file("C:/Users/kim65/OneDrive/바탕 화면/w/LG/led(LG).txt")
led_LG<-readLines(led_LG)

length(led_LG)


word_df2<-ex_word_freq(dis_LG)

pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))



#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df2$word, word_df2$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


##LG RAM

RAM_LG <- file("C:/Users/kim65/OneDrive/바탕 화면/w/LG/RAM(LG).txt")
RAM_LG<-readLines(RAM_LG)

length(RAM_LG)


word_df3<-ex_word_freq(RAM_LG)


pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))




x11() 
wordcloud(word_df3$word, word_df3$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")




##LG transistor


Tran_LG <- file("C:/Users/kim65/OneDrive/바탕 화면/w/LG/Transistor(LG).txt")
Tran_LG<-readLines(Tran_LG)

length(Tran_LG)


word_df4<-ex_word_freq(Tran_LG)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df4$word, word_df4$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


