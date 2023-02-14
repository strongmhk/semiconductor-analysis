install.packages("KoNLP")
install.packages("wordcloud")
install.packages("tm")
install.packages("wordcloud2")
install.packages("ggplot2")



library(KoNLP)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(wordcloud2)
library(ggplot2)


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




##Samsung dis

dis_s <- file("C:/Users/kim65/OneDrive/바탕 화면/w/samsung/dis_sam.txt")
dis_s<-readLines(dis_s)

length(dis_s) # 데이터 개수 확인 


word_df5<-ex_word_freq(dis_s)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df5$word, word_df5$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")










##samsung led
l_sam <- file("C:/Users/kim65/OneDrive/바탕 화면/w/samsung/led_sam.txt")
l_sam<-readLines(l_sam)

length(l_sam)


word_df6<-ex_word_freq(l_sam)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df6$word, word_df6$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")


##samsung RAM

r_sam <- file("C:/Users/kim65/OneDrive/바탕 화면/w/samsung/ram_sam.txt")
r_sam<-readLines(r_sam)

length(r_sam)


word_df7<-ex_word_freq(r_sam)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df7$word, word_df7$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")




##samsung transistor


tran_sam <- file("C:/Users/kim65/OneDrive/바탕 화면/w/samsung/tran_sam.txt")
tran_sam<-readLines(tran_sam)

length(tran_sam)


word_df8<-ex_word_freq(tran_sam)

#단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired") #12가지 색상
windowsFonts(malgun=windowsFont("맑은 고딕"))


#단어 구름 시각화
x11() #별도의 창을 띄우는 함수
wordcloud(word_df8$word, word_df8$freq, scale= c(5,1), min.freq=3, random.order=F, rot.per =.1, colors = pal, family= "malgun")













