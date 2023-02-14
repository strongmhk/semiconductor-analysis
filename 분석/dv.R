### 막대그래프 그려주기
# install.packages("ggplot2")
# install.packages("gridExtra")
library(ggplot2)
library(gridExtra)


#막대그래프 
barplot(word_df5$freq,names.arg = word_df5$word)
title(main="sk display",xlab="keyword",ylab="freq")


#기업별 ggplot(display)
word_df1<-data.frame(word = names(wordResult1[1:10]), freq= wordResult1[1:10])
word_df5<-data.frame(word = names(wordResult5[1:10]), freq= wordResult5[1:10])
word_df9<-data.frame(word = names(wordResult9[1:10]), freq= wordResult9[1:10])


win.graph(15,10)
ggplot(word_df1, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "LG display 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df5, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SAMSUNG display 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df9, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SK display 관련 특허 키워드")

#기업별 제품별 ggplot(led)
word_df2<-data.frame(word = names(wordResult2[1:10]), freq= wordResult2[1:10])
word_df6<-data.frame(word = names(wordResult6[1:10]), freq= wordResult6[1:10])
word_df10<-data.frame(word = names(wordResult10[1:10]), freq= wordResult10[1:10])


win.graph(15,10)
ggplot(word_df2, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "LG led 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df6, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SAMSUNG led 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df10, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SK led 관련 특허 키워드")


#기업별 제품별 ggplot(ram)
word_df3<-data.frame(word = names(wordResult3[1:10]), freq= wordResult3[1:10])
word_df7<-data.frame(word = names(wordResult7[1:10]), freq= wordResult7[1:10])
word_df11<-data.frame(word = names(wordResult11[1:10]), freq= wordResult11[1:10])


win.graph(15,10)
ggplot(word_df2, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "LG ram 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df6, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SAMSUNG ram 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df10, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SK ram 관련 특허 키워드")

#기업별 제품별 ggplot(transistor)
word_df4<-data.frame(word = names(wordResult4[1:10]), freq= wordResult4[1:10])
word_df8<-data.frame(word = names(wordResult8[1:10]), freq= wordResult8[1:10])
word_df12<-data.frame(word = names(wordResult12[1:10]), freq= wordResult12[1:10])


win.graph(15,10)
ggplot(word_df4, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "LG transistor 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df8, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SAMSUNG transistor 관련 특허 키워드")
win.graph(15,10)
ggplot(word_df12, aes(word, freq, fill = word)) + geom_bar(stat='identity') + labs(title = "SK transistor 관련 특허 키워드")



d1 <- ggplot(word_df1, aes(word, freq, fill = word)) + geom_bar(stat='identity')
d5 <- ggplot(word_df5, aes(word, freq, fill = word)) + geom_bar(stat='identity')
d9 <- ggplot(word_df9, aes(word, freq, fill = word)) + geom_bar(stat='identity')
grid.arrange(d1, d5, d9, nrow=1, ncol=3)




getwd()
setwd('C:/sgt1/jobang2/jobang_proj')
setwd('C:/Users/kim65/OneDrive/바탕 화면/w')


# 년도별 심사진행상태 추출 함수 정의 (x=데이터 프레임 , y=출원년도, z=심사진행상태)
# install.packages("dplyr")
library(dplyr)
rate<- function(x, y, z){
  b<-x %>% filter(grepl(y, 출원일자))
  a<-intersect(x %>% filter(grepl(y, 출원일자)), x %>% filter(grepl(z, 심사진행상태)))
  
  return(nrow(a)/nrow(b))
}



# display(crystal) 키워드 최근 5년(2022 ~ 2018) 심사진행상태 추이
# 등록결정=1, 거절결정=2, 포기=3, 취하=4
crys_lg<-read.csv("crys_lg.csv", header = T)
crys_sam<-read.csv("crys_sam.csv", header = T)
crys_sk<-read.csv("crys_sk.csv", header = T)

# 결측치 제거
crys_lg <- crys_lg[!(crys_lg$심사진행상태 == "" ), ]
crys_sam <- crys_sam[!(crys_sam$심사진행상태 == "" ), ]
crys_sk <- crys_sk[!(crys_sk$심사진행상태 == "" ), ]



# 등록결정
crys_l_2022_1<-rate(crys_lg, 2022, '등록결정')
crys_l_2021_1<-rate(crys_lg, 2021, '등록결정')
crys_l_2020_1<-rate(crys_lg, 2020, '등록결정')
crys_l_2019_1<-rate(crys_lg, 2019, '등록결정')
crys_l_2018_1<-rate(crys_lg, 2018, '등록결정')

crys_s_2022_1<-rate(crys_sam, 2022, '등록결정')
crys_s_2021_1<-rate(crys_sam, 2021, '등록결정')
crys_s_2020_1<-rate(crys_sam, 2020, '등록결정')
crys_s_2019_1<-rate(crys_sam, 2019, '등록결정')
crys_s_2018_1<-rate(crys_sam, 2018, '등록결정')

crys_k_2022_1<-rate(crys_sk, 2022, '등록결정')
crys_k_2021_1<-rate(crys_sk, 2021, '등록결정')
crys_k_2020_1<-rate(crys_sk, 2020, '등록결정')
crys_k_2019_1<-rate(crys_sk, 2019, '등록결정')
crys_k_2018_1<-rate(crys_sk, 2018, '등록결정')

#거절결정
crys_l_2022_2<-rate(crys_lg, 2022, '거절결정')
crys_l_2021_2<-rate(crys_lg, 2021, '거절결정')
crys_l_2020_2<-rate(crys_lg, 2020, '거절결정')
crys_l_2019_2<-rate(crys_lg, 2019, '거절결정')
crys_l_2018_2<-rate(crys_lg, 2018, '거절결정')

crys_s_2022_2<-rate(crys_sam, 2022, '거절결정')
crys_s_2021_2<-rate(crys_sam, 2021, '거절결정')
crys_s_2020_2<-rate(crys_sam, 2020, '거절결정')
crys_s_2019_2<-rate(crys_sam, 2019, '거절결정')
crys_s_2018_2<-rate(crys_sam, 2018, '거절결정')

crys_k_2022_2<-rate(crys_sk, 2022, '거절결정')
crys_k_2021_2<-rate(crys_sk, 2021, '거절결정')
crys_k_2020_2<-rate(crys_sk, 2020, '거절결정')
crys_k_2019_2<-rate(crys_sk, 2019, '거절결정')
crys_k_2018_2<-rate(crys_sk, 2018, '거절결정')



# 포기
crys_l_2022_3<-rate(crys_lg, 2022, '포기')
crys_l_2021_3<-rate(crys_lg, 2021, '포기')
crys_l_2020_3<-rate(crys_lg, 2020, '포기')
crys_l_2019_3<-rate(crys_lg, 2019, '포기')
crys_l_2018_3<-rate(crys_lg, 2018, '포기')

crys_s_2022_3<-rate(crys_sam, 2022, '포기')
crys_s_2021_3<-rate(crys_sam, 2021, '포기')
crys_s_2020_3<-rate(crys_sam, 2020, '포기')
crys_s_2019_3<-rate(crys_sam, 2019, '포기')
crys_s_2018_3<-rate(crys_sam, 2018, '포기')

crys_k_2022_3<-rate(crys_sk, 2022, '포기')
crys_k_2021_3<-rate(crys_sk, 2021, '포기')
crys_k_2020_3<-rate(crys_sk, 2020, '포기')
crys_k_2019_3<-rate(crys_sk, 2019, '포기')
crys_k_2018_3<-rate(crys_sk, 2018, '포기')



# 취하
crys_l_2022_4<-rate(crys_lg, 2022, '취하')
crys_l_2021_4<-rate(crys_lg, 2021, '취하')
crys_l_2020_4<-rate(crys_lg, 2020, '취하')
crys_l_2019_4<-rate(crys_lg, 2019, '취하')
crys_l_2018_4<-rate(crys_lg, 2018, '취하')

crys_s_2022_4<-rate(crys_sam, 2022, '취하')
crys_s_2021_4<-rate(crys_sam, 2021, '취하')
crys_s_2020_4<-rate(crys_sam, 2020, '취하')
crys_s_2019_4<-rate(crys_sam, 2019, '취하')
crys_s_2018_4<-rate(crys_sam, 2018, '취하')

crys_k_2022_4<-rate(crys_sk, 2022, '취하')
crys_k_2021_4<-rate(crys_sk, 2021, '취하')
crys_k_2020_4<-rate(crys_sk, 2020, '취하')
crys_k_2019_4<-rate(crys_sk, 2019, '취하')
crys_k_2018_4<-rate(crys_sk, 2018, '취하')


crys_l<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(crys_l_2022_1,crys_l_2021_1,crys_l_2020_1,crys_l_2019_1,crys_l_2018_1,crys_l_2022_2,crys_l_2021_2,crys_l_2020_2,crys_l_2019_2,crys_l_2018_2,crys_l_2022_3,crys_l_2021_3,crys_l_2020_3,crys_l_2019_3,crys_l_2018_3,crys_l_2022_4,crys_l_2021_4,crys_l_2020_4,crys_l_2019_4,crys_l_2018_4))

crys_s<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(crys_s_2022_1,crys_s_2021_1,crys_s_2020_1,crys_s_2019_1,crys_s_2018_1,crys_s_2022_2,crys_s_2021_2,crys_s_2020_2,crys_s_2019_2,crys_s_2018_2,crys_s_2022_3,crys_s_2021_3,crys_s_2020_3,crys_s_2019_3,crys_s_2018_3,crys_s_2022_4,crys_s_2021_4,crys_s_2020_4,crys_s_2019_4,crys_s_2018_4))

crys_k<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(crys_k_2022_1,crys_k_2021_1,crys_k_2020_1,crys_k_2019_1,crys_k_2018_1,crys_k_2022_2,crys_k_2021_2,crys_k_2020_2,crys_k_2019_2,crys_k_2018_2,crys_k_2022_3,crys_k_2021_3,crys_k_2020_3,crys_k_2019_3,crys_k_2018_3,crys_k_2022_4,crys_k_2021_4,crys_k_2020_4,crys_k_2019_4,crys_k_2018_4))





win.graph(15,10)
g1 <- ggplot(data=crys_l, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
 geom_point(size=2.5, color='#FFFFFF') + labs(title='LG crystal 관련 특허 최근 5년 심사진행상태')


g2 <- ggplot(data=crys_s, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SAMSUNG crystal 관련 특허 최근 5년 심사진행상태')


g3 <- ggplot(data=crys_k, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SK crystal 관련 특허 최근 5년 심사진행상태')


grid.arrange(g1, g2, g3, nrow=1, ncol=3)


# led(oled) 키워드 최근 5년(2022 ~ 2018) 심사진행상태 추이
# 등록결정=1, 거절결정=2, 포기=3, 취하=4

oled_lg<-read.csv("oled_lg.csv", header = T)
oled_sam<-read.csv("oled_sam.csv", header = T)
oled_sk<-read.csv("oled_sk.csv", header = T)

# 결측치 제거
oled_lg <- oled_lg[!(oled_lg$심사진행상태 == "" ), ]
oled_sam <- oled_sam[!(oled_sam$심사진행상태 == "" ), ]
oled_sk <- oled_sk[!(oled_sk$심사진행상태 == "" ), ]


# 등록결정
oled_l_2022_1<-rate(oled_lg, 2022, '등록결정')
oled_l_2021_1<-rate(oled_lg, 2021, '등록결정')
oled_l_2020_1<-rate(oled_lg, 2020, '등록결정')
oled_l_2019_1<-rate(oled_lg, 2019, '등록결정')
oled_l_2018_1<-rate(oled_lg, 2018, '등록결정')

oled_s_2022_1<-rate(oled_sam, 2022, '등록결정')
oled_s_2021_1<-rate(oled_sam, 2021, '등록결정')
oled_s_2020_1<-rate(oled_sam, 2020, '등록결정')
oled_s_2019_1<-rate(oled_sam, 2019, '등록결정')
oled_s_2018_1<-rate(oled_sam, 2018, '등록결정')

oled_k_2022_1<-rate(oled_sk, 2022, '등록결정')
oled_k_2021_1<-rate(oled_sk, 2021, '등록결정')
oled_k_2020_1<-rate(oled_sk, 2020, '등록결정')
oled_k_2019_1<-rate(oled_sk, 2019, '등록결정')
oled_k_2018_1<-rate(oled_sk, 2018, '등록결정')

#거절결정
oled_l_2022_2<-rate(oled_lg, 2022, '거절결정')
oled_l_2021_2<-rate(oled_lg, 2021, '거절결정')
oled_l_2020_2<-rate(oled_lg, 2020, '거절결정')
oled_l_2019_2<-rate(oled_lg, 2019, '거절결정')
oled_l_2018_2<-rate(oled_lg, 2018, '거절결정')

oled_s_2022_2<-rate(oled_sam, 2022, '거절결정')
oled_s_2021_2<-rate(oled_sam, 2021, '거절결정')
oled_s_2020_2<-rate(oled_sam, 2020, '거절결정')
oled_s_2019_2<-rate(oled_sam, 2019, '거절결정')
oled_s_2018_2<-rate(oled_sam, 2018, '거절결정')

oled_k_2022_2<-rate(oled_sk, 2022, '거절결정')
oled_k_2021_2<-rate(oled_sk, 2021, '거절결정')
oled_k_2020_2<-rate(oled_sk, 2020, '거절결정')
oled_k_2019_2<-rate(oled_sk, 2019, '거절결정')
oled_k_2018_2<-rate(oled_sk, 2018, '거절결정')



# 포기
oled_l_2022_3<-rate(oled_lg, 2022, '포기')
oled_l_2021_3<-rate(oled_lg, 2021, '포기')
oled_l_2020_3<-rate(oled_lg, 2020, '포기')
oled_l_2019_3<-rate(oled_lg, 2019, '포기')
oled_l_2018_3<-rate(oled_lg, 2018, '포기')

oled_s_2022_3<-rate(oled_sam, 2022, '포기')
oled_s_2021_3<-rate(oled_sam, 2021, '포기')
oled_s_2020_3<-rate(oled_sam, 2020, '포기')
oled_s_2019_3<-rate(oled_sam, 2019, '포기')
oled_s_2018_3<-rate(crys_sam, 2018, '포기')

oled_k_2022_3<-rate(oled_sk, 2022, '포기')
oled_k_2021_3<-rate(oled_sk, 2021, '포기')
oled_k_2020_3<-rate(oled_sk, 2020, '포기')
oled_k_2019_3<-rate(oled_sk, 2019, '포기')
oled_k_2018_3<-rate(oled_sk, 2018, '포기')



# 취하
oled_l_2022_4<-rate(oled_lg, 2022, '취하')
oled_l_2021_4<-rate(oled_lg, 2021, '취하')
oled_l_2020_4<-rate(oled_lg, 2020, '취하')
oled_l_2019_4<-rate(oled_lg, 2019, '취하')
oled_l_2018_4<-rate(oled_lg, 2018, '취하')

oled_s_2022_4<-rate(oled_sam, 2022, '취하')
oled_s_2021_4<-rate(oled_sam, 2021, '취하')
oled_s_2020_4<-rate(oled_sam, 2020, '취하')
oled_s_2019_4<-rate(oled_sam, 2019, '취하')
oled_s_2018_4<-rate(oled_sam, 2018, '취하')

oled_k_2022_4<-rate(oled_sk, 2022, '취하')
oled_k_2021_4<-rate(oled_sk, 2021, '취하')
oled_k_2020_4<-rate(oled_sk, 2020, '취하')
oled_k_2019_4<-rate(oled_sk, 2019, '취하')
oled_k_2018_4<-rate(oled_sk, 2018, '취하')


oled_l<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(oled_l_2022_1,oled_l_2021_1,oled_l_2020_1,oled_l_2019_1,oled_l_2018_1,oled_l_2022_2,oled_l_2021_2,oled_l_2020_2,oled_l_2019_2,oled_l_2018_2,oled_l_2022_3,oled_l_2021_3,oled_l_2020_3,oled_l_2019_3,oled_l_2018_3,oled_l_2022_4,oled_l_2021_4,oled_l_2020_4,oled_l_2019_4,oled_l_2018_4))

oled_s<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(oled_s_2022_1,oled_s_2021_1,oled_s_2020_1,oled_s_2019_1,oled_s_2018_1,oled_s_2022_2,oled_s_2021_2,oled_s_2020_2,oled_s_2019_2,oled_s_2018_2,oled_s_2022_3,oled_s_2021_3,oled_s_2020_3,oled_s_2019_3,oled_s_2018_3,oled_s_2022_4,oled_s_2021_4,oled_s_2020_4,oled_s_2019_4,oled_s_2018_4))

oled_k<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(oled_k_2022_1,oled_k_2021_1,oled_k_2020_1,oled_k_2019_1,oled_k_2018_1,oled_k_2022_2,oled_k_2021_2,oled_k_2020_2,oled_k_2019_2,oled_k_2018_2,oled_k_2022_3,oled_k_2021_3,oled_k_2020_3,oled_k_2019_3,oled_k_2018_3,oled_k_2022_4,oled_k_2021_4,oled_k_2020_4,oled_k_2019_4,oled_k_2018_4))





win.graph(15,10)
g1 <- ggplot(data=oled_l, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='LG oled 관련 특허 최근 5년 심사진행상태')


g2 <- ggplot(data=oled_s, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SAMSUNG oled 관련 특허 최근 5년 심사진행상태')


g3 <- ggplot(data=oled_k, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SK oled 관련 특허 최근 5년 심사진행상태')


grid.arrange(g1, g2, g3, nrow=1, ncol=3)





# ram(sram) 키워드 최근 5년(2022 ~ 2018) 심사진행상태 추이
# 등록결정=1, 거절결정=2, 포기=3, 취하=4

sram_lg<-read.csv("sram_lg.csv", header = T)
sram_sam<-read.csv("sram_sam.csv", header = T)
sram_sk<-read.csv("sram_sk.csv", header = T)

# 결측치 제거
sram_lg <- sram_lg[!(sram_lg$심사진행상태 == "" ), ]
sram_sam <- sram_sam[!(sram_sam$심사진행상태 == "" ), ]
sram_sk <- sram_sk[!(sram_sk$심사진행상태 == "" ), ]



# 등록결정
sram_l_2022_1<-rate(sram_lg, 2022, '등록결정')
sram_l_2021_1<-rate(sram_lg, 2021, '등록결정')
sram_l_2020_1<-rate(sram_lg, 2020, '등록결정')
sram_l_2019_1<-rate(sram_lg, 2019, '등록결정')
sram_l_2018_1<-rate(sram_lg, 2018, '등록결정')

sram_s_2022_1<-rate(sram_sam, 2022, '등록결정')
sram_s_2021_1<-rate(sram_sam, 2021, '등록결정')
sram_s_2020_1<-rate(sram_sam, 2020, '등록결정')
sram_s_2019_1<-rate(sram_sam, 2019, '등록결정')
sram_s_2018_1<-rate(sram_sam, 2018, '등록결정')

sram_k_2022_1<-rate(sram_sk, 2022, '등록결정')
sram_k_2021_1<-rate(sram_sk, 2021, '등록결정')
sram_k_2020_1<-rate(sram_sk, 2020, '등록결정')
sram_k_2019_1<-rate(sram_sk, 2019, '등록결정')
sram_k_2018_1<-rate(sram_sk, 2018, '등록결정')

#거절결정
sram_l_2022_2<-rate(sram_lg, 2022, '거절결정')
sram_l_2021_2<-rate(sram_lg, 2021, '거절결정')
sram_l_2020_2<-rate(sram_lg, 2020, '거절결정')
sram_l_2019_2<-rate(sram_lg, 2019, '거절결정')
sram_l_2018_2<-rate(sram_lg, 2018, '거절결정')

sram_s_2022_2<-rate(sram_sam, 2022, '거절결정')
sram_s_2021_2<-rate(sram_sam, 2021, '거절결정')
sram_s_2020_2<-rate(sram_sam, 2020, '거절결정')
sram_s_2019_2<-rate(sram_sam, 2019, '거절결정')
sram_s_2018_2<-rate(sram_sam, 2018, '거절결정')

sram_k_2022_2<-rate(sram_sk, 2022, '거절결정')
sram_k_2021_2<-rate(sram_sk, 2021, '거절결정')
sram_k_2020_2<-rate(sram_sk, 2020, '거절결정')
sram_k_2019_2<-rate(sram_sk, 2019, '거절결정')
sram_k_2018_2<-rate(sram_sk, 2018, '거절결정')



# 포기
sram_l_2022_3<-rate(sram_lg, 2022, '포기')
sram_l_2021_3<-rate(sram_lg, 2021, '포기')
sram_l_2020_3<-rate(sram_lg, 2020, '포기')
sram_l_2019_3<-rate(sram_lg, 2019, '포기')
sram_l_2018_3<-rate(sram_lg, 2018, '포기')

sram_s_2022_3<-rate(sram_sam, 2022, '포기')
sram_s_2021_3<-rate(sram_sam, 2021, '포기')
sram_s_2020_3<-rate(sram_sam, 2020, '포기')
sram_s_2019_3<-rate(sram_sam, 2019, '포기')
sram_s_2018_3<-rate(sram_sam, 2018, '포기')

sram_k_2022_3<-rate(sram_sk, 2022, '포기')
sram_k_2021_3<-rate(sram_sk, 2021, '포기')
sram_k_2020_3<-rate(sram_sk, 2020, '포기')
sram_k_2019_3<-rate(sram_sk, 2019, '포기')
sram_k_2018_3<-rate(sram_sk, 2018, '포기')


# 취하
sram_l_2022_4<-rate(sram_lg, 2022, '취하')
sram_l_2021_4<-rate(sram_lg, 2021, '취하')
sram_l_2020_4<-rate(sram_lg, 2020, '취하')
sram_l_2019_4<-rate(sram_lg, 2019, '취하')
sram_l_2018_4<-rate(sram_lg, 2018, '취하')

sram_s_2022_4<-rate(sram_sam, 2022, '취하')
sram_s_2021_4<-rate(sram_sam, 2021, '취하')
sram_s_2020_4<-rate(sram_sam, 2020, '취하')
sram_s_2019_4<-rate(sram_sam, 2019, '취하')
sram_s_2018_4<-rate(sram_sam, 2018, '취하')

sram_k_2022_4<-rate(sram_sk, 2022, '취하')
sram_k_2021_4<-rate(sram_sk, 2021, '취하')
sram_k_2020_4<-rate(sram_sk, 2020, '취하')
sram_k_2019_4<-rate(sram_sk, 2019, '취하')
sram_k_2018_4<-rate(sram_sk, 2018, '취하')


sram_l<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(sram_l_2022_1,sram_l_2021_1,sram_l_2020_1,sram_l_2019_1,sram_l_2018_1,sram_l_2022_2,sram_l_2021_2,sram_l_2020_2,sram_l_2019_2,sram_l_2018_2,sram_l_2022_3,sram_l_2021_3,sram_l_2020_3,sram_l_2019_3,sram_l_2018_3,sram_l_2022_4,sram_l_2021_4,sram_l_2020_4,sram_l_2019_4,sram_l_2018_4))

sram_s<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(sram_s_2022_1,sram_s_2021_1,sram_s_2020_1,sram_s_2019_1,sram_s_2018_1,sram_s_2022_2,sram_s_2021_2,sram_s_2020_2,sram_s_2019_2,sram_s_2018_2,sram_s_2022_3,sram_s_2021_3,sram_s_2020_3,sram_s_2019_3,sram_s_2018_3,sram_s_2022_4,sram_s_2021_4,sram_s_2020_4,sram_s_2019_4,sram_s_2018_4))

sram_k<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(sram_k_2022_1,sram_k_2021_1,sram_k_2020_1,sram_k_2019_1,sram_k_2018_1,sram_k_2022_2,sram_k_2021_2,sram_k_2020_2,sram_k_2019_2,sram_k_2018_2,sram_k_2022_3,sram_k_2021_3,sram_k_2020_3,sram_k_2019_3,sram_k_2018_3,sram_k_2022_4,sram_k_2021_4,sram_k_2020_4,sram_k_2019_4,sram_k_2018_4))





win.graph(15,10)
g1 <- ggplot(data=sram_l, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='LG sram 관련 특허 최근 5년 심사진행상태')


g2 <- ggplot(data=sram_s, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SAMSUNG sram 관련 특허 최근 5년 심사진행상태')


g3 <- ggplot(data=sram_k, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) +
  geom_point(size=2.5, color='#FFFFFF') + labs(title='SK sram 관련 특허 최근 5년 심사진행상태')


grid.arrange(g1, g2, g3, nrow=1, ncol=3)


# transistor(panel) 키워드 최근 5년(2022 ~ 2018) 심사진행상태 추이
# 등록결정=1, 거절결정=2, 포기=3, 취하=4

pa_lg<-read.csv("panel_lg.csv", header = T)
pa_sam<-read.csv("panel_sam.csv", header = T)
pa_sk<-read.csv("panel_sk.csv", header = T)

# 결측치 제거
pa_lg <- pa_lg[!(pa_lg$심사진행상태 == "" ), ]
pa_sam <- pa_sam[!(pa_sam$심사진행상태 == "" ), ]
pa_sk <- pa_sk[!(pa_sk$심사진행상태 == "" ), ]

# 등록결정
pa_l_2022_1<-rate(pa_lg, 2022, '등록결정')
pa_l_2021_1<-rate(pa_lg, 2021, '등록결정')
pa_l_2020_1<-rate(pa_lg, 2020, '등록결정')
pa_l_2019_1<-rate(pa_lg, 2019, '등록결정')
pa_l_2018_1<-rate(pa_lg, 2018, '등록결정')

pa_s_2022_1<-rate(pa_sam, 2022, '등록결정')
pa_s_2021_1<-rate(pa_sam, 2021, '등록결정')
pa_s_2020_1<-rate(pa_sam, 2020, '등록결정')
pa_s_2019_1<-rate(pa_sam, 2019, '등록결정')
pa_s_2018_1<-rate(pa_sam, 2018, '등록결정')

pa_k_2022_1<-rate(pa_sk, 2022, '등록결정')
pa_k_2021_1<-rate(pa_sk, 2021, '등록결정')
pa_k_2020_1<-rate(pa_sk, 2020, '등록결정')
pa_k_2019_1<-rate(pa_sk, 2019, '등록결정')
pa_k_2018_1<-rate(pa_sk, 2018, '등록결정')

#거절결정
pa_l_2022_2<-rate(pa_lg, 2022, '거절결정')
pa_l_2021_2<-rate(pa_lg, 2021, '거절결정')
pa_l_2020_2<-rate(pa_lg, 2020, '거절결정')
pa_l_2019_2<-rate(pa_lg, 2019, '거절결정')
pa_l_2018_2<-rate(pa_lg, 2018, '거절결정')

pa_s_2022_2<-rate(pa_sam, 2022, '거절결정')
pa_s_2021_2<-rate(pa_sam, 2021, '거절결정')
pa_s_2020_2<-rate(pa_sam, 2020, '거절결정')
pa_s_2019_2<-rate(pa_sam, 2019, '거절결정')
pa_s_2018_2<-rate(pa_sam, 2018, '거절결정')

pa_k_2022_2<-rate(pa_sk, 2022, '거절결정')
pa_k_2021_2<-rate(pa_sk, 2021, '거절결정')
pa_k_2020_2<-rate(pa_sk, 2020, '거절결정')
pa_k_2019_2<-rate(pa_sk, 2019, '거절결정')
pa_k_2018_2<-rate(pa_sk, 2018, '거절결정')



# 포기
pa_l_2022_3<-rate(pa_lg, 2022, '포기')
pa_l_2021_3<-rate(pa_lg, 2021, '포기')
pa_l_2020_3<-rate(pa_lg, 2020, '포기')
pa_l_2019_3<-rate(pa_lg, 2019, '포기')
pa_l_2018_3<-rate(pa_lg, 2018, '포기')

pa_s_2022_3<-rate(pa_sam, 2022, '포기')
pa_s_2021_3<-rate(pa_sam, 2021, '포기')
pa_s_2020_3<-rate(pa_sam, 2020, '포기')
pa_s_2019_3<-rate(pa_sam, 2019, '포기')
pa_s_2018_3<-rate(pa_sam, 2018, '포기')

pa_k_2022_3<-rate(pa_sk, 2022, '포기')
pa_k_2021_3<-rate(pa_sk, 2021, '포기')
pa_k_2020_3<-rate(pa_sk, 2020, '포기')
pa_k_2019_3<-rate(pa_sk, 2019, '포기')
pa_k_2018_3<-rate(pa_sk, 2018, '포기')



# 취하
pa_l_2022_4<-rate(pa_lg, 2022, '취하')
pa_l_2021_4<-rate(pa_lg, 2021, '취하')
pa_l_2020_4<-rate(pa_lg, 2020, '취하')
pa_l_2019_4<-rate(pa_lg, 2019, '취하')
pa_l_2018_4<-rate(pa_lg, 2018, '취하')

pa_s_2022_4<-rate(pa_sam, 2022, '취하')
pa_s_2021_4<-rate(pa_sam, 2021, '취하')
pa_s_2020_4<-rate(pa_sam, 2020, '취하')
pa_s_2019_4<-rate(pa_sam, 2019, '취하')
pa_s_2018_4<-rate(pa_sam, 2018, '취하')

pa_k_2022_4<-rate(pa_sk, 2022, '취하')
pa_k_2021_4<-rate(pa_sk, 2021, '취하')
pa_k_2020_4<-rate(pa_sk, 2020, '취하')
pa_k_2019_4<-rate(pa_sk, 2019, '취하')
pa_k_2018_4<-rate(pa_sk, 2018, '취하')


pa_l<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(pa_l_2022_1,pa_l_2021_1,pa_l_2020_1,pa_l_2019_1,pa_l_2018_1,pa_l_2022_2,pa_l_2021_2,pa_l_2020_2,pa_l_2019_2,pa_l_2018_2,pa_l_2022_3,pa_l_2021_3,pa_l_2020_3,pa_l_2019_3,pa_l_2018_3,pa_l_2022_4,pa_l_2021_4,pa_l_2020_4,pa_l_2019_4,pa_l_2018_4))

pa_s<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(pa_s_2022_1,pa_s_2021_1,pa_s_2020_1,pa_s_2019_1,pa_s_2018_1,pa_s_2022_2,pa_s_2021_2,pa_s_2020_2,pa_s_2019_2,pa_s_2018_2,pa_s_2022_3,pa_s_2021_3,pa_s_2020_3,pa_s_2019_3,pa_s_2018_3,pa_s_2022_4,pa_s_2021_4,pa_s_2020_4,pa_s_2019_4,pa_s_2018_4))

pa_k<-data.frame('심사진행상태'=c('등록결정','등록결정','등록결정','등록결정','등록결정','거절결정', '거절결정', '거절결정', '거절결정', '거절결정','포기','포기','포기','포기','포기','취하','취하','취하','취하','취하'), '연도'=c(2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018,2022,2021,2020,2019,2018), '비율'= c(pa_k_2022_1,pa_k_2021_1,pa_k_2020_1,pa_k_2019_1,pa_k_2018_1,pa_k_2022_2,pa_k_2021_2,pa_k_2020_2,pa_k_2019_2,pa_k_2018_2,pa_k_2022_3,pa_k_2021_3,pa_k_2020_3,pa_k_2019_3,pa_k_2018_3,pa_k_2022_4,pa_k_2021_4,pa_k_2020_4,pa_k_2019_4,pa_k_2018_4))




win.graph(15,10)
g1<-ggplot(data=pa_l, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) + geom_point(size=2.5, color='#FFFFFF') + labs(title='LG panel 관련 특허 최근 5년 심사진행상태')


g2 <- ggplot(data=pa_s, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) + geom_point(size=2.5, color='#FFFFFF') + labs(title='SAMSUNG panel 관련 특허 최근 5년 심사진행상태')


g3 <- ggplot(data=pa_k, aes(x=연도,y=비율,group=심사진행상태,color=심사진행상태))+ geom_line(size=1.3) + geom_point(size=4) + geom_point(size=2.5, color='#FFFFFF') + labs(title='SK panel 관련 특허 최근 5년 심사진행상태')


grid.arrange(g1, g2, g3, nrow=1, ncol=3)
