rm(list=ls()) 
setwd("C:/haha")

##LG Display
#데이터 불러오기 및 전처리
display_lg <- read.csv("display_lg.csv", header=T)
display_lg <- display_lg[!(display_lg$심사진행상태 == ""), ]
display_lg$심사진행상태 <- gsub("\\(.*?\\)","",display_lg$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지
getwd()
.libPaths("C:/haha")
#install.packages("stringr")
library(stringr)
display_lg$심사진행상태 <- substr(display_lg$심사진행상태, 1, 5)
display_lg$심사진행상태 <- str_trim(display_lg$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
display_lg_1 <- nrow(display_lg[display_lg$심사진행상태 == "등록결정",])
display_lg_1 <- nrow(display_lg[display_lg$심사진행상태 == "등록결정",])
display_lg_2 <- nrow(display_lg[display_lg$심사진행상태 == "거절결정",])
display_lg_3 <- nrow(display_lg[display_lg$심사진행상태 == "포기",])
display_lg_4 <- nrow(display_lg[display_lg$심사진행상태 == "취하",])
display_lg_5 <- nrow(display_lg[display_lg$심사진행상태 == "원결정유지",])
display_lg <- nrow(display_lg)

#lg사 display 관련 특허건 개수
display_lg #39209 #lg사 display 관련 특허 건 수
display_lg_1 #26915 #lg사 display 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
display_lg_2 #2996 #lg사 display 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
display_lg_3 #54 #lg사 display 관련 특허 중 포기(미납) 건 개수
display_lg_4 #8755 #lg사 display 관련 특허 중 취하(심사미청구) 건 개수
display_lg_5 #422 #lg사 display 관련 특허 중 원결정유지 건 개수

#lg사 display 관련 특허건 비율
display_lg_1_per <- display_lg_1 / display_lg 
display_lg_2_per <- display_lg_2 / display_lg
display_lg_3_per <- display_lg_3 / display_lg 
display_lg_4_per <- display_lg_4 / display_lg 
display_lg_5_per <- display_lg_5 / display_lg #원결정유지건은 제외
display_lg_1_per #0.6864495
display_lg_2_per #0.07641103
display_lg_3_per #0.001377235
display_lg_4_per #0.2232906

#lg사 display 관련 특허건
display_lg <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                         "비율"=c(display_lg_1_per,display_lg_2_per,display_lg_3_per,display_lg_4_per))


## Lg Led
#데이터 불러오기 및 전처리
led_lg <- read.csv("led_lg.csv", header=T)
led_lg <- led_lg[!(led_lg$심사진행상태 == ""), ]
led_lg$심사진행상태 <- gsub("\\(.*?\\)","",led_lg$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지
getwd()
.libPaths("C:/haha")
#install.packages("stringr")
library(stringr)
led_lg$심사진행상태 <- substr(led_lg$심사진행상태, 1, 5)
led_lg$심사진행상태 <- str_trim(led_lg$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
led_lg_1 <- nrow(led_lg[led_lg$심사진행상태 == "등록결정",])
led_lg_1 <- nrow(led_lg[led_lg$심사진행상태 == "등록결정",])
led_lg_2 <- nrow(led_lg[led_lg$심사진행상태 == "거절결정",])
led_lg_3 <- nrow(led_lg[led_lg$심사진행상태 == "포기",])
led_lg_4 <- nrow(led_lg[led_lg$심사진행상태 == "취하",])
led_lg_5 <- nrow(led_lg[led_lg$심사진행상태 == "원결정유지",])
led_lg <- nrow(led_lg)

#lg사 led 관련 특허건 개수
led_lg #1056 #lg사 led 관련 특허 건 수
led_lg_1 #696 #lg사 led 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
led_lg_2 #160 #lg사 led 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
led_lg_3 #5 #lg사 led 관련 특허 중 포기(미납) 건 개수
led_lg_4 #188 #lg사 led 관련 특허 중 취하(심사미청구) 건 개수
led_lg_5 #5 #lg사 led 관련 특허 중 원결정유지 건 개수

#lg사 led 관련 특허건 비율
led_lg_1_per <- led_lg_1 / led_lg 
led_lg_2_per <- led_lg_2 / led_lg
led_lg_3_per <- led_lg_3 / led_lg 
led_lg_4_per <- led_lg_4 / led_lg 
led_lg_5_per <- led_lg_5 / led_lg #원결정유지건은 제외
led_lg_1_per #0.6590909
led_lg_2_per #0.1515152
led_lg_3_per #0.004734848
led_lg_4_per #0.1780303


#lg사 led 관련 특허건
led_lg <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                     "비율"=c(led_lg_1_per,led_lg_2_per,led_lg_3_per,led_lg_4_per))

##LG ram
#데이터 불러오기 및 전처리
ram_lg <- read.csv("ram_lg.csv", header=T)
ram_lg <- ram_lg[!(ram_lg$심사진행상태 == ""), ]
ram_lg$심사진행상태 <- gsub("\\(.*?\\)","",ram_lg$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지
getwd()
.libPaths("C:/haha")
#install.packages("stringr")
library(stringr)
ram_lg$심사진행상태 <- substr(ram_lg$심사진행상태, 1, 5)
ram_lg$심사진행상태 <- str_trim(ram_lg$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
ram_lg_1 <- nrow(ram_lg[ram_lg$심사진행상태 == "등록결정",])
ram_lg_1 <- nrow(ram_lg[ram_lg$심사진행상태 == "등록결정",])
ram_lg_2 <- nrow(ram_lg[ram_lg$심사진행상태 == "거절결정",])
ram_lg_3 <- nrow(ram_lg[ram_lg$심사진행상태 == "포기",])
ram_lg_4 <- nrow(ram_lg[ram_lg$심사진행상태 == "취하",])
ram_lg_5 <- nrow(ram_lg[ram_lg$심사진행상태 == "원결정유지",])
ram_lg <- nrow(ram_lg)

#lg사 ram 관련 특허건 개수
ram_lg #69 #lg사 ram 관련 특허 건 수
ram_lg_1 #36 #lg사 ram 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
ram_lg_2 #5 #lg사 ram 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
ram_lg_3 #0 #lg사 ram 관련 특허 중 포기(미납) 건 개수
ram_lg_4 #27 #lg사 ram 관련 특허 중 취하(심사미청구) 건 개수
ram_lg_5 #0 #lg사 ram 관련 특허 중 원결정유지 건 개수

#lg사 ram 관련 특허건 비율
ram_lg_1_per <- ram_lg_1 / ram_lg 
ram_lg_2_per <- ram_lg_2 / ram_lg
ram_lg_3_per <- ram_lg_3 / ram_lg 
ram_lg_4_per <- ram_lg_4 / ram_lg 
ram_lg_5_per <- ram_lg_5 / ram_lg #원결정유지건은 제외
ram_lg_1_per #0.5217391
ram_lg_2_per #0.07246377
ram_lg_3_per #0
ram_lg_4_per #0.3913043


#lg사 ram 관련 특허건
ram_lg <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                     "비율"=c(ram_lg_1_per,ram_lg_2_per,ram_lg_3_per,ram_lg_4_per))


##LG transistor
#데이터 불러오기 및 전처리
transistor_lg <- read.csv("transistor_lg.csv", header=T)
transistor_lg <- transistor_lg[!(transistor_lg$심사진행상태 == ""), ]
transistor_lg$심사진행상태 <- gsub("\\(.*?\\)","",transistor_lg$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지
getwd()
.libPaths("C:/haha")
#install.packages("stringr")
library(stringr)
transistor_lg$심사진행상태 <- substr(transistor_lg$심사진행상태, 1, 5)
transistor_lg$심사진행상태 <- str_trim(transistor_lg$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
transistor_lg_1 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "등록결정",])
transistor_lg_1 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "등록결정",])
transistor_lg_2 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "거절결정",])
transistor_lg_3 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "포기",])
transistor_lg_4 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "취하",])
transistor_lg_5 <- nrow(transistor_lg[transistor_lg$심사진행상태 == "원결정유지",])
transistor_lg <- nrow(transistor_lg)

#lg사 transistor 관련 특허건 개수
transistor_lg 
transistor_lg_1 
transistor_lg_2
transistor_lg_3
transistor_lg_4
transistor_lg_5

#lg사 transistor 관련 특허건 비율
transistor_lg_1_per <- transistor_lg_1 / transistor_lg 
transistor_lg_2_per <- transistor_lg_2 / transistor_lg
transistor_lg_3_per <- transistor_lg_3 / transistor_lg
transistor_lg_4_per <- transistor_lg_4 / transistor_lg
transistor_lg_5_per <- transistor_lg_5 / transistor_lg #원결정유지건은 제외
transistor_lg_1_per 
transistor_lg_2_per 
transistor_lg_3_per 
transistor_lg_4_per 


#lg사 transistor 관련 특허건
transistor_lg <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                            "비율"=c(transistor_lg_1_per,transistor_lg_2_per,transistor_lg_3_per,transistor_lg_4_per))



##sam dis
#데이터 불러오기 및 전처리
display_samsung <- read.csv("C:/Users/dnjs1/Downloads/Rr/display_samsung.csv", header=T)
display_samsung <- display_samsung[!(display_samsung$심사진행상태 == ""), ]
display_samsung$심사진행상태 <- gsub("\\(.*?\\)","",display_samsung$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

display_samsung$심사진행상태 <- substr(display_samsung$심사진행상태, 1, 5)
display_samsung$심사진행상태 <- str_trim(display_samsung$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
display_samsung_1 <- nrow(display_samsung[display_samsung$심사진행상태 == "등록결정",])
display_samsung_1 <- nrow(display_samsung[display_samsung$심사진행상태 == "등록결정",])
display_samsung_2 <- nrow(display_samsung[display_samsung$심사진행상태 == "거절결정",])
display_samsung_3 <- nrow(display_samsung[display_samsung$심사진행상태 == "포기",])
display_samsung_4 <- nrow(display_samsung[display_samsung$심사진행상태 == "취하",])
display_samsung_5 <- nrow(display_samsung[display_samsung$심사진행상태 == "원결정유지",])
display_samsung <- nrow(display_samsung)

#samsung사 display 관련 특허건 개수
display_samsung #43934 #samsung사 display 관련 특허 건 수
display_samsung_1 #27026 #samsung사 display 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
display_samsung_2 #4840 #samsung사 display 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
display_samsung_3 #379 #samsung사 display 관련 특허 중 포기(미납) 건 개수
display_samsung_4 #11326 #samsung사 display 관련 특허 중 취하(심사미청구) 건 개수
display_samsung_5 #259 #samsung사 display 관련 특허 중 원결정유지 건 개수

#samsung사 display 관련 특허건 비율
display_samsung_1_per <- display_samsung_1 / display_samsung 
display_samsung_2_per <- display_samsung_2 / display_samsung
display_samsung_3_per <- display_samsung_3 / display_samsung
display_samsung_4_per <- display_samsung_4 / display_samsung
display_samsung_5_per <- display_samsung_5 / display_samsung #원결정유지건은 제외
display_samsung_1_per #0.61515
display_samsung_2_per #0.1101652
display_samsung_3_per #0.008626576
display_samsung_4_per #0.008626576

#samsung사 display 관련 특허건
display_samsung <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                              "비율"=c(display_samsung_1_per,display_samsung_2_per,display_samsung_3_per,display_samsung_4_per))



##sam led
#데이터 불러오기 및 전처리
led_samsung <- read.csv("C:/Users/dnjs1/Downloads/Rr/led_samsung.csv", header=T)
led_samsung <- led_samsung[!(led_samsung$심사진행상태 == ""), ]
led_samsung$심사진행상태 <- gsub("\\(.*?\\)","",led_samsung$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

led_samsung$심사진행상태 <- substr(led_samsung$심사진행상태, 1, 5)
led_samsung$심사진행상태 <- str_trim(led_samsung$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
led_samsung_1 <- nrow(led_samsung[led_samsung$심사진행상태 == "등록결정",])
led_samsung_1 <- nrow(led_samsung[led_samsung$심사진행상태 == "등록결정",])
led_samsung_2 <- nrow(led_samsung[led_samsung$심사진행상태 == "거절결정",])
led_samsung_3 <- nrow(led_samsung[led_samsung$심사진행상태 == "포기",])
led_samsung_4 <- nrow(led_samsung[led_samsung$심사진행상태 == "취하",])
led_samsung_5 <- nrow(led_samsung[led_samsung$심사진행상태 == "원결정유지",])
led_samsung <- nrow(led_samsung)

#samsung사 led 관련 특허건 개수
led_samsung #1453 #samsung사 led 관련 특허 건 수
led_samsung_1 #874 #samsung사 led 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
led_samsung_2 #134 #samsung사 led 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
led_samsung_3 #22 #samsung사 led 관련 특허 중 포기(미납) 건 개수
led_samsung_4 #416 #samsung사 led 관련 특허 중 취하(심사미청구) 건 개수
led_samsung_5 #5 #samsung사 led 관련 특허 중 원결정유지 건 개수

#samsung사 led 관련 특허건 비율
led_samsung_1_per <- led_samsung_1 / led_samsung 
led_samsung_2_per <- led_samsung_2 / led_samsung
led_samsung_3_per <- led_samsung_3 / led_samsung
led_samsung_4_per <- led_samsung_4 / led_samsung
led_samsung_5_per <- led_samsung_5 / led_samsung #원결정유지건은 제외
led_samsung_1_per #0.6015141
led_samsung_2_per #0.09222299
led_samsung_3_per #0.01514109
led_samsung_4_per #0.2863042


#samsung사 led 관련 특허건
led_samsung <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                          "비율"=c(led_samsung_1_per,led_samsung_2_per,led_samsung_3_per,led_samsung_4_per))


## sam ram
#데이터 불러오기 및 전처리
ram_samsung <- read.csv("C:/Users/dnjs1/Downloads/Rr/ram_samsung.csv", header=T)
ram_samsung <- ram_samsung[!(ram_samsung$심사진행상태 == ""), ]
ram_samsung$심사진행상태 <- gsub("\\(.*?\\)","",ram_samsung$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

ram_samsung$심사진행상태 <- substr(ram_samsung$심사진행상태, 1, 5)
ram_samsung$심사진행상태 <- str_trim(ram_samsung$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
ram_samsung_1 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "등록결정",])
ram_samsung_1 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "등록결정",])
ram_samsung_2 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "거절결정",])
ram_samsung_3 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "포기",])
ram_samsung_4 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "취하",])
ram_samsung_5 <- nrow(ram_samsung[ram_samsung$심사진행상태 == "원결정유지",])
ram_samsung <- nrow(ram_samsung)

#samsung사 ram 관련 특허건 개수
ram_samsung 
ram_samsung_1 
ram_samsung_2
ram_samsung_3
ram_samsung_4
ram_samsung_5

#samsung사 ram 관련 특허건 비율
ram_samsung_1_per <- ram_samsung_1 / ram_samsung 
ram_samsung_2_per <- ram_samsung_2 / ram_samsung
ram_samsung_3_per <- ram_samsung_3 / ram_samsung
ram_samsung_4_per <- ram_samsung_4 / ram_samsung
ram_samsung_5_per <- ram_samsung_5 / ram_samsung #원결정유지건은 제외
ram_samsung_1_per 
ram_samsung_2_per 
ram_samsung_3_per 
ram_samsung_4_per 


#samsung사 ram 관련 특허건 비율
ram_samsung <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                          "비율"=c(ram_samsung_1_per,ram_samsung_2_per,ram_samsung_3_per,ram_samsung_4_per))




##sam tran
#데이터 불러오기 및 전처리
transistor_samsung <- read.csv("C:/Users/dnjs1/Downloads/Rr/transistor_samsung.csv", header=T)
transistor_samsung <- transistor_samsung[!(transistor_samsung$심사진행상태 == ""), ]
transistor_samsung$심사진행상태 <- gsub("\\(.*?\\)","",transistor_samsung$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

transistor_samsung$심사진행상태 <- substr(transistor_samsung$심사진행상태, 1, 5)
transistor_samsung$심사진행상태 <- str_trim(transistor_samsung$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
transistor_samsung_1 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "등록결정",])
transistor_samsung_1 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "등록결정",])
transistor_samsung_2 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "거절결정",])
transistor_samsung_3 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "포기",])
transistor_samsung_4 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "취하",])
transistor_samsung_5 <- nrow(transistor_samsung[transistor_samsung$심사진행상태 == "원결정유지",])
transistor_samsung <- nrow(transistor_samsung)

#samsung사 transistor 관련 특허건 개수
transistor_samsung 
transistor_samsung_1 
transistor_samsung_2
transistor_samsung_3
transistor_samsung_4
transistor_samsung_5

#samsung사 transistor 관련 특허건 비율
transistor_samsung_1_per <- transistor_samsung_1 / transistor_samsung 
transistor_samsung_2_per <- transistor_samsung_2 / transistor_samsung
transistor_samsung_3_per <- transistor_samsung_3 / transistor_samsung
transistor_samsung_4_per <- transistor_samsung_4 / transistor_samsung
transistor_samsung_5_per <- transistor_samsung_5 / transistor_samsung #원결정유지건은 제외
transistor_samsung_1_per 
transistor_samsung_2_per 
transistor_samsung_3_per 
transistor_samsung_4_per 

#samsung사 transistor 관련 특허건 비율 그래프
transistor_samsung <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                                 "비율"=c(transistor_samsung_1_per,transistor_samsung_2_per,transistor_samsung_3_per,transistor_samsung_4_per))



##sk dis
#데이터 불러오기 및 전처리
display_sk <- read.csv("C:/Users/dnjs1/Downloads/Rr/skdisplay.csv", header=T, fileEncoding="euc-kr")
display_sk <- display_sk[!(display_sk$심사진행상태 == ""), ]
display_sk$심사진행상태 <- gsub("\\(.*?\\)","",display_sk$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

display_sk$심사진행상태 <- substr(display_sk$심사진행상태, 1, 5)
display_sk$심사진행상태 <- str_trim(display_sk$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
display_sk_1 <- nrow(display_sk[display_sk$심사진행상태 == "등록결정",])
display_sk_1 <- nrow(display_sk[display_sk$심사진행상태 == "등록결정",])
display_sk_2 <- nrow(display_sk[display_sk$심사진행상태 == "거절결정",])
display_sk_3 <- nrow(display_sk[display_sk$심사진행상태 == "포기",])
display_sk_4 <- nrow(display_sk[display_sk$심사진행상태 == "취하",])
display_sk_5 <- nrow(display_sk[display_sk$심사진행상태 == "원결정유지",])
display_sk <- nrow(display_sk)

#sk사 display 관련 특허건 개수
display_sk #314 #sk사 display 관련 특허 건 수
display_sk_1 #221 #sk사 display 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
display_sk_2 #42 #sk사 display 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
display_sk_3 #6 #sk사 display 관련 특허 중 포기(미납) 건 개수
display_sk_4 #43 #sk사 display 관련 특허 중 취하(심사미청구) 건 개수
display_sk_5 #0 #sk사 display 관련 특허 중 원결정유지 건 개수

#sk사 display 관련 특허건 비율
display_sk_1_per <- display_sk_1 / display_sk 
display_sk_2_per <- display_sk_2 / display_sk
display_sk_3_per <- display_sk_3 / display_sk 
display_sk_4_per <- display_sk_4 / display_sk 
display_sk_5_per <- display_sk_5 / display_sk #원결정유지건은 제외
display_sk_1_per #0.7038217
display_sk_2_per #0.133758
display_sk_3_per #0.01910828
display_sk_4_per #0.1369427


#sk사 display 관련 특허건 비율
display_sk <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                         "비율"=c(display_sk_1_per,display_sk_2_per,display_sk_3_per,display_sk_4_per))



##sk led
#데이터 불러오기 및 전처리
led_sk <- read.csv("led_sk.csv", header=T, fileEncoding="euc-kr")
led_sk <- led_sk[!(led_sk$심사진행상태 == ""), ]
led_sk$심사진행상태 <- gsub("\\(.*?\\)","",led_sk$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지
getwd()
.libPaths("C:/haha")
#install.packages("stringr")
library(stringr)
led_sk$심사진행상태 <- substr(led_sk$심사진행상태, 1, 5)
led_sk$심사진행상태 <- str_trim(led_sk$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
led_sk_1 <- nrow(led_sk[led_sk$심사진행상태 == "등록결정",])
led_sk_1 <- nrow(led_sk[led_sk$심사진행상태 == "등록결정",])
led_sk_2 <- nrow(led_sk[led_sk$심사진행상태 == "거절결정",])
led_sk_3 <- nrow(led_sk[led_sk$심사진행상태 == "포기",])
led_sk_4 <- nrow(led_sk[led_sk$심사진행상태 == "취하",])
led_sk_5 <- nrow(led_sk[led_sk$심사진행상태 == "원결정유지",])
led_sk <- nrow(led_sk)

#sk사 led 관련 특허건 개수
led_sk #153 #sk사 led 관련 특허 건 수
led_sk_1 #98 #sk사 led 관련 특허 중 등록결정 건 개수 (재심사 후 등록결정건 포함)
led_sk_2 #11 #sk사 led 관련 특허 중 거절결정 건 개수(거절결정 후 재심사중인 건 포함)
led_sk_3 #2 #sk사 led 관련 특허 중 포기(미납) 건 개수
led_sk_4 #41 #sk사 led 관련 특허 중 취하(심사미청구) 건 개수
led_sk_5 #1 #sk사 led 관련 특허 중 원결정유지 건 개수

#SK사 led 관련 특허건 비율
led_sk_1_per <- led_sk_1 / led_sk 
led_sk_2_per <- led_sk_2 / led_sk
led_sk_3_per <- led_sk_3 / led_sk 
led_sk_4_per <- led_sk_4 / led_sk 
led_sk_5_per <- led_sk_5 / led_sk #원결정유지건은 제외
led_sk_1_per #0.6405229
led_sk_2_per # 0.07189542
led_sk_3_per #0.0130719
led_sk_4_per # 0.2679739

#SK사 led 관련 특허건 비율
led_sk <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                     "비율"=c(led_sk_1_per,led_sk_2_per,led_sk_3_per,led_sk_4_per))


##sk ram
#데이터 불러오기 및 전처리
ram_sk <- read.csv("ram_sk.csv", header=T, fileEncoding = "euc-kr")
ram_sk <- ram_sk[!(ram_sk$심사진행상태 == ""), ]
ram_sk$심사진행상태 <- gsub("\\(.*?\\)","",ram_sk$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

ram_sk$심사진행상태 <- substr(ram_sk$심사진행상태, 1, 5)
ram_sk$심사진행상태 <- str_trim(ram_sk$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
ram_sk_1 <- nrow(ram_sk[ram_sk$심사진행상태 == "등록결정",])
ram_sk_1 <- nrow(ram_sk[ram_sk$심사진행상태 == "등록결정",])
ram_sk_2 <- nrow(ram_sk[ram_sk$심사진행상태 == "거절결정",])
ram_sk_3 <- nrow(ram_sk[ram_sk$심사진행상태 == "포기",])
ram_sk_4 <- nrow(ram_sk[ram_sk$심사진행상태 == "취하",])
ram_sk_5 <- nrow(ram_sk[ram_sk$심사진행상태 == "원결정유지",])
ram_sk <- nrow(ram_sk)

#sk사 ram 관련 특허건 개수
ram_sk 
ram_sk_1 
ram_sk_2
ram_sk_3
ram_sk_4
ram_sk_5

#sk사 ram 관련 특허건 비율
ram_sk_1_per <- ram_sk_1 / ram_sk 
ram_sk_2_per <- ram_sk_2 / ram_sk
ram_sk_3_per <- ram_sk_3 / ram_sk
ram_sk_4_per <- ram_sk_4 / ram_sk
ram_sk_5_per <- ram_sk_5 / ram_sk #원결정유지건은 제외
ram_sk_1_per 
ram_sk_2_per 
ram_sk_3_per 
ram_sk_4_per 

#sk사 ram 관련 특허건 비율
ram_sk <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                     "비율"=c(ram_sk_1_per,ram_sk_2_per,ram_sk_3_per,ram_sk_4_per))


##sk tran
#데이터 불러오기 및 전처리
transistor_sk <- read.csv("transistor_sk.csv", header=T, fileEncoding = "euc-kr")
transistor_sk <- transistor_sk[!(transistor_sk$심사진행상태 == ""), ]
transistor_sk$심사진행상태 <- gsub("\\(.*?\\)","",transistor_sk$심사진행상태)
#등록결정/거절결정/포기/취하(심사미청구)/기타=원결정유지

transistor_sk$심사진행상태 <- substr(transistor_sk$심사진행상태, 1, 5)
transistor_sk$심사진행상태 <- str_trim(transistor_sk$심사진행상태)

#각 항목별 개수 파악 (등록결정=1, 거절결정=2, 포기=3, 취하=4, 기타=5(원결정유지) )
transistor_sk_1 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "등록결정",])
transistor_sk_1 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "등록결정",])
transistor_sk_2 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "거절결정",])
transistor_sk_3 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "포기",])
transistor_sk_4 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "취하",])
transistor_sk_5 <- nrow(transistor_sk[transistor_sk$심사진행상태 == "원결정유지",])
transistor_sk <- nrow(transistor_sk)

#sk사 transistor 관련 특허건 개수
transistor_sk 
transistor_sk_1 
transistor_sk_2
transistor_sk_3
transistor_sk_4
transistor_sk_5

#sk사 transistor 관련 특허건 비율
transistor_sk_1_per <- transistor_sk_1 / transistor_sk 
transistor_sk_2_per <- transistor_sk_2 / transistor_sk
transistor_sk_3_per <- transistor_sk_3 / transistor_sk
transistor_sk_4_per <- transistor_sk_4 / transistor_sk
transistor_sk_5_per <- transistor_sk_5 / transistor_sk #원결정유지건은 제외
transistor_sk_1_per 
transistor_sk_2_per 
transistor_sk_3_per 
transistor_sk_4_per 


#sk사 transistor 관련 특허건 비율
transistor_sk <- data.frame("심사진행상태"=c("등록결정","거절결정","포기","취하"),
                            "비율"=c(transistor_sk_1_per,transistor_sk_2_per,transistor_sk_3_per,transistor_sk_4_per))





d_l = ggplot(display_lg, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "LG사 display 관련 특허건 비율")

l_l = ggplot(led_lg, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
     geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
     geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
     labs(title = "LG사 led 관련 특허건 비율")

r_l = ggplot(ram_lg, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "LG사 ram 관련 특허건 비율")

t_l = ggplot(transistor_lg, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "LG사 transistor 관련 특허건 비율")

d_s = ggplot(display_samsung, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Samsung사 display 관련 특허건 비율")

l_s = ggplot(led_samsung, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Samsung사 led 관련 특허건 비율")

r_s = ggplot(ram_samsung, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Samsung사 ram 관련 특허건 비율")

t_s = ggplot(transistor_samsung, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Samsung사 transistor 관련 특허건 비율")

d_sk = ggplot(display_sk, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Sk사 display 관련 특허건 비율")

l_sk = ggplot(led_sk, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
       geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
       geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
       labs(title = "Sk사 led 관련 특허건 비율")

r_sk = ggplot(ram_sk, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
        geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
        geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
        labs(title = "Sk사 ram 관련 특허건 비율")

t_sk = ggplot(transistor_sk, aes(x=심사진행상태, y=비율, fill=심사진행상태))+
        geom_bar(mapping=aes(fill=심사진행상태),stat = "identity")+
        geom_label(aes(label=round(비율,3),nudge_y = 1.1)) +
        labs(title = "Sk사 transistor 관련 특허건 비율")

grid.arrange(d_l,l_l,r_l,t_l,
             d_s,l_s,r_s,t_s,
             d_sk,l_sk,r_sk,t_sk,
             nrow=3, ncol=4)
