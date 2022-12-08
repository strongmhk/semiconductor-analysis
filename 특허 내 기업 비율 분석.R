rm(list=ls()) 
setwd("C:/haha")

##Display
#데이터 불러오기 및 전처리
display_lg <- read.csv("display_lg.csv", header=T)
display_lg <- nrow(display_lg)
display_samsung <- read.csv("display_samsung.csv", header=T)
display_samsung <- nrow(display_samsung)
display_sk <- read.csv("display_sk.csv", header=T, fileEncoding="euc-kr")
display_sk <- nrow(display_sk)
display_all <- sum(display_lg,display_samsung,display_sk)
#비율
display_lg <- display_lg/display_all
display_samsung <- display_samsung/display_all
display_sk <- display_sk/display_all

display <- data.frame("기업"=c("LG","SAMSUNG","SK"),
                      "퍼센트"=c(display_lg*100,display_samsung*100,display_sk*100))


##led
#데이터 불러오기 및 전처리
led_lg <- read.csv("led_lg.csv", header=T)
led_lg <- nrow(led_lg)
led_samsung <- read.csv("led_samsung.csv", header=T)
led_samsung <- nrow(led_samsung)
led_sk <- read.csv("led_sk.csv", header=T, fileEncoding="euc-kr")
led_sk <- nrow(led_sk)
led_all <- sum(led_lg,led_samsung,led_sk)
#비율
led_lg <- led_lg/led_all
led_samsung <- led_samsung/led_all
led_sk <- led_sk/led_all

led <- data.frame("기업"=c("LG","SAMSUNG","SK"),
                  "퍼센트"=c(led_lg*100,led_samsung*100,led_sk*100))

##ram
#데이터 불러오기 및 전처리
ram_lg <- read.csv("ram_lg.csv", header=T)
ram_lg <- nrow(ram_lg)
ram_samsung <- read.csv("ram_samsung.csv", header=T)
ram_samsung <- nrow(ram_samsung)
ram_sk <- read.csv("ram_sk.csv", header=T, fileEncoding="euc-kr")
ram_sk <- nrow(ram_sk)
ram_all <- sum(ram_lg,ram_samsung,ram_sk)
#비율
ram_lg <- ram_lg/ram_all
ram_samsung <- ram_samsung/ram_all
ram_sk <- ram_sk/ram_all

ram <- data.frame("기업"=c("LG","SAMSUNG","SK"),
                  "퍼센트"=c(ram_lg*100,ram_samsung*100,ram_sk*100))

##transistor
#데이터 불러오기 및 전처리
transistor_lg <- read.csv("transistor_lg.csv", header=T)
transistor_lg <- nrow(transistor_lg)
transistor_samsung <- read.csv("transistor_samsung.csv", header=T)
transistor_samsung <- nrow(transistor_samsung)
transistor_sk <- read.csv("transistor_sk.csv", header=T, fileEncoding="euc-kr")
transistor_sk <- nrow(transistor_sk)
transistor_all <- sum(transistor_lg,transistor_samsung,transistor_sk)
#비율
transistor_lg <- transistor_lg/transistor_all
transistor_samsung <- transistor_samsung/transistor_all
transistor_sk <- transistor_sk/transistor_all

transistor <- data.frame("기업"=c("LG","SAMSUNG","SK"),
                         "퍼센트"=c(transistor_lg*100,transistor_samsung*100,transistor_sk*100))


##그래프
getwd()
.libPaths("C:/haha")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)

dis = ggplot(display, aes(x = '', y = 퍼센트, fill = 기업)) +
       geom_bar(width=0.5, stat = "identity", color="white") +
       coord_polar("y",start=1)+
       geom_text(aes(label = paste0(round(퍼센트,2),"%")),
                 position = position_stack(vjust = 0.5))+
       theme_void()+
       labs(title = "Display 관련 특허 내 각 기업 비율")

led = ggplot(led, aes(x = '', y = 퍼센트, fill = 기업)) +
       geom_bar(width=0.5, stat = "identity", color="white") +
       coord_polar("y",start=1)+
       geom_text(aes(label = paste0(round(퍼센트,2),"%")),
                     position = position_stack(vjust = 0.5))+
       theme_void()+
       labs(title = "led 관련 특허 내 각 기업 비율")

ram = ggplot(ram, aes(x = '', y = 퍼센트, fill = 기업)) +
       geom_bar(width=0.5, stat = "identity", color="white") +
        coord_polar("y",start=1)+
       geom_text(aes(label = paste0(round(퍼센트,2),"%")),
                     position = position_stack(vjust = 0.5))+
       theme_void()+
       labs(title = "ram 관련 특허 내 각 기업 비율")

tran = ggplot(transistor, aes(x = '', y = 퍼센트, fill = 기업)) +
        geom_bar(width=0.5, stat = "identity", color="white") +
        coord_polar("y",start=1)+
        geom_text(aes(label = paste0(round(퍼센트,2),"%")),
                      position = position_stack(vjust = 0.5))+
        theme_void()+
        labs(title = "tansistor 관련 특허 내 각 기업 비율")

grid.arrange(dis, ram, led, tran, nrow=2, ncol=2)

