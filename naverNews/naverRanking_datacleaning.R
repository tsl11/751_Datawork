### naver ranking news and naver ranking keyword data 
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(KoNLP)
library(ggplot2)
library(lubridate)

## loading data
weeklyClick_2016 = read.csv("naver_weeklyClick_2016.csv")
weeklyClick_O14 = read.csv("ranking_news_2014.csv")
weeklyKeyword_2014 = read.csv("naver_weeklyKeyword_2014.csv")
weeklyKeyword_2016 = read.csv("naver_weeklyKeyword_2016.csv")

weeklyClick_2014_original = weeklyClick_O14 %>% mutate(datetime = as.Date.POSIXct(datetime)) %>%
  arrange(datetime) %>% mutate(week = c(rep(1:{nrow(.)/30}, each = 30)))
weeklyClick_2014 = weeklyClick_2014_original[1:480,] 

weeklyClick_2016 = weeklyClick_2016 %>% mutate(datetime = as.Date.POSIXct(datetime)) %>%
  arrange(datetime) %>% mutate(week = c(rep(1:{nrow(.)/30}, each = 30)))

######## filtering by relevant keyword
scandal_2014 = c("정윤회", "문건", "유출", "문건유출", "조응천", "최경위", "박관천", "비선", "문고리","십상시")
index_2014 = weeklyClick_2014$title %>% lapply(function(x) str_detect(x, scandal_2014) %>% any()) %>% unlist()

Click_14 = weeklyClick_2014[index_2014,] 
mostview_14 = Click_14 %>% group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30})

scandal_2016 = c("최순실", "정유라", "이대", "미르", "연설문", "K스포츠", "박근혜", "비선", "광화문", "국정농단")
index_2016 = weeklyClick_2016$body %>% lapply(function(x) str_detect(x, scandal_2016) %>% any()) %>% unlist()

Click_16 = weeklyClick_2016[index_2016,] 
mostview_16 = Click_16 %>% group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30})


## Visualization
mostview_14 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() + ylim(0,1) 

mostview_16 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() 

########ranking keyword

top20_14 = t(weeklyKeyword_2014) %>% {.[-(1:2),]}
colnames(top20_14) <- 1:20
rownames(top20_14) = ymd("2014-11-28") + c(0:15)*week(1)

