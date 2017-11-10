### naver ranking news and naver ranking keyword data 
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(KoNLP)
library(ggplot2)
library(lubridate)
library(tidyr)

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

####################
##1. 정윤회########
scandal_2014 = c("정윤회", "문건", "유출", "문건유출", "조응천", "최경위", "박관천", "비선", "문고리","십상시")
index_2014 = weeklyClick_2014$title %>% lapply(function(x) str_detect(x, scandal_2014) %>% any()) %>% unlist()

Click_14 = weeklyClick_2014[index_2014,] 
mostview_14 = Click_14 %>% group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30},
         year = c(rep(2014.1, 16))) 

####################
## 2. 땅콩##########

scandal_2014_2 = c("땅콩", "회항", "조현아", "대한항공")
index_2014_2 = weeklyClick_2014$title %>% lapply(function(x) str_detect(x, scandal_2014_2) %>% any()) %>% unlist()

Click_14_2 = weeklyClick_2014[index_2014_2,] 
mostview_14_2 = Click_14_2 %>% group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30},
         year = c(rep(2014.2, 16))) 

###################
## 3. 최순실######

scandal_2016 = c("최순실", "정유라", "이대", "미르", "연설문", "K스포츠", "박근혜", "비선", "광화문", "국정농단")
index_2016 = weeklyClick_2016$body %>% lapply(function(x) str_detect(x, scandal_2016) %>% any()) %>% unlist()

Click_16 = weeklyClick_2016[index_2016,] 
mostview_16 = Click_16 %>% group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30})

Clicks = bind_rows(Click_14, Click_14_2, Click_16)

mostview = Clicks %>% group_by(week, year) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30})

## Visualization
mostview_14 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() + ylim(0,1) 

mostview_16 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() 

mostview %>%
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x=week, y=portion, color = year)) + geom_line() + geom_point() 


#########################################################################
###### Entire ranking news 
W14 = ISOdate(2014,11,22) %>% seq(., by = "week", length.out = 16) 
W16 = ISOdate(2016,09,17) %>% seq(., by = "week", length.out = 16) 

rank_14 = read.csv("naver_weeklyClick000_2014.csv")
rank_16 = read.csv("naver_weeklyClick000_2016.csv")

rank_14 = rank_14 %>% mutate(datetime = as.Date.POSIXct(datetime)) %>%
  arrange(datetime)

rank_16 = rank_16 %>% mutate(datetime = as.Date.POSIXct(datetime)) %>%
  arrange(datetime) 

index = list()
week = array(NA, c(length(rank_14$datetime), 1))

for(i in 1:{length(W14)-1}){
  index[[i]] = rank_14$datetime %>% map_lgl(function(x) {x<W14[[i+1]] & x >=W14[[i]]})
  }

index = lapply(index, function(x) as.integer(x))
names(index) = c(paste0("week", 1:{length(W14)-1}))
index = as.data.frame(index) 
rank_14 = cbind(rank_14, index) %>%
  gather(., week, value, week1:week15) %>%
  select(-value) %>% 
  mutate(week = lapply(week, function(x) str_replace(x, "week", "")) %>% lapply(., function(x) as.integer(x)))


index = list()
week = array(NA, c(length(rank_16$datetime), 1))

for(i in 1:{length(W16)-1}){
  index[[i]] = rank_16$datetime %>% map_lgl(function(x) {x<W16[[i+1]] & x >=W16[[i]]})
}

index = lapply(index, function(x) as.integer(x))
names(index) = c(paste0("week", 1:{length(W16)-1}))
index = as.data.frame(index) 
rank_16 = cbind(rank_16, index) %>%
  gather(., week, value, week1:week15) %>%
  select(-value) %>% 
  mutate(week = lapply(week, function(x) str_replace(x, "week", "")) %>% lapply(., function(x) as.integer(x)))

######## filtering by relevant keyword

####################
##1. 정윤회########
scandal_2014 = c("정윤회", "문건", "유출", "문건유출", "조응천", "최경위", "박관천", "비선", "문고리","십상시")
index_2014 = rank_14$body %>% lapply(function(x) str_detect(x, scandal_2014) %>% any()) %>% unlist()

total14_1 = rank_14[index_2014,] 
totalrank14_1 = total14_1 %>% 
  mutate(week = unlist(week) %>% as.factor) %>%
  group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30},
         year = c(rep(2014.1, 15))) 

####################
## 2. 땅콩##########

scandal_2014_2 = c("땅콩", "회항", "조현아", "대한항공", "조양호")
index_2014_2 = rank_14$body %>% lapply(function(x) str_detect(x, scandal_2014_2) %>% any()) %>% unlist()

total14_2 = rank_14[index_2014_2,] 
totalrank14_2 = total14_2 %>% 
  mutate(week = unlist(week)) %>%
  group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30},
         year = c(rep(2014.2, 15))) 

###################
## 3. 최순실######

scandal_2016 = c("최순실", "정유라", "이대", "미르", "연설문", "K스포츠", "박근혜", "비선", "광화문", "국정농단")
index_2016 = rank_16$body %>% lapply(function(x) str_detect(x, scandal_2016) %>% any()) %>% unlist()

total16 = rank_16[index_2016,] 
totalrank16 = total16 %>% 
  mutate(week = unlist(week) %>% as.factor) %>%
  group_by(week) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30},
         year = c(rep(2016, 15))) 

totals = bind_rows(totalrank14_1, totalrank14_2, totalrank16) %>%
  mutate(year = map({.$year}, as.factor))

mostview = totals %>% group_by(week, year) %>% summarise(nrows = n()) %>%
  mutate(portion = {nrows/30})

## Visualization
mostview_14 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() + ylim(0,1) 

mostview_16 %>% ggplot(aes(x=week, y=portion)) + geom_line() +
  geom_point() 

mostview %>%
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x=week, y=portion, color = year)) + geom_line() + geom_point() 




########ranking keyword

top20_14 = t(weeklyKeyword_2014) %>% {.[-(1:2),]}
colnames(top20_14) <- 1:20
rownames(top20_14) = ymd("2014-11-28") + c(0:15)*week(1)


top20_16 = t(weeklyKeyword_2016) %>% {.[-(1:2),]}

colnames(top20_16) <- 1:20
rownames(top20_16) <- rownames(top20_14)