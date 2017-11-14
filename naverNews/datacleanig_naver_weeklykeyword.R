library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)

##narticle produced daily per keyword
narticle = prod %>% filter(keyword != "조현아")

rank = function(dataframe, x){
  loc = which(dataframe == x, arr.ind = TRUE) %>% {.[[1,1]]}
  value = x*{20-loc}
  return(value)
}

wrank14 = weeklyKeyword_2014 %>% map_df(function(x){
  str_detect(x, "정윤회") %>% as.integer()}) 

loc14 = which(wrank14 == 1, arr.ind = TRUE)
wrank14[loc14] = 2/30

key16 = c("최순실", "박근혜", "탄핵", "시국선언", "정유라", "이화여대", "jtbc","뉴스룸", "대국민담화", "안종범", 
          "최순득", "장시호", "우병우", "차은택", "길라임", "비아그라", "고영태", "김기춘", "가결", "이재용", 
          "노승일", "조여옥", "청문회", "이슬비", "조윤선", "특검")

wrank16 = weeklyKeyword_2016 %>% map_df(function(x){
  str_detect(x, paste(key16,collapse = '|')) %>% as.integer()})

loc16 = which(wrank16 == 1, arr.ind = TRUE)
for(i in 1:nrow(loc16)){
  r = loc16[[i,1]]
  c = loc16[[i,2]]
  if(r < 11){
    wrank16[r,c] = 2/30
  } else {
    wrank16[r,c] = 1/30
  }
}

date14 = sapply(seq(ISOdate(2014,11,28), by = "week", length.out = 16), function(x) format(as.Date(x), "%Y%m%d"))
date16 = sapply(seq(ISOdate(2016,09,23), by = "week", length.out = 16), function(x) format(as.Date(x), "%Y%m%d"))

wrank = data_frame(
  date = c(date14, date16),
  word_rank = c(colSums(wrank14)[-(1:2)], colSums(wrank16)[-(1:2)])
)

save(wrank, file = "word_ranking.RData")
