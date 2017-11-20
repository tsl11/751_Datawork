### Big Kinds narticle 

library(tidyr)
library(purrr)
library(lubridate)
library(dplyr)
library(ggplot2)

big14 = read.csv("narticle_bigkinds2014.csv")
big16 = read.csv("narticle_bigkinds2016.csv")

big = bind_rows(big14,big16) %>% select(c(기간, total)) 
colnames(big) = c("date", "narticle")
big = big %>% mutate(date = ymd(date))

n14 = big %>% filter(date > ymd(20141127) & date < ymd(20150305))
n16 = big %>% filter(date > ymd(20160919) & date < ymd(20170106))

narticle = bind_rows(n14, n16)

data_big = full_join(data, narticle)
data_big[is.na(data_big)] = 0

mutate(n_total = totals/50) %>%

data_big %>% mutate(n_total = totals/10) %>%
  gather(cat, value = val, narticle, n_total) %>% ggplot(aes(x=date, y=val, color = cat))+
  geom_line() + scale_x_date(limits = c(ymd(20141128), ymd(20150304))) + 
  labs(x = "Date", y = "Value", color="Variable", title = "2014 Daily trends of facebook and articles") +
  scale_color_hue(labels = c("facebook shares", "# of articles")) + scale_y_continuous(limits = c(0, 750))

d14 = sapply(seq(ISOdate(2014,11,28), by = "week", length.out = 16), function(x) format(as.Date(x), "%Y%m%d"))
d16 = sapply(seq(ISOdate(2016,09,23), by = "week", length.out = 16), function(x) format(as.Date(x), "%Y%m%d"))               

week = list()
week[[1]] = data_big %>% select(c(date, narticle, totals, shares, likes, comments, nrows)) %>%
  filter(date < ymd(d14[2])) %>% select(-date) %>% colSums()

for(i in 2:16){
  
  week[[i]] = data_big %>% select(c(date, narticle, totals, shares, likes, comments, nrows)) %>%
    filter(date < ymd(d14[i+1]) & date >= ymd(d14[i])) %>% select(-date) %>% colSums()
}

week2 = list()
week2[[1]] = data_big %>% select(c(date, narticle, totals, shares, likes, comments, nrows)) %>%
  filter(date < ymd(d16[2])) %>% select(-date) %>% colSums()

for(i in 2:16){
  
  week2[[i]] = data_big %>% select(c(date, narticle, totals, shares, likes, comments, nrows)) %>%
    filter(date < ymd(d16[i+1]) & date >= ymd(d16[i])) %>% select(-date) %>% colSums()
}

narticle = map(week, "narticle")
fbshares = map(week, "shares")
fblikes = map(week, "likes")
fbcomments = map(week, "comments")
fbpostings =  map(week, "nrows") 
fbtotals =  map(week, "totals") 

week_df = data_frame(
 year = c(rep(2014, 16)),
 week = c(1:16),
 narticle = narticle,
 fbshares = fbshares,
 fblikes = fblikes,
 fbcomments = fbcomments,
 fbpostings = fbpostings,
 fbtotals = fbtotals
)

narticle = map(week2, "narticle")
fbshares = map(week2, "shares")
fblikes = map(week2, "likes")
fbcomments = map(week2,"comments")
fbpostings = map(week2, "nrows")
fbtotals = map(week2, "totals") 

week2 = data_frame(
  year = c(rep(2016, 16)),
  week = c(1:16),
  narticle = narticle,
  fbshares = fbshares,
  fblikes = fblikes,
  fbcomments = fbcomments,
  fbpostings = fbpostings,
  fbtotals = fbtotals
)

weekly = bind_rows(week_df, week2)
wrank = wrank %>% mutate(week = c(rep(1:16, 2)),
                         year = c(rep(c(2014,2016), each = 16)))

weekly = full_join(weekly, wrank, by = c("week", "year")) %>% full_join(., mostview, by = c("week", "year"))
weekly = weekly %>% mutate(rank_portion = (rank.art.portion*30)/50) %>%
  select(-rank.art.portion)

library(latticeExtra)
weekly14 = weekly %>% filter(year==2014)
weekly16 = weekly %>% filter(year==2016)
############2014################################################
## narticle ~ rank_portion
obj1 <- xyplot(narticle ~ week, weekly14, type = "l" , lwd=2)
obj2 <- xyplot(rank_portion ~ week, weekly14, type = "l", lwd=2, scales = list(y = list(at=c(0.2,0.4,0.6, 0.8, 1.0), labels=c(0.2,0.4,0.6, 0.8, 1.0))))
doubleYScale(obj1, obj2, text = c("# of articles", "portion of top ranked articles") , add.ylab2 = TRUE)

## fbtotals ~ rank_portion
obj3 <- xyplot(fbtotals ~ week, weekly14, type = "l" , lwd=2)
obj4 <- xyplot(rank_portion ~ week, weekly14, type = "l", lwd=2, scales = list(y = list(at=c(0.2,0.4,0.6, 0.8, 1.0), labels=c(0.2,0.4,0.6, 0.8, 1.0))))
doubleYScale(obj3, obj4, text = c("facebook totals", "portion of top ranked articles") , add.ylab2 = TRUE)

## narticle ~ fbtotals
obj5 <- xyplot(narticle ~ week, weekly14, type = "l" , lwd=2)
obj6 <- xyplot(fbtotals ~ week, weekly14, type = "l", lwd=2)
doubleYScale(obj5, obj6, text = c("# of articles", "facebook totals") , add.ylab2 = TRUE)


############2016################################################
## narticle ~ rank_portion
obj11 <- xyplot(narticle ~ week, weekly16, type = "l" , lwd=2)
obj12 <- xyplot(rank_portion ~ week, weekly16, type = "l", lwd=2, scales = list(y = list(at=c(0.2,0.4,0.6, 0.8, 1.0), labels=c(0.2,0.4,0.6, 0.8, 1.0))))
doubleYScale(obj11, obj12, text = c("# of articles", "portion of top ranked articles") , add.ylab2 = TRUE)

## fbtotals ~ rank_portion
obj13 <- xyplot(fbtotals ~ week, weekly16, type = "l" , lwd=2)
obj14 <- xyplot(rank_portion ~ week, weekly16, type = "l", lwd=2, scales = list(y = list(at=c(0.2,0.4,0.6, 0.8, 1.0), labels=c(0.2,0.4,0.6, 0.8, 1.0))))
doubleYScale(obj13, obj14, text = c("facebook totals", "portion of top ranked articles") , add.ylab2 = TRUE)

## narticle ~ fbtotals
obj15 <- xyplot(narticle ~ week, weekly16, type = "l" , lwd=2)
obj16 <- xyplot(fbtotals ~ week, weekly16, type = "l", lwd=2)
doubleYScale(obj15, obj16, text = c("# of articles", "facebook totals") , add.ylab2 = TRUE)



