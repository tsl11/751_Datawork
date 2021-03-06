# Facebook media data cleaning

library(Rfacebook)
library(dplyr)
library(tibble)
library(purrr)
library(KoNLP)
library(lubridate)
library(stringr)
library(base64enc)
library(reshape2)
library(varhandle)in

useSejongDic()
buildDictionary(ext_dic = c('sejong', 'woorimalsam'),user_dic = data.frame("term"=c("이대", "최순실", "박근혜", "태블릿", "정유라"), "tag"=c("nqq", "nqpb", "nqpb", "f", "nqpb")))
buildDictionary(ext_dic = c('sejong', 'woorimalsam'),user_dic = data.frame("term"=c("미르재단", "K스포츠재단"), "tag"=c("nqq", "nqq")))
buildDictionary(ext_dic = c('sejong', 'woorimalsam'),user_dic = data.frame("term"=c("정윤회", "문건", "유출", "문건유출", "조응천", "최경위", "박관천", "비선", "문고리","십상시"), "tag"=c("nqpb","nqpb","nqpb","nqpb","nqpb","nqpb","nqpb","nqpb","nqpb","nqpb")))


## Cleaning each data frame
sisain = sisain %>% 
  mutate(activity_count = shares_count + likes_count + comments_count,
         X = 1:nrow(.)) %>%
  arrange(desc(activity_count)) %>%
  select(-c(X,from_id, id))

mediawatch = mediawatch %>% 
  mutate(activity_count = shares_count + likes_count + comments_count,
         X = 1:nrow(.)) %>%
  arrange(desc(activity_count)) %>%
  select(-c(X,from_id, id))

chosun = chosun %>% 
  mutate(activity_count = shares_count + likes_count + comments_count,
         X = 1:nrow(.)) %>%
  arrange(desc(activity_count)) %>%
  select(-c(X,from_id, id))

jtbc = jtbc %>% 
  mutate(activity_count = shares_count + likes_count + comments_count) %>%
  arrange(desc(activity_count)) %>%
  select(-c(from_id, id))

newstapa = newstapa %>% 
  mutate(activity_count = shares_count + likes_count + comments_count) %>%
  arrange(desc(activity_count)) %>%
  select(-c(X,from_id, id))

sbsnews = sbsnews%>% 
  mutate(activity_count = shares_count + likes_count + comments_count) %>%
  arrange(desc(activity_count)) %>%
  select(-c(X,from_id, id))

##Combining all into one df and changing Date format
fbmedia = list(donga, edaily, fnnews, hankukilbo, hankyoreh, hankyung, joongang, kbs, kukmin, kyunghyang, mbc, mbn, mediatoday, newsis, nocut, ohmynews, pressian, segye, yonhap, ytn) %>%
  lapply(., function(x) select(x, -c(X, from_id, id))) %>% map_df(function(x) x) 

fbmedia =  bind_rows(fbmedia, chosun, jtbc, newstapa, nocut, sbsnews) %>%
  mutate(created_time = as.Date(created_time)) %>%
  arrange(desc(created_time))

##### filtering fbmedia by keyword
setwd("/Users/tammylee/751_Datawork/facebook/csv_data")
fbmedia = read.csv("fbmedia_combined.csv")

fbmedia_2016 = fbmedia %>%
  mutate(created_time = unfactor(created_time) %>% as.Date()) %>%
  filter(created_time > as.Date("2016-09-19") & created_time < as.Date("2017-01-06"))

scandal_2016 = c("최순실", "정유라", "이대", "미르재단", "국정농단", "K스포츠재단", "박근혜", "비선","광화문")
index_2016 = fbmedia_2016$message %>% lapply(function(x) str_detect(x, scandal_2016) %>% any()) %>% unlist()

fbmedia_2016 = fbmedia_2016[index_2016, ] 


index_2016 = fbmedia_2016$message %>% lapply(function(x) str_detect(x, scandal_2016) %>% any()) %>% unlist()
fbmedia_2016 = fbmedia_2016[index_2016, ] 


fbsummary_2016 = fbmedia_2016 %>% 
  group_by(created_time) %>% 
  summarise(nrows = n(), shares = sum(shares_count), likes = sum(likes_count), 
            comments = sum(comments_count), totals = sum(activity_count)) %>%
  mutate(totals = likes + shares + comments) %>% arrange(totals) %>% na.omit()

fbkeyword_2016 = fbmedia_2016$message %>% sapply(function(x) na.omit(x) %>% extractNoun())
names(fbkeyword_2016) <- c(1:4846)  
fbkeyword_2016 = unlist(fbkeyword_2016) %>% as.data.frame() %>% map_df(str_replace_all("-",""))

##facebook 2014 data


fbmedia_2014 = fbmedia %>% 
  mutate(created_time = unfactor(created_time) %>% as.Date()) %>%
  filter(created_time > as.Date("2014-11-27") & created_time < as.Date("2015-03-06"))

scandal_2014 = c("정윤회", "문건", "유출", "문건유출", "조응천", "최경위", "박관천", "비선", "문고리","십상시")
index_2014 = fbmedia_2014$message %>% lapply(function(x) str_detect(x, scandal_2014) %>% any()) %>% unlist()

fbmedia_2014 = fbmedia_2014[index_2014, ] 

fbsummary_2014 = fbmedia_2014 %>% 
  group_by(created_time) %>% 
  summarise(nrows = n(), shares = sum(shares_count), likes = sum(likes_count), 
            comments = sum(comments_count), totals = sum(activity_count)) %>%
  mutate(totals = likes + shares + comments) %>% na.omit()

fbsummary = bind_rows(fbsummary_2014, fbsummary_2016)

save(fbsummary, file = "fbsummary.Rdata")



# visualize evolution in metric
library(ggplot2)
library(scales)
library(reshape2)
fbvis_2016 = fbsummary_2016 %>% select(c(created_time, shares, likes, comments, totals)) %>%
  melt(id.var="created_time") %>% group_by(variable) %>%
  mutate(cum_value = cumsum(value)) 

fbvis_2016 %>% ggplot(aes(x=created_time, y=cum_value, color=variable)) + geom_line(aes(alpha=0.8))

fbsummary_2016 %>% ggplot(aes(x=created_time, y=totals)) + geom_line() + scale_x_date(date_breaks = "1 day", date_labels = "%b %d", limits = c(as.Date("2016-09-20"), as.Date("2016-10-07")))


fbsummary_2016 %>% ggplot(aes(x=created_time, y=totals)) + geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

fbvis_2014 = fbsummary_2014 %>% select(c(created_time, shares, likes, comments, totals)) %>%
  melt(id.var="created_time") %>% group_by(variable) %>%
  mutate(cum_value = cumsum(value)) 

fbsummary_2014 %>% ggplot(aes(x=created_time, y=totals)) + geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")


#########11/06/2017########################################################################  
############################################################################################  
fb_keyword_2016 = fbmedia_2016$message %>% sapply(function(x) na.omit(x) %>% extractNoun())

#sapply(function(x) na.omit(x) %>% extractNoun, USE.NAMES = FALSE) %>% unlist() %>% map_df(function(x) x)


naverClick_2016 = naverClick_2016 %>% mutate(datetime = as.Date.POSIXct(datetime)) %>%
  arrange(desc(datetime))

format.facebook.date = function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

jtbc_2014 = jtbc %>%  
  map(
    
  )
filter(message %in% c("정윤회", "문건", "유출") & created_time < as.Date('2015-12-31'))

aggregate.metric = function(page, metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month),
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

page = jtbc
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes_count","shares_count","comments_count","activity_total"), aggregate.metric)
df <- do.call(rbind, df.list)

# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y"), limits = as.Date(c('2014-04-16','2016-10-28'))) + 
  scale_y_log10("Average count per post", breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())


############ newstapa data cleaning
newstapa_2014 = fbmedia_2014 %>% filter(from_name %in% c("Newstapa"))
newstapa_2016 = fbmedia_2016 %>% filter(from_name %in% c("Newstapa"))


newstapa_2014 = newstapa_2014 %>% 
  mutate(created_time = ymd(created_time)) %>%
  group_by(created_time) %>% 
  summarise(nrows = n(), shares = sum(shares_count), likes = sum(likes_count), 
            comments = sum(comments_count), totals = sum(activity_count)) %>%
  mutate(totals = likes + shares + comments) %>% arrange(created_time) %>% na.omit()

newstapa_2016 = newstapa_2016 %>% 
  group_by(created_time) %>% 
  summarise(nrows = n(), shares = sum(shares_count), likes = sum(likes_count), 
            comments = sum(comments_count), totals = sum(activity_count)) %>%
  mutate(totals = likes + shares + comments) %>% arrange(created_time) %>% na.omit()


W14 = ISOdate(2014,11,22) %>% seq(., by = "week", length.out = 16)
W16 = ISOdate(2016,09,23) %>% seq(., by = "week", length.out = 16) 

index = list()

for(i in 1:{length(W14)-1}){
  index[[i]] = newstapa_2014$created_time %>% map_lgl(function(x) {x<W14[[i+1]] & x >=W14[[i]]})
}

index = lapply(index, function(x) as.integer(x))
names(index) = c(paste0("week", 1:{length(W14)-1}))
index = as.data.frame(index) 
newstapa_2014 = cbind(newstapa_2014, index) %>%
  gather(., week, value, week1:week15) %>%
  select(-value) %>% 
  mutate(week = lapply(week, function(x) str_replace(x, "week", "")) %>% lapply(., function(x) as.integer(x)))


index = list()
week = array(NA, c(length(newstapa_2016$created_time), 1))

for(i in 1:{length(W16)-1}){
  index[[i]] = newstapa_2016$created_time %>% map_lgl(function(x) {x<W16[[i+1]] & x >=W16[[i]]})
}

index = lapply(index, function(x) as.integer(x))
names(index) = c(paste0("week", 1:{length(W16)-1}))
index = as.data.frame(index) 
newstapa_2016 = cbind(newstapa_2016, index) %>%
  gather(., week, value, week1:week15) %>%
  mutate(week = lapply(week, function(x) str_replace(x, "week", "")) %>% lapply(., function(x) as.integer(x)))

ntp2016 = newstapa_2016 %>%
  mutate(week = unlist(week)) %>%
  select(nrows, totals, week, value) %>%
  filter(value!=0) %>%
  select(-value) %>% 
  group_by(week) %>% 
  summarise(fbp_nwtp = sum(nrows), fbt_nwtp = sum(totals)) %>%
  arrange(week) %>% 
  mutate(fbp_nwtp_lag1 = lag(fbp_nwtp, order_by = week))

newstapa_2016 = newstapa_2016 %>%
  mutate(week = unlist(week)) %>%
  select(nrows, totals, week) %>%
  group_by(week) %>% 
  summarise(fbp_nwtp = sum(nrows), fbt_nwtp = sum(totals)) %>%
  arrange(week) %>% 
  mutate(fbp_nwtp_lag1 = lag(fbp_nwtp, order_by = week))

