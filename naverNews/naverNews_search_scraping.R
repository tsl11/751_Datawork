library(rvest)
library(dplyr)
library(N2H4)
library(lubridate)

# Setting date
Date = ymd("2015-03-28") - days(0:120)   # 2017-04-12로부터 1년 동안의 날짜를 모두 나타냅니다.
df_2016 = data.frame(Date) 
df_date = format(df_2016$Date,"%Y%m%d") 

base_url = "http://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date="  
url_list = list()

strTime<-Sys.time()
midTime<-Sys.time()

# popular news head ####################################
for(i in 1:length(df_date)){
  print(paste0(i," / ",length(df_date), "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  
  url_news = paste0(base_url, df_date[i])
  news_data = read_html(url_news)   
  ranking_urls = html_nodes(news_data, "dt") %>% html_nodes("a") %>% html_attr("href") %>% 
    data_frame() %>% map_df(function(x) paste0("http://news.naver.com",x))
  url_list[[i]] = ranking_urls
}

url_list = map_df(url_list, function(x) x) 
colnames(url_list) <- c("urls")

cont = lapply(url_list$urls, getContent)

ranking_news =  data_frame(
  url = map(cont, "url"),
  datetime = map(cont, "datetime") %>% map_if(is.character, function(x) x=NA),
  press = map(cont, "press"),
  title = map(cont, "title"),
  body = map(cont, "body")
)

write.csv(ranking_news_unlit, file = "ranking_news_2014.csv")
