library(rvest)
library(dplyr)
library(purrr)
library(N2H4)
library(lubridate)
### weekly click 

# Setting date
strDate = ISOdate(2014,11,28)
df_date = seq(strDate, by = "day", length.out = 120)
df_date = sapply(df_date, function(x) format(as.Date(x), "%Y%m%d"))
base_url = "http://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=000&date="  
url_list2 = list()

strTime<-Sys.time()
midTime<-Sys.time()

# popular news head ####################################
for(i in 1:length(df_date)){
  print(paste0(i," / ",length(df_date), "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  
  url_news = paste0(base_url, df_date[i])
  news_data = read_html(url_news)   
  ranking_urls = html_nodes(news_data, "dt")
  ranking_urls = ranking_urls %>% html_nodes("a") %>% html_attr("href") %>% 
    map(function(x) paste0("http://news.naver.com",x))
  url_list2[[i]] = ranking_urls
}

cont = url_list %>% unlist(., recursive = FALSE) %>%
  lapply(., getContent)

dailyrank16 =  data_frame(
  url = map(cont, "url"),
  datetime = map(cont, "datetime") %>% map_if(is.character, function(x) x=NA),
  press = map(cont, "press"),
  title = map(cont, "title"),
  body = map(cont, "body")
)

dailyrank16 = sapply(dailyrank16, unlist)
write.csv(dailyrank16, file = "naver_dailyClick_2016.csv")


