library(rvest)
library(dplyr)
library(N2H4)
library(lubridate)
### weekly click 

# Setting date
strDate = ISOdate(2016,09,23)
df_date = seq(strDate, by = "week", length.out = 16)
df_date = sapply(df_date, function(x) format(as.Date(x), "%Y%m%d"))
base_url = "http://news.naver.com/main/ranking/popularWeek.nhn?rankingType=popular_week&sectionId=000&date="  
url_list = list()

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
  url_list[[i]] = ranking_urls
}

cont = url_list %>% unlist(., recursive = FALSE) %>%
  lapply(., getContent)

ranking_news =  data_frame(
  url = map(cont, "url"),
  datetime = map(cont, "datetime") %>% map_if(is.character, function(x) x=NA),
  press = map(cont, "press"),
  title = map(cont, "title"),
  body = map(cont, "body")
)

ranking_news = sapply(ranking_news, unlist)
write.csv(ranking_news, file = "naver_weeklyClick000_2016.csv")


