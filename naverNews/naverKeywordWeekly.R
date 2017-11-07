library(rvest)
library(dplyr)
library(N2H4)
library(lubridate)
### weekly search 

# Setting date
strDate = ISOdate(2016,09,23)
df_date = seq(strDate, by = "week", length.out = 16)
df_date = sapply(df_date, function(x) format(as.Date(x), "%Y%m%d"))
base_url = "http://news.naver.com/main/ranking/searchWeek.nhn?mid=etc&rankingType=search_week&date="  
keyword_list = data_frame(
  week = 1:20
)

strTime<-Sys.time()
midTime<-Sys.time()

# popular news head ####################################
for(i in 1:length(df_date)){
  print(paste0(i," / ",length(df_date), "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  
  url_news = paste0(base_url, df_date[i])
  news_data = read_html(url_news)   
  ranking_urls = html_nodes(news_data, ".ranking_keyword a") %>% html_text() %>% data_frame()
  colnames(ranking_urls) <- df_date[i]
  keyword_list = cbind(keyword_list, ranking_urls)
}



write.csv(keyword_list, file = "naver_weeklyKeyword_2016.csv")


