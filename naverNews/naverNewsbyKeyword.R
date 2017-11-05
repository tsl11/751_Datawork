library(rvest)
library(purrr)
library(N2H4)
library(stringr)

strDate = c("2014-11-28")
endDate = c("2014-11-28")

strTime<-Sys.time()
midTime<-Sys.time()

qlist<-c("정윤회","문건")
newsList = list()

for (i in 1:length(qlist)){
  dir.create("./data",showWarnings=F)
  dir.create(paste0("./data/news_",qlist[i]),showWarnings=F)
  
  strDateo = gsub("-",".",strDate)
  strDated = gsub("-","",strDate)
  endDateo = gsub("-",".",endDate)
  endDated = gsub("-","",endDate)
  
  print(paste0(date," / ",qlist[i], "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  pageUrli = paste0("https://search.naver.com/search.naver?where=news&query=",qlist[i],"&ie=utf8&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds=",strDateo,"&de=",endDateo,"&docid=&nso=so%3Ar%2Cp%3Afrom",strDated,"to",endDated,"%2Ca%3Aall&mynews=0&mson=0&refresh_start=0&related=0")  
  
  maxArticle = read_html(pageUrli) %>% 
    html_node("#main_pack > div.news.mynews.section > div > div.title_desc.all_my > span") %>%
    html_text() %>% str_extract(.,"\\d{3,}") %>% as.integer()
  
  for (i in 1:{maxArticle %/% 10}){
    start = (i-1)*10 + 1
    pageUrl = paste0(pageUrli,"&start=",start)
    newsList[[i]] = getUrlListByQuery(pageUrl)
  }
}

page_content = map_df(newsList, function(x) x) 
page_content = lapply(page_content$news_links, function(x) getContent(x))

page_df = data_frame(
  url = map(page_content, "url"),
  datetime = map(page_content, "datetime") %>% map_if(is.character, function(x) x=NA),
  press = map(page_content, "press"),
  title = map(page_content, "title"),
  body = map(page_content, "body")
)

