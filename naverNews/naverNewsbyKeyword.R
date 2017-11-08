library(rvest)
library(purrr)
library(N2H4)
library(stringr)

strDate = c("2016-09-20")
endDate = c("2016-12-28")

strDateo = gsub("-",".",strDate)
strDated = gsub("-","",strDate)
endDateo = gsub("-",".",endDate)
endDated = gsub("-","",endDate)

strTime<-Sys.time()
midTime<-Sys.time()

qlist<-c("최순실","정유라")
newsList = list()

for (i in 1:length(qlist)){
  
  print(paste0(qlist[i], "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  pageUrli = paste0("https://search.naver.com/search.naver?where=news&query=",qlist[i],"&ie=utf8&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds=",strDateo,"&de=",endDateo,"&docid=&nso=so%3Ar%2Cp%3Afrom",strDated,"to",endDated,"%2Ca%3Aall&mynews=0&mson=0&refresh_start=0&related=0")  
  
  maxArticle = read_html(pageUrli) %>% 
    html_node("#main_pack > div.news.mynews.section > div > div.title_desc.all_my > span") %>%
    html_text() %>% str_extract(.,"\\d{3,}\\,\\d{3,}") %>% str_replace(",","") %>% as.integer()
  
  for (i in 1:{maxArticle %/% 10}){
    start = (i-1)*10 + 1
    print(paste0(i," / ",{maxArticle %/% 10}, "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
    midTime<-Sys.time()
    
    pageUrl = paste0(pageUrli,"&start=",start)
    newsList[[i]] = getUrlListByQuery(pageUrl)
  }
}


## qlist[1] 3471까지 돌렸음 (11/8/2017)

page_content = map_df(newsList, function(x) x) 
page_content = lapply(page_content$news_links, function(x) getContent(x))

page_df = data_frame(
  url = map(page_content, "url"),
  datetime = map(page_content, "datetime") %>% map_if(is.character, function(x) x=NA),
  press = map(page_content, "press"),
  title = map(page_content, "title"),
  body = map(page_content, "body")
)

