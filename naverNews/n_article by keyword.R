library(rvest)
library(purrr)
library(dplyr)
devtools::install_github("forkonlp/N2H4")
library(N2H4)
library(stringr)
library(KoNLP)
library(lubridate)
library(varhandle)

strDate = c("2016-09-20")
endDate = c("2016-12-28")

strDateo = gsub("-",".",strDate)
strDated = gsub("-","",strDate)
endDateo = gsub("-",".",endDate)
endDated = gsub("-","",endDate)

strDate = ISOdate(2014,11,28)
df_date = seq(strDate, by = "day", length.out = 120)
df_date = sapply(df_date, function(x) format(as.Date(x), "%Y%m%d"))


strTime<-Sys.time()
midTime<-Sys.time()

qlist<-c("정윤회", "조현아")
prod14 = matrix(data = NA, nrow = 240, ncol = 3)

for (i in 1:length(qlist)){
  for(j in 1:length(df_date)){
    
    dateo = ymd(df_date[j]) %>% gsub("-",".", .)
    dated = df_date[j]
    
  print(paste0(qlist[i],df_date[j],"/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  pageUrli = paste0("https://search.naver.com/search.naver?where=news&query=",qlist[i],"&ie=utf8&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds=",dateo,"&de=",dateo,"&docid=&nso=so%3Ar%2Cp%3Afrom",dated,"to",dated,"%2Ca%3Aall&mynews=0&mson=0&refresh_start=0&related=0")  
  
  max = read_html(pageUrli) %>% 
    html_node(".all_my span") %>%
    html_text() %>%  str_extract("[^/]*$") %>% str_replace("건","") 
  
  prod14[j+{i-1}*120,1] = df_date[j]
  prod14[j+{i-1}*120,2] = max
  prod14[j+{i-1}*120,3] = qlist[i]
    }
}

prod = rbind(prod14, prod16)
colnames(prod) <- c("date", "n_article", "keyword")
prod = as.data.frame(prod) %>%
  mutate(date = unfactor(date) %>% ymd(),
         n_article = map(n_article, function(x) gsub(",", "", x)) %>% unlist() %>% as.double())

prod1 = bind_rows(slice(prod,1:120), slice(prod,121:240))
prod2 = bind_rows(slice(prod,241:360), slice(prod,361:480))

prod1 = mutate(prod1, date = c(rep(paste0("day", 1:120), 2)))
prod2 = mutate(prod2, date = c(rep(paste0("day", 1:120), 2)))

prod1[which(is.na(prod1$n_article)), which(colnames(prod1)=="n_article")] = 0
prod2[which(is.na(prod2$n_article)), which(colnames(prod2)=="n_article")] = 0
 

#### visualization
library(ggplot2)
library(scales)
library(tidyr)

vis_pr = bind_rows(prod1,prod2)
vis_1 = prod1 %>% {.[which(is.na(prod1$n_article)), which(colnames(.)=="n_article")]} = 0 %>%
  mutate(keyword = unfactor(keyword)) %>%
  group_by(keyword) %>% 
  mutate(cum_narticle = cumsum(n_article))

vis_1 %>%  ggplot(aes(x = date, y = cum_narticle)) + geom_line(aes(color = keyword)) + 
  scale_x_discrete(breaks = seq(from = 1, to = 120, by = 10), labels = c(paste0("day",seq(from = 1, to = 120, by = 10) ))) + 
  theme_bw() + theme(axis.title.x = element_blank())

