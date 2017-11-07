library(Rfacebook)
library(dplyr)
library(tibble)
library(purrr)
library(KoNLP)

useSejongDic()

options(scipen = 999)
token <- "EAACEdEose0cBAP5j1qry7qu4Ti2XZAeuNJjUkbstZBRbffLuLZCmONxZCjwuBpFNxvZBbOOnuVVRWQa5UPZBrYV5nZCFd7NZCWhJIOUZBCbjlC9t3opC2vZBcaiC6yjKp6cOxQ9z75CtjAZAQvH4eG76fBrfR7q3E2IIchjNkEHofSFFY8XnJzTM9BPElOXHQwaKj8ZD"


yonhap<- getPage("yonhap", token, n = 5000, since='2014/11/28', until='2017/1/30')

yonhap = yonhap %>% 
  mutate(activity_count = shares_count + likes_count + comments_count) %>%
  arrange(desc(activity_count)) 

write.csv(yonhap, "yonhap_fb.csv")

