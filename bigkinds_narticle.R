### Big Kinds narticle 

library(tidyr)
library(purrr)
library(lubridate)
library(dplyr)

big14 = read.csv("narticle_bigkinds2014.csv")
big16 = read.csv("narticle_bigkinds2016.csv")

big = bind_rows(big14,big16) %>% select(c(기간, total)) 
colnames(big) = c("date", "narticle")
big = big %>% mutate(date = ymd(date))

n14 = big %>% filter(date > ymd(20141127) & date < ymd(20150305))
n16 = big %>% filter(date > ymd(20160919) & date < ymd(20170106))

narticle = bind_rows(n14, n16)

data_big = full_join(data, narticle)

data_big %>% mutate(n_total = totals/50) %>%
  gather(cat, value = val, narticle, n_total) %>% ggplot(aes(x=date, y=val, color = cat))+
  geom_line() + scale_x_date(limits = c(ymd(20160920), ymd(20170105))) + scale_y_continuous(limits = c(0, 1000))

               
               
               
               