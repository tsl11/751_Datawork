## Data combining

library(dplyr)

narticle = narticle %>% filter(keyword != "정유라")
fbsummary = fbsummary %>% mutate(date = created_time) %>% select(-created_time)
data = full_join(narticle, fbsummary, by = c("date")) %>%
  filter(!is.na(date))

fbn.na = which(is.na(data$nrows))
fbshares.na = which(is.na(data$shares))
fblikes.na = which(is.na(data$likes))
fbcomments.na = which(is.na(data$comments))
fbtotals.na = which(is.na(data$totals))

data[fbn.na, which(names(data) %in% c("nrows"))] = 0
data[fbshares.na, which(names(data) %in% c("shares"))] = 0
data[fblikes.na, which(names(data) %in% c("likes"))] = 0
data[fbcomments.na, which(names(data) %in% c("comments"))] = 0
data[fbtotals.na, which(names(data) %in% c("totals"))] = 0

daily_wrank = wrank %>% slice(rep(1:n(), each = 7))
date14 = sapply(seq(ISOdate(2014,11,22), by = "day", length.out = 16*7), function(x) format(as.Date(x), "%Y%m%d"))
date16 = sapply(seq(ISOdate(2016,09,17), by = "day", length.out = 16*7), function(x) format(as.Date(x), "%Y%m%d"))

daily_wrank$date = c(date14,date16) %>% ymd()
data = full_join(data, daily_wrank, by = c("date"))
