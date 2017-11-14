### Regressions

data14 = data %>% filter(date < ymd("2016-01-01")) %>%
  mutate(word_rank = word_rank*100,
         totals = log(totals),
         n_article = log(n_article),
         nrows = nrows/10)

zero_index = which(data14$totals == 0)
data14$date[zero_index]
data14$totals[which(data14$totals==-Inf)] = 0
data14$n_article[1:length(data14)-1] = data14$n_article[2:length(data14)] 
data14 = data14[-nrow(data14),]

data16 = data %>% filter(date > ymd("2016-01-01")) %>%
  mutate(word_rank = word_rank*100)

zero_index16 = which(data16$totals == 0)

data16$n_article[1:length(data16)-1] = data16$n_article[2:length(data16)] 
data16 = data14[-nrow(data16),]

OLS14_totals = lm(n_article ~ nrows + totals + word_rank , data14)

tOLS14_totals_wr = lm(n_article ~ nrows + totals , data14)
tOLS14_shares = lm(n_article ~ nrows + shares + word_rank, data14)

tOLS16_totals = lm(n_article ~ nrows + totals + word_rank, data16)

## fbshares = narticles + nrows + word rank
OLS14 = lm(totals ~ n_article + nrows + word_rank, data14)
OLS16 = lm(totals ~ n_article + nrows + word_rank, data16)

##### visualization
library(ggplot2)
library(scales)
data14 %>% mutate(log_n = log(n_article),
                  log_total = log(totals)) %>%
  ggplot(aes(x=log_n, y = log_total)) + geom_point() +geom_abline(slope = 1)
data14 %>% gather(cat, value = val, n_article, totals) %>%
  ggplot(aes(x = date, y = val, color = cat)) + geom_line() +
  scale_x_date(limits = c(2014-11-28, 2015-01-15))

vis14 = data14 %>% gather(cat, value = val, n_article, totals, word_rank, nrows) 
vis14 %>% ggplot(aes(x = date, y = val, color = cat )) + geom_line()



