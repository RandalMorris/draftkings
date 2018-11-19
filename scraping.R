library(tidyverse)
library(lubridate)
library(rvest)

WEEKS <- 1:18
YEARS <- 2013:2018
all_games <- NULL

for (yr in YEARS) {
  for (wk in WEEKS) {
    
    url <- paste0("http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=", wk, "&year=", yr)
    
    weekly <-
      read_html(url) %>%
      html_nodes("pre") %>%
      html_text() %>%
      read_delim(delim = ";")
    
    all_games <- rbind(all_games, weekly)
  }
}

with(all_games, table(Week, Year))
with(distinct(all_games, Week, Year, Team), table(Week, Year))

ggplot(data = all_games %>% filter(complete.cases(.), `DK salary` > 0)) +
  geom_point(aes(x = `DK salary`, y = `DK points`, color = as.factor(Week))) +
  facet_wrap(facets = ~Year, nrow = 2)

write_csv(all_games, "./data/all_games.csv")
