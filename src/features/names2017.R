names2017 <- babynames %>%
  filter(year==2017) %>%
  group_by(name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10)