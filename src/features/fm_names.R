allfemalenames <- babynames %>%
  filter(sex=="F") %>%
  group_by(name) %>%
  summarize(count=sum(n))

allmalenames <- babynames %>%
  filter(sex=="M") %>%
  group_by(name) %>%
  summarize(count=sum(n))


allnamesmerge <- merge(allfemalenames, allmalenames, by="name")

allnamesmerge <- allnamesmerge %>%
  mutate(diff=count.x - count.y) %>%
  mutate(sum=count.x + count.y) %>%
  mutate(unisex = sum-abs(diff)) %>%
  arrange(desc(unisex)) %>%
  mutate(rank=row_number())