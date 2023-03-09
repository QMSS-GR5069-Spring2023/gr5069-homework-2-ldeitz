## ---------------------------
##
## Script name: visualizations
##
## Purpose of script: To output all necessary visualizations for the project
##
## Author: Lauren Deitz ; Sarang Patel
##
## Date Created: 2023-03-08
##
## ---------------------------
##
## Notes:
##
## ---------------------------

## Total number of babies given the top 10 most popular names in 2017

babynames %>%
  filter(name %in% names2017$name) %>%
  group_by(year,name) %>%
  mutate(count=sum(n)) %>%
  ggplot(.,aes(x=year, y=count, color=name, group=name)) +
  geom_line()
## ---------------------------

## Total number of babies given the top 15 unisex nanmes over time

babynames %>%
  filter(name %in% top15unisex$name) %>%
  group_by(year, name) %>%
  mutate(count=sum(n)) %>%
  ggplot(.,aes(x=year, y=count, group=name, color=name)) +
  geom_line()
## ---------------------------

## total number of babies with a "city name" - Brooklyn, Madison, Savannah, Austin

babynames %>%
  filter(name=="Brooklyn" | name=="Madison" | name=="Savannah" | name=="Austin") %>%
  group_by(year, name) %>%
  mutate(total=sum(n)) %>%
  ggplot(.,aes(x=year, y=total, group=name, color=name)) +
  geom_line()