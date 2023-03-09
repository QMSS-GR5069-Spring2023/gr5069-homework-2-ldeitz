## ---------------------------
##
## Script name: munged_data.R
##
## Purpose of script: All other commands to munge/further filter the raw data
##
## Author: Lauren Deitz ; Sarang Patel
##
## Date Created: 2023-03-08
##
## ---------------------------
##
## Notes: 
##   Includes several commands to get counts & differences between data including filters
##
## ---------------------------

# Get the top 10 names in 2017 and their popularity over time
names2017 <- babynames %>%
  filter(year==2017) %>%
  group_by(name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10)

# Get counts for all female names
allfemalenames <- babynames %>%
  filter(sex=="F") %>%
  group_by(name) %>%
  summarize(count=sum(n))

# Get counts for all male names
allmalenames <- babynames %>%
  filter(sex=="M") %>%
  group_by(name) %>%
  summarize(count=sum(n))

# Merge female and male name counts by name
allnamesmerge <- merge(allfemalenames, allmalenames, by="name")

# Calculate the count difference and total count for each name
allnamesmerge <- allnamesmerge %>%
  mutate(diff=count.x - count.y) %>% # take count difference
  mutate(sum=count.x + count.y) %>% # get total count
  mutate(unisex = sum-abs(diff)) %>% # calculate unisex total
  arrange(desc(unisex)) %>% # sort by unisex totals descending
  mutate(rank=row_number()) # rank

# Get the top 15 unisex names and their popularity over time
top15unisex = filter(allnamesmerge, rank<=15)

# Get the total count for the names Brooklyn, Madison, Savannah, and Austin over time
names_to_plot <- babynames %>%
  filter(name=="Brooklyn" | name=="Madison" | name=="Savannah" | name=="Austin") %>%
  group_by(year, name) %>%
  mutate(total=sum(n))