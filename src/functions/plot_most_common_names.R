# Function to plot the most common names in a given year over the entire period
# Args:
#   year: the year to filter the data on
#   num_names: the number of top names to include in the plot
# Returns:
#   a ggplot object

plot_most_common_names <- function(year, num_names) {
  
  # Filter data for the given year and group by name, summarizing total count
  names_year <- babynames %>%
    filter(year == year) %>%
    group_by(name) %>%
    summarize(total = sum(n))
  
  # Sort by total count and add rank
  names_year_ranked <- names_year %>%
    arrange(desc(total)) %>%
    mutate(rank = row_number())
  
  # Filter for top num_names
  names_top <- names_year_ranked %>%
    filter(rank <= num_names) %>%
    select(name)
  
  # Filter data for top names and group by year and name, summarizing count
  babynames_top <- babynames %>%
    filter(name %in% names_top$name) %>%
    group_by(year, name) %>%
    summarize(count = sum(n))
  
  # Plot with ggplot
  ggplot(babynames_top, aes(x = year, y = count, color = name, group = name)) +
    geom_line()
}

# Example usage:
# plot_most_common_names(2017, 10)