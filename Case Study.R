library(tibble)
library(dplyr)
library(ggplot2)
library(readr)
library(pheatmap)
library(tidyr)
library(purrr)


# load data
all_comps = read_csv("all_comps.csv")
skiresults = read_csv("skiresults.csv")

# subset with only the columns requested in the task
all_comps_compact = all_comps[c("id", "season", "gender", "hill_size_x")]

# adding type column
all_comps$type = ""
all_comps$type[all_comps$hill_size_x < 50] = "Small Hill"
all_comps$type[all_comps$hill_size_x %in%  c(50:84)] = "Medium Hill"
all_comps$type[all_comps$hill_size_x %in%  c(85:109)] = "Normal Hill"
all_comps$type[all_comps$hill_size_x %in%  c(110:189)] = "Large Hill"
all_comps$type[all_comps$hill_size_x > 189] = "Ski Flying Hill"

# merge datasets
merged_data = merge(all_comps, skiresults, by = "id")
merged_data_numeric = select_if(merged_data, is.numeric)
summary(merged_data)

# calculate correlations
correlation = round(cor(
  select_if(merged_data, is.numeric),
  use = "pairwise.complete.obs",
  method = "spearman"
),
2)
correlation = as.data.frame(correlation)
correlation = correlation[-c(8), -c(8)] # removed this rows and colums because there was only NA in it

# produce a heatmap for the correlation
pheatmap(
  correlation,
  display_numbers = TRUE,
  main = "correlation heatmap",
  treeheight_row = 0,
  treeheight_col = 0
)

# produce distributions per variable
for (name in names(select_if(merged_data, is.numeric))) {
  data = as.matrix(select_if(merged_data, is.numeric)[name])
  par(mfrow = c(1, 3))
  plot.ecdf(data, main = paste(name, "ecdf", " "))
  boxplot(data, main = paste(name, "boxplot", " "))
  hist(data, main = paste(name, "histogram", " "))
}


# define function to draw boxplots by groups
draw_grouped_boxplot = function(data, group_by_column, data_column) {
  # define aesthetics valid for the whole diagram
  aesthetics = aes(
    x = get(group_by_column),
    y = get(data_column),
    group = get(group_by_column)
  )
  
  # init plot
  plot = ggplot(data = data, aesthetics)
  
  # boxplot, without outliers and whiskers
  boxplot = geom_boxplot(outlier.shape = NA, coef = 0)
  
  # calculate quantiles and setup coords
  grouped_by_quantiles = data %>%
    group_by_at(group_by_column) %>%
    summarise(
      q25 = quantile(get(data_column), probs = .25, na.rm = TRUE),
      q75 = quantile(get(data_column), probs = .75, na.rm = TRUE)
    )
  
  # calculate means per group
  means = data %>%
    group_by_at(group_by_column) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup()
  
  # set coordinates to make sure the diagramms fit to the space
  coords = coord_cartesian(ylim = c(
    min(grouped_by_quantiles$q25),
    max(grouped_by_quantiles$q75)
  ))
  
  # draw means as points
  points = geom_point(data = means, aesthetics, group = group_by_column)
  
  # add some labels
  labels = labs(
    title = paste(data_column, "by", group_by_column, sep = " "),
    subtitle = "median, upper and lower quartile, mean",
    x = group_by_column,
    y = data_column
  )
  
  # construct and return plot
  plot =  plot + boxplot + coords + points + labels
  return(plot)
}

# define for which groups the boxplots should be drawn
interesting_groups = c("round")
# iterate over groups and numeric values
for (group in interesting_groups) {
  for (name in names(select_if(merged_data, is.numeric))) {
    print(name)
    print(draw_grouped_boxplot(merged_data, group, name))
  }
}

# -- Task --
# a) Reduce the data such that it only contains information about the final rounds on a ski flying hill.
# b) Add a variable n_final_rounds to the data set, which provides per pair of athlete and season
#    the number of final rounds (on a ski flying hill) that he/she participated in.
# c) For each athlete, who participated in at least 5 final rounds on a ski flying hill,
#    compute the median amount of note_points across all competitions per season.
# d) Based on the previously computed aggregated note points, produce a tibble that provides the
#    names of the best and worst athlete(s) per season.

# Task a)
final_ski_flying = merged_data %>%
  filter(round == "final round") %>%
  filter(type == "Ski Flying Hill")
final_ski_flying

# Task b)
participated_final_rounds = final_ski_flying %>% group_by(name, season) %>%
  mutate(n_final_rounds = n()) %>% # added variable for rounds per season
  ungroup()
participated_final_rounds

# Task c)
median_note_points = participated_final_rounds %>% group_by(name) %>%
  mutate(n_final_rounds_overall = sum(n_final_rounds)) %>% # this sum is independent of the season
  ungroup() %>%
  filter(n_final_rounds_overall >= 5) %>%
  group_by(name, season) %>%
  mutate(median_note_points = median(note_points)) %>%
  ungroup() %>%
  distinct(name, season, median_note_points)
median_note_points

# Task d)
best_and_worst = median_note_points %>%
  group_by(season) %>%
  mutate(rank = rank(median_note_points, ties.method = "min")) %>%
  filter(rank == 1 | rank == max(rank)) %>%
  mutate(rankname = if_else(rank == 1, "worst", "best")) %>%
  select(season, rankname, name, median_note_points) %>%
  pivot_wider(
    names_from = rankname,
    values_from = name,
    values_fn = function(x) {
      paste(x, collapse = ", ")
    } # multiple athletes per year possible --> concat
  )
best_and_worst