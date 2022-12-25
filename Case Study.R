library(tibble)
library(dplyr)
library(ggplot2)
library(readr)
library(pheatmap)
library(tidyr)
library(purrr)

# load data
all_comps = read_csv('all_comps.csv')
skiresults = read_csv('skiresults.csv')

# subset with only the columns requested in the task
all_comps_compact = all_comps[c("id", "season", "gender", "hill_size_x")]

# adding type column
all_comps$type = ""
all_comps$type[all_comps$hill_size_x < 50] = "Small Hill"
all_comps$type[all_comps$hill_size_x %in%  c(50:84)] = "Medium Hill"
all_comps$type[all_comps$hill_size_x %in%  c(85:109)] = "Normal Hill"
all_comps$type[all_comps$hill_size_x %in%  c(110:189)] = "Large Hill"
all_comps$type[all_comps$hill_size_x > 189] = "Sky Flying Hill"

# merge datasets
mergedData = merge(all_comps, skiresults, by = "id")
mergedData_numeric = select_if(mergedData, is.numeric)
summary(mergedData)

# todo: check where correlation makes sense
correlation = round(cor(select_if(mergedData, is.numeric), method = "spearman"), 2)
correlation = as.data.frame(correlation)
correlation = correlation[-c(6, 7, 8), -c(6, 7, 8)] # removed this rows and colums because there was only NA in it

# generating a heatmap for the correlation
pheatmap(
  correlation,
  display_numbers = TRUE,
  main = "correlation heatmap",
  treeheight_row = 0,
  treeheight_col = 0,
)

# see distributions per variable
for (name in names(select_if(mergedData, is.numeric))) {
  data = as.matrix(select_if(mergedData, is.numeric)[name])
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
    group_by(get(group_by_column)) %>%
    summarise(
      q25 = quantile(get(data_column), probs = .25, na.rm = TRUE),
      q75 = quantile(get(data_column), probs = .75, na.rm = TRUE)
    )
  
  coords = coord_cartesian(ylim = c(
    min(grouped_by_quantiles$q25),
    max(grouped_by_quantiles$q75)
  ))
  
  # calculate means per group
  means = data %>%
    group_by(get(group_by_column)) %>%
    summarise_all(mean)
  
  points = geom_point(data = means, aesthetics)
  
  labels = labs(
    title = paste(data_column, "by", group_by_column, sep = " "),
    subtitle = "median, upper and lower quartile, mean",
    x = group_by_column,
    y = data_column
  )
  
  # build and return plot
  plot =  plot + boxplot + coords + points + labels
  return(plot)
}

# define for which groups the boxplots should be drawn
interesting_groups = c("round", "gender", "season")
# iterate over groups and numeric values
for (group in interesting_groups) {
  for (name in names(select_if(mergedData, is.numeric))) {
    print(name)
    print(draw_grouped_boxplot(mergedData, group, name))
  }
}

#Reduce the data such that it only contains information about the final rounds on a ski flying hill.
#• Add a variable n_final_rounds to the data set, which provides per pair of athlete and season the number of final
#rounds (on a ski flying hill) that he/she participated in.
#• For each athlete, who participated in at least 5 final rounds on a ski flying hill, compute the median amount of
#note_points across all competitions per season.
#• Based on the previously computed aggregated note points, produce a tibble that provides the names of the
#best and worst athlete(s) per season.
finalsSkyFlyingHill = mergedData %>%
  filter(round == "final round") %>%
  filter(type == "Sky Flying Hill")

test = finalsSkyFlyingHill[c("season", "name")] %>%
  distinct()
test


n_final_rounds = finalsSkyFlyingHill %>%
  group_by(name, season) %>%
  summarise(n_final_rounds = n()) %>%
  ungroup()
n_final_rounds

n_final_rounds_sum = n_final_rounds %>%
  group_by(name) %>%
  mutate(sum = sum(n_final_rounds)) %>%
  ungroup() %>%
  distinct_at(vars(name, sum))

select_if(mergedData, n_final_rounds_sum$sum[mergedData$name] > 5) %>%
  select_if()


test1 = lmap(test, calcSeasonSum)
test1

add_column(test, test1)
