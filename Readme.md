# Reducing Repetition in Data Analysis, R

`plotSummariesByFactors` automates plotting summary statistics grouped by 1, 2, or 3 factors.

```{r}
# summary metrics
summary <- c(
	avgDistance = "mean(distance_meters)",
	avgDuration = "mean(ride_duration)",
    countRides = "n()"
)
# factor pairs
times <- c("day_of_week", "month")
factors <- c("member_casual", "bike_type")
factor_cols <- lapply(times, function(t){
	lapply(factors, function(f){
		c(t, f)
	})
})
# generate plots
res <-plotSummariesByFactors(df, factor_cols, summary)
res$combined # display combined plot
res$plots # a tibble of factors, summaries, and individual plots, for use in reports
```

The above will produce the Cartesian product of the provided factor combinations and summary metrics, resulting in timesxfactorsxsummaries, or 2x2x2=8 bar plots, each corresponding to one of the possible combinations of factors and summaries.

## Installation

```{r}
devtools::install_github("Iancam/plotSummariesByFactors")
```

## Dependencies

- rlang (syms)
- dplyr
- ggplot2
- tibble
- patchwork

## Quick Start

```{r}
library(plotSummariesByFactors)
# Define summaries (named character vector of dplyr expressions)
summary <- c(
  avgDistance = "mean(distance_meters)",
  avgDuration = "mean(ride_duration)",
  countRides  = "n()"
)

# Define factor combinations (each element is a character vector of length 1–3)
factor_cols <- list(
  c("day_of_week"),
  c("month", "member_casual"),
  c("month", "member_casual", "bike_type")
)

res <- plotSummariesByFactors(df, factor_cols, summary)

# Inspect individual plots as a tibble
res$plots

# Show combined patchwork of all plots (ncol = length(summary))
res$combined
```

## Function Reference

`plotSummariesByFactors(df, factor_cols, summary)`

- `df`: data.frame with the grouping columns and measure columns used in summary.
- `factor_cols`: list of character vectors (length 1–3) naming grouping columns.
  Examples: list(c("day_of_week"), c("month", "member_casual"))
- `summary`: named character vector mapping output names → dplyr summary expressions as strings.
  Example: c(avgDuration = "mean(ride_duration)", countRides = "n()")
  Returns a list:

plots: tibble with columns summary, factor_1, factor_2, factor_3, plot (ggplot)
combined: patchwork object combining all plots with ncol = length(summary)

## Examples

One factor (bar):

```{r}
plotSummariesByFactors(
  df,
  list(c("day_of_week")),
  c(avgDuration = "mean(ride_duration)")
)$combined
```

Two factors (dodged bar, fill = factor_2):

```{r}
plotSummariesByFactors(
  df,
  list(c("month", "member_casual")),
  c(countRides = "n()")
)$combined
```

Three factors (facet by factor_3):

```{r}
plotSummariesByFactors(
  df,
  list(c("month", "member_casual", "bike_type")),
  c(avgDistance = "mean(distance_meters)")
)$combined
```

## Generating Factor Combinations

```{r}
library(comprehenr)
factors_1 = c("month", "year")
factors_2 = c("member_casual", "rideable_type")
summaries = c(
	"numRides"="n()",
	"avgDuration" = "mean(ride_duration)"
)
factor_tuples = to_list(for (f1 in factors_1) for (f2 in factors_2) c(f1, f2))
```

## Why?

Say you're tasked with exploring a bike share data set. You're comparing the behavior of different membership classes, and your dataset has columns for:

- ride_duration
- distance_meters
- bike_type
- member_casual
- month
- year
- day_of_week
- hour

You might begin with, What is the relationship between duration and member_casual? To answer, you formalize the question into code:

```{r}
df %>%
	group_by(member_casual) %>%
	summarise(avgDuration = mean(ride_duration)) %>%
	ggplot(aes(x=member_casual, y=avgDuration)) + geom_col()
```

The resultant graph puts membership type on the x-axis and average trip duration on the y. Then you ask, are there seasonal trends? To answer, you have to translate the question into the domain of the data: One candidate is, What is the relationship between the average duration, per membership type, per month? Once expressed in terms of your data, you again write code, which looks similar but not exactly the same as before. This time you might "dodge" the bars by membership type, creating a grouped bar plot, or facet by membership type, creating a faceted set of bar plots.

Answering each of these questions in isolation takes a lot of steps, but if you step back, a pattern emerges. Each question can be expressed as a relationship between ($factor_1, $factor_2, ... $factor_k) and $summary_statistic_j. For k=1, k=2, and k=3, we can answer with a bar plot, a grouped bar plot, and a faceted set of grouped bar plots, respectively.

When the question space is formalized in this way, you no longer have to specify each question in isolation; you can generate a set of interesting questions by taking the Cartesian product of factors and summary statistics. Rather than dealing with the language of graphs, group_bys, and summaries, you can focus on the variables specific to the question space: which factors influence which summary statistics.

## Licence: MIT
