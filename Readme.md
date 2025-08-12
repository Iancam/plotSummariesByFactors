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
plotSummariesByFactors(df, factor_cols, summary)
```

The above will produce 2x2x2=8 bar plots, each corresponding to one of the possible combinations of factors and summaries.

---

## Why?

Say you're tasked with exploring a bike share data set. You're comparing the behavior of different membership classes, and your dataset has columns for:

- duration
- distance
- bike_type
- membership
- month
- year
- day of week
- hour

You might begin with, What is the relationship between duration and membership? To answer, you formalize the question into code:

```{r}
df %>%
	group_by(membership) %>%
	summarise(avgDuration = mean(duration)) %>$
	ggplot(aes(x=membership, y=avgDuration)) + geom_col()
```

The resultant graph puts membership type on the x-axis and average trip duration on the y. Then you ask, are there seasonal trends? To answer, you have to translate the question into the domain of the data: One candidate is, What is the relationship between the average duration, per membership type, per month? Once expressed in terms of your data, you again write code, which looks similar but not exactly the same as before.

Answering each of these questions in isolation takes a lot of steps, but if you step back, a pattern emerges. Each question can be expressed as a relationship between ($factor_1, $factor_2, ... $factor_k) and $summary_statistic_j. For k=1, k=2, and k=3, we can answer with a bar plot, a grouped bar plot, and a faceted set of grouped bar plots, respectively.

When the question space is formalized in this way, you no longer have to specify each question in isolation; you can generate a set of interesting questions by taking the Cartesian product of factors and summary statistics. Rather than dealing with the language of graphs, group_bys, and summaries, you can focus on the language of exploring the question space: generating and filtering questions and evaluating graphs for patterns.

To get the Cartesian product, I used nested `lapply` functions, but you could use the `comprehenr` package for more Pythonic syntax.

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
