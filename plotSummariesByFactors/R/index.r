#' Plot aggregated summaries across factor combinations
#'
#' Computes grouped summaries over provided factor combinations and renders
#' bar charts for each summary metric. Supports one or two grouping factors:
#' - 1 factor: bar chart of summary vs. factor
#' - 2 factors: dodged bar chart, fill mapped to the second factor
#'
#' @param df data.frame. Source data containing grouping and measure columns.
#' @param factor_cols list of character vectors (possibly nested) where each element
#'   is a character vector of length 1 or 2 naming grouping columns, e.g.
#'   `list(c("day_of_week"), c("month","member_casual"))`.
#' @param summary named character vector mapping output column names to dplyr
#'   summary expressions as strings, e.g. `c(avgDistance = "mean(distance_meters)",
#'   countRides = "n()")`.
#'
#' @return A list with two elements:
#'   - plots: a tibble with columns `summary`, `factor_1`, `factor_2`, `factor_3`, and `plot` (ggplot objects)
#'   - combined: a patchwork object combining all plots arranged with `ncol = length(summary)`
#'
#' @details Uses tidy-eval helpers (`syms`) and `eval(parse(...))` to convert
#'   provided strings to symbols/expressions for grouping and summarising.
#'   For each factor combination and each summary metric, a `ggplot2` column
#'   chart is created and combined with `patchwork::wrap_plots`.
#'
#' @examples
#' # summary metrics
#' summary <- c(
#'   avgDistance = "mean(distance_meters)",
#'   avgDuration = "mean(ride_duration)",
#'   countRides  = "n()"
#' )
#' # factor pairs
#' times <- c("day_of_week", "month")
#' factors <- c("member_casual")
#' factor_cols <- lapply(times, function(t) lapply(factors, function(f) c(t, f)))
#' # generate plots
#' graphItAll(df, factor_cols, summary)
#' @export
plotSummariesByFactors = function(df, factor_cols, summary) {
  graphs = lapply(factor_cols, function(factors) {
    mapply(function(summary_col, equation) {
      summarized = df %>%
        dplyr::group_by(!!!syms(unlist(factors))) %>%
        dplyr::summarise(!!summary_col := eval(parse(text = equation)))
      
      factors = unlist(factors)
      if(length(factors) == 1) {
        p = summarized %>% ggplot(aes(eval(parse(text = factors[1])),
                                         eval(parse(text = summary_col)))) + geom_col()
        return(list(plot = p, summary = summary_col, factors = factors))
      }
      if(length(factors) == 2) {
        p = summarized %>%
          ggplot(aes(x=eval(parse(text =factors[1])), y=eval(parse(text = summary_col)))) +
          geom_col(aes(fill= eval(parse(text = factors[2]))), position = "dodge") +
          labs(x=factors[1], y=summary_col, fill=factors[2])
        return(list(plot = p, summary = summary_col, factors = factors))
      }
      if(length(factors) == 3) {
        p = summarized %>%
          ggplot(aes(x=eval(parse(text =factors[1])), y=eval(parse(text = summary_col)))) +
          geom_col(aes(fill= eval(parse(text = factors[2]))), position = "dodge") +
          facet_wrap(~ eval(parse(text = factors[3]))) +
          labs(x=factors[1], y=summary_col, fill=factors[2])
        return(list(plot = p, summary = summary_col, factors = factors))
      }
      # summarized %>% ggplot(aes(!!factors[1], !!summary_col)) + geom_col()
    }, names(summary), summary, SIMPLIFY = FALSE)
  })
  flat_items = unlist(graphs, recursive = FALSE)
  
  plots_tbl = dplyr::bind_rows(lapply(flat_items, function(item) {
    fs = unlist(item$factors)
    tibble::tibble(
      summary   = item$summary,
      factor_1  = if (length(fs) >= 1) fs[1] else NA_character_,
      factor_2  = if (length(fs) >= 2) fs[2] else NA_character_,
      factor_3  = if (length(fs) >= 3) fs[3] else NA_character_,
      plot      = list(item$plot)
    )
  }))
  
  combined = patchwork::wrap_plots(plots_tbl$plot, ncol = length(summary))
  
  return(list(plots = plots_tbl, combined = combined))
  
}
