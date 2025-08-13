

#' Plot aggregated summaries across factor combinations
#'
#' Computes grouped summaries over provided factor combinations and renders
#' bar charts for each summary metric. Supports one, two, or three grouping factors:
#' - 1 factor: bar chart of summary vs. factor
#' - 2 factors: dodged bar chart, fill mapped to the second factor
#' - 3 factors: facet_wrap on the third factor
#'
#' @param df data.frame. Source data containing grouping and measure columns.
#' @param factor_cols list of character vectors (possibly nested) where each element
#'   is a character vector of length 1, 2, or 3 naming grouping columns, e.g.
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
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise across all_of
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs 
#' @importFrom rlang syms := .data
#' @examples
#' # summary metrics
#' df <- tibble::tibble(
#'   distance_meters = c(1200, 800, 1500, 900, 1100, 700),
#'   ride_duration   = c(600, 540, 720, 500, 650, 480),  # seconds
#'   day_of_week     = factor(
#'     c("Mon","Mon","Tue","Tue","Wed","Wed"),
#'     levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
#'     ordered = TRUE
#'   ),
#'   month           = factor(
#'     c("Jan","Jan","Jan","Feb","Feb","Feb"),
#'     levels = month.abb,
#'     ordered = TRUE
#'   ),
#'   member_casual   = factor(c("member","casual","member","casual","member","casual"))
#')
#' summary <- c(
#'   avgDistance = "mean(distance_meters)",
#'   avgDuration = "mean(ride_duration)",
#'   countRides  = "dplyr::n()"
#' )
#' # factor pairs
#' times <- c("day_of_week", "month")
#' factors <- c("member_casual")
#' factor_cols <- lapply(times, function(t){
#'   lapply(factors, function(f){
#'     c(t, f)
#'   })
#' })
#' # generate plots
#' plotSummariesByFactors(df, factor_cols, summary)
#' @export
plotSummariesByFactors = function(df, factor_cols, summary) {
  graphs = lapply(factor_cols, function(factors) {
    mapply(function(summary_col, equation) {
      summarized = df %>%
        group_by(across(all_of(unlist(factors)))) %>%
        summarise(!!summary_col := rlang::eval_tidy(rlang::parse_expr(equation)))
      
      factors = unlist(factors)
      if(length(factors) == 1) {
        p = summarized %>%
          ggplot(aes(x=.data[[factors[1]]],
                     y=.data[[summary_col]])) +
          geom_col() +
          labs(x=factors[1], y=summary_col)
        return(list(plot = p, summary = summary_col, factors = factors))
      }
      if(length(factors) == 2) {
        p = summarized %>%
          ggplot(aes(x=.data[[factors[1]]], y=.data[[summary_col]])) +
          geom_col(aes(fill= .data[[factors[2]]]), position = "dodge") +
          labs(x=factors[1], y=summary_col, fill=factors[2])
        return(list(plot = p, summary = summary_col, factors = factors))
      }
      if(length(factors) == 3) {
        p = summarized %>%
          ggplot(aes(x=.data[[factors[1]]], y=.data[[summary_col]])) +
          geom_col(aes(fill= .data[[factors[2]]]), position = "dodge") +
          facet_wrap(~ .data[[factors[3]]]) +
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


