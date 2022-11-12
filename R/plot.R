
#' Plot UKBiobank Accelerometry Data
#' This function plots UKBiobank accelerometry data.
#' @param x an object inherited from data.frame. It is
#' assumed to have a `time`, `X`, `Y`, and `Z` column along with
#' either a `time` or `freq` column.
#' @return a time-series plot faceted by axis.
#' @importFrom tidyr pivot_longer all_of
#' @importFrom ggplot2 aes_string ggplot geom_line facet_grid
#' @examples
#' data(ukb_accel)
#' accel_plot(ukb_accel[1:1000,])
#' @export
accel_plot = function(x) {
  if ("time" %in% colnames(x)) {
    col = "time"
  } else if ("freq" %in% colnames(x)) {
    col = "freq"
  } else {
    stop("A `time` or a `freq` column must appear in the data.")
  }
  x |>
    pivot_longer(-all_of(col)) |>
    ggplot(aes_string(x = col, y = "value")) +
      geom_line() +
      facet_grid(name ~ .)
}
