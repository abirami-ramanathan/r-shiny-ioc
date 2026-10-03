fmt_number <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x)) return("—")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_currency <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x)) return("—")
  paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x)) return("—")
  paste0(formatC(x, format = "f", digits = digits), "%")
}

keep_selected <- function(data, column, selected) {
  if (is.null(selected) || length(selected) == 0 || "All" %in% selected) {
    return(data)
  }
  data %>% filter(.data[[column]] %in% selected)
}

empty_plot <- function(message = "No records match the selected filters.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = 5) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}

kpi_card <- function(title, value, subtitle = "") {
  shiny::div(
    class = "kpi-card",
    shiny::div(class = "kpi-title", title),
    shiny::div(class = "kpi-value", value),
    shiny::div(class = "kpi-subtitle", subtitle)
  )
}
