zero <- function(x) {
  gsub("0 ( 0.0)", "0", x, fixed = TRUE) %>%
    {gsub("^0.0", "<0.1", .)}
}
