zero <- function(x) {
  gsub("0 ( 0.0)", "0", x, fixed = TRUE) %>%
    {gsub("0.0", "<0.1", .)}
}

# Cached manually and not loaded by default
model_data <-
  if (file.exists("cache/model_data.rds"))
    readRDS("cache/model_data.rds")
