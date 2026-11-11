# Reads a product list from data/. Named a_catalog.R so it loads before the files that use it.
read_catalog <- function(file) {
  df <- utils::read.csv(file.path("data", file), stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                        na.strings = character(), colClasses = "character")
  df[] <- lapply(df, function(x) { Encoding(x) <- "UTF-8"; x })
  df$price <- as.numeric(df$price)
  df[df$active %in% c("TRUE", "true"), , drop = FALSE]
}
