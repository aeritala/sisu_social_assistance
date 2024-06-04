writeOption <- function(option, value) {
  paste0("--", option, "=", value)
}

addParentheses <- function(string) {
  for (i in seq_along(string)) {
    if (isFALSE(grepl("\"", string[i]))) {
      string[i] <- paste0("\"", string[i], "\"")
    }
  }
  return(string)
}