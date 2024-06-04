
getStandardizationPars <- function(dataset) {
  df_names <- names(dataset)
  df_names <- sapply(df_names, function(x) {
    if (!(x %in% c("knro", "toimtuki_data")) &
        !is.integer(dataset[[x]]) &
        !is.factor(dataset[[x]])) {
      return(x)
    }
  })
  s_names <- unlist(df_names)
  standardize_pars <- data.frame("mean" = numeric(length(s_names)),
                                 "sd" = numeric(length(s_names)),
                                 row.names = names(s_names)) 
  
  for (name in s_names) {
    standardize_pars[name,] <- c(mean(dataset[[name]]),
                                 sd(dataset[[name]]))
  }
  
  return(standardize_pars)
}

standardize <- function(x, mean, sd) {
  if (is.na(mean) | is.na(sd)) {
    warning("Error: Mean or SD is NA, variable
            not standardized.")
    return(x)
  } else {
    return((x - mean) / sd)
  }
}

standardizeDataSet <- function(dataset, standardize_pars) {
  s_names <- rownames(standardize_pars)
  for (name in s_names) {
    newmean <- standardize_pars[name, "mean"]
    newsd <- standardize_pars[name, "sd"]
    dataset[[name]] <- standardize(dataset[[name]],
                                    newmean,
                                    newsd)
  }
  return(dataset)
}

applyStandardizationPars <- function(standard_pars, datasets) {
  for (i in seq_along(datasets)) {
    datasets[[i]] <- standardizeDataSet(
      dataset = datasets[[i]],
      standardize_pars = standard_pars)
  }
  return(datasets)
}


saveDataSet <- function(dataset, filename, newpath, tag = NULL) {
  #filename <- deparse(substitute(dataset))
  if (!is.null(tag)) {
    tag <- paste0("_", tag)
  }
  saveRDS(dataset,
          file = file.path(newpath,
                           paste0(filename, tag, ".Rds")))
}