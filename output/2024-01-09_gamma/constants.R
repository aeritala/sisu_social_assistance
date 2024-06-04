# load helpers ----
library(dplyr)
source(file = file.path("analysis",
                        "model_analysis",
                        "write_submodel_tasks.R"),
       local = TRUE)
source(file = file.path("analysis",
                        "model_analysis",
                        "write_analysis_tasks.R"),
       local = TRUE)
source(file = file.path("models",
                        "tasks",
                        "hypotheses.R"),
       local = TRUE)

# setup ----
SEED <- 0109
task <- "2024-01-09_gamma"
task_path <- file.path("output",
                       task)
dir.exists(task_path)
fit_tag <- "gamma"
dist <- "gamma"