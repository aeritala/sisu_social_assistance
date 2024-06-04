# load helpers ----
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(stringr)))
source(file = file.path("analysis",
                        "model_analysis",
                        "write_submodel_tasks.R"),
       local = TRUE)
source(file = file.path("analysis",
                        "model_analysis",
                        "write_analysis_tasks.R"),
       local = TRUE)

# set up ----
SEED <- 0131
task <- "2024-01-31_tp"
task_path <- file.path("output",
                       "2024-03-12_tp")
bern_path <- file.path("output",
                       "2024-01-26_bernoulli")
weib_path <- file.path("output",
                      "2024-01-09_weibull")
gamma_path <- file.path("output",
                        "2024-01-09_gamma")
ggamma_path <- file.path("output",
                         "2024-01-10_ggamma_alpha")

#dir.exists(task_path)
tag_bern <- "tpbernoulli"
dist_bern <- "bernoulli"
tag_weib <- "tpweibull"
dist_weib <- "weibull"
tag_gamma <- "tpgamma"
dist_gamma <- "gamma"
tag_ggamma <- "tpggamma"
dist_ggamma <- "generalized_gamma"