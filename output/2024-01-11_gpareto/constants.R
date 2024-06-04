# load helpers ----
suppressMessages(suppressWarnings(library(dplyr)))

source(file = file.path("analysis",
                        "model_analysis",
                        "write_submodel_tasks.R"),
       local = TRUE)
source(file = file.path("analysis",
                        "model_analysis",
                        "write_analysis_tasks.R"),
       local = TRUE)

# set up ----
SEED <- 1129
task <- "2024-01-11_gpareto"
task_path <- file.path("output",
                       task)
#dir.exists(task_path)
fit_tag <- "gpareto"

sa_formula <- "toimtuki_data ~ fikavuv + sp + frake + palkat1k_aik + paatoim_sisu + asumtuet_data + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + ftoimtuki_simul + lapsip_data + muusoset_data_sum + kansel_perhel_data"
f <- c("toimtuki_data ~ fikavuv + asumtuet_data",
       sa_formula)