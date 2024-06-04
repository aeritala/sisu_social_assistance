
library(ggplot2)
library(dplyr)
library(kableExtra)

source(file = file.path("models",
                        "functions",
                        "analysis_helpers.R"))

errorMatrix <- function(data, simul) {
  error_matrix <- table(simul, data) %>% 
    prop.table(.)*100
  error_matrix <- data.frame("non-recipients" = error_matrix[1,],
                             "recipients" = error_matrix[2,],
                             row.names = c("Non-recipient",
                                           "Recipient"))
  print(error_matrix)
}

reg19 <- readRDS(file.path("no_vc",
                               "cache",
                               "sisu_est_register_data_2019_tidy.Rds"))
reg19 <- reg19 %>%
  mutate(perustt = ifelse(perustt < 1, 0, perustt),
         perustt_kylla = ifelse(perustt != 0, 1, 0))

errorMat <- errorMatrix(reg19$perustt_kylla, reg19$simul_kylla)

kbl(errorMat,
    digits = 2,
    col.names = c("", "Non-recipient", "Recipient")) %>%
  add_header_above(c("", "SISU--model" = 2))

saveRDS(errorMat,
        file = file.path("reports",
                         "content_seminaarityo",
                         "table-desc_errmat-sisu.Rds"))

# FN and FP rates
calcFalseNeg(reg19$perustt_kylla, reg19$simul_kylla)*100
calcFalsePos(reg19$perustt_kylla, reg19$simul_kylla)*100

# Tuen saajien osuus
table(rek400k$data_kylla)
prop.table(table(rek400k$data_kylla))

# Vain tukea saaneet

toimtuki <- subset(rek_malli, rek_malli$data_kylla != 0 | rek_malli$simul_kylla != 0)

errorDescriptives <- function(data, my_var) {
  data %>% summarise(sum = sum({{ my_var }}),
                          median = median({{ my_var }}),
                          mean = mean({{ my_var }}),
                          sd = sd({{ my_var }}))
}


errorDescriptives(toimtuki, TOIMTUKI_DATA)
errorDescriptives(toimtuki, TOIMTUKI_SIMUL)
