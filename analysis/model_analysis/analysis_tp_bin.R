
# Set up ----
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
source(file = file.path("models", "functions", "tp_bin_helpers.R"))
source(file = file.path("models", "functions", "analysis_helpers.R"))
source(file = file.path("report", "functions", "report_helpers.R"))

# Attributes ----
SEED <- 517
limit_overwrite <- FALSE
class_overwrite <- FALSE
pp_overwrite <- TRUE
output_path <- file.path("output", "2024-05-16_tp")
binfile <- file.path(output_path, "fit_tp01.Rds")
fit_binreg <- readRDS(binfile)
res_path <- file.path("output",
                      "2024-05-17_results",
                      "src")
roc_name <- "roc_data.R"
ck_name <- "limit_ck.Rds"
te_name <- "limit_te.Rds"
class_name <- "class_data.R"
pp_name <- "postpred_data.R"
nsim <- 3000
n_limits <- 100

train_data <- readRDS(file.path(
  "no_vc",
  "cache",
  "sample_train_standard.Rds"
))

test_data <- readRDS(file.path(
  "no_vc",
  "cache",
  "sample_test_standard.Rds"
))

test_data_og <- readRDS(file.path(
  "no_vc",
  "cache",
  "sample_test.Rds"
))

if (limit_overwrite | !file.exists(file.path(res_path, roc_name))) {
  print(paste("Calculating error ratios and optimization measures..."))
  z <- fit_binreg$data$data_kylla
  len <- n_limits
  limits <- seq(0, 1, length.out = len)
  errorratios <- data.frame(l = numeric(nsim*len),
                            proptp = numeric(nsim*len),
                            propfp = numeric(nsim*len))
  cohenkappas <- data.frame(l = numeric(nsim*len),
                            est = numeric(nsim*len))
  testerrors <- data.frame(l = numeric(nsim*len),
                           est = numeric(nsim*len))
  j <- 1
  set.seed(SEED)
  for (i in seq_len(nsim)) {
    print(paste(i, Sys.time()))
    binreg_epred <- posterior_epred(fit_binreg,
                                    draw_ids = i)
    
    # Classification limits
    cohenkappas[j:(j+len-1),] <- calcStatistic(binreg_epred, z, calcCohenKappa, limits)
    testerrors[j:(j+len-1),] <- calcStatistic(binreg_epred, z, calcTestError, limits)
    
    # Error ratios
    errorratios[j:(j+len-1),] <- calcStatistic(binreg_epred, z, errorRatio, limits)
    j <- j+len
  }
  saveRDS(errorratios, file.path(res_path, roc_name))
  
  # Classification accuracy
  cksum <- cohenkappas %>% 
    group_by(l) %>% 
    summarise(mean = mean(est))
  l_ck <- cksum$l[which.max(cksum$mean)]
  saveRDS(l_ck, file.path(res_path, ck_name))
  saveRDS(l_ck, file.path(output_path, ck_name))
 
  tesum <- testerrors %>% 
    group_by(l) %>% 
    summarise(mean = mean(est))
  l_te <- cksum$l[which.min(tesum$mean)]
  saveRDS(l_te, file.path(res_path, te_name))
  saveRDS(l_te, file.path(output_path, te_name))
  
}

if (class_overwrite | !file.exists(file.path(res_path, class_name))) {
  
  print(paste("Calculating false positive and false negative rates..."))
  
  l_ck <- readRDS(file.path(res_path, ck_name))
  l_te <- readRDS(file.path(res_path, te_name))
  
  ckres <- data.frame(
    "test_error" = numeric(nsim),
    "false_pos" = numeric(nsim),
    "false_neg" = numeric(nsim),
    "optim_stat" = "cohens_kappa")
  teres <- data.frame(
    "test_error" = numeric(nsim),
    "false_pos" = numeric(nsim),
    "false_neg" = numeric(nsim),
    "optim_stat" = "test_error")
  ppres <- data.frame(
    "test_error" = numeric(nsim),
    "false_pos" = numeric(nsim),
    "false_neg" = numeric(nsim),
    "optim_stat" = "none")
  
  z <- test_data$data_kylla
  set.seed(SEED)
  for (i in seq_len(nsim)) {
    print(paste(i, Sys.time()))
    binreg_epred <- posterior_epred(fit_binreg,
                                    newdata = test_data,
                                    draw_ids = i)
    ppred <- posterior_predict(fit_binreg,
                               newdata = test_data,
                               draw_ids = i)
    
    ckpred <- predictClass(binreg_epred, l = l_ck)
    ckres[i, "test_error"] <- calcPropIncorrect(ckpred, z)
    ckres[i, "false_pos"] <- calcFalsePos(ckpred, z)
    ckres[i, "false_neg"] <- calcFalseNeg(ckpred, z)
    
    tepred <- predictClass(binreg_epred, l = l_te)
    teres[i, "test_error"] <-  calcPropIncorrect(tepred, z)
    teres[i, "false_pos"] <- calcFalsePos(tepred, z)
    teres[i, "false_neg"] <- calcFalseNeg(tepred, z)
    
    ppres[i, "test_error"] <-  calcPropIncorrect(ppred, z)
    ppres[i, "false_pos"] <- calcFalsePos(ppred, z)
    ppres[i, "false_neg"] <- calcFalseNeg(ppred, z)
  }
  
  res_class <- bind_rows(ckres, teres, ppres)
  res_class <- res_class %>% 
    pivot_longer(cols = !c("optim_stat"),
                 names_to = "statistic",
                 values_to = "estimate")
  saveRDS(res_class, file.path(res_path, "class_data.R"))
  
}

# Posterior predictive statuses ----

if (pp_overwrite | !file.exists(file.path(res_path, pp_name))) {
  
  print(paste("Calculating error ratios for the posterior predictive case..."))
  
  z <- fit_binreg$data$data_kylla
  part <- 10
  pperrorratios <- data.frame("tprate" = numeric(nsim),
                              "fprate" = numeric(nsim))
  index <- seq(1, nsim, part)
  set.seed(SEED)
  for (i in seq_along(index)) {
    print(paste(i, Sys.time()))
    new_ids <- index[i]:(index[i]+part-1)
    binreg_ppred <- posterior_predict(fit_binreg,
                                      draw_ids = new_ids)
    
    # Error ratios
    pperrorratios[new_ids,"tprate"] <- apply(binreg_ppred, 1, truePosRate, z)
    pperrorratios[new_ids,"fprate"] <- apply(binreg_ppred, 1, falsePosRate, z)
  }
  
  saveRDS(pperrorratios, file.path(res_path, pp_name))

}

pper <- readRDS(file.path(res_path, pp_name))
