# Sample data set

# Set up ----
library(dplyr)

SEED <- 1912
cache_path <- file.path("no_vc",
                        "cache")
sisu_data <- readRDS(file.path(
  cache_path,
  "sisu_est_register_data_2019_tidy.Rds"
))

source(file = file.path("preprocess",
                        "functions",
                        "sampling_helpers.R"),
       local = TRUE)

# sampling ----

N <- nrow(sisu_data)
i_total <- 1:N
N_test <- (0.2*N) %>% as.integer() 

# test data set
set.seed(SEED)
i_test <- sample(i_total, N_test)
sample_test <- sisu_data[i_test,]
sample_train <- sisu_data[-i_test,]
sample_trainpos <- sample_train %>%
  filter(toimtuki_data > 0)

# training and validation data sets for the continuous component
sample_train_pos <- sample_train %>% 
  filter(toimtuki_data > 0)
i_nontest_pos <- 1:nrow(sample_train_pos)

N_realvalid <- (0.2*nrow(sample_train_pos)) %>% as.integer()
set.seed(SEED)
i_realvalid <- sample(i_nontest_pos, N_realvalid)
sample_realvalid <- sample_train_pos[i_realvalid,]
sample_realtrain <- sample_train_pos[-i_realvalid,]

# training and validation data sets for the binary component
N_realtrain <- nrow(sample_realtrain)
i_nontest <- 1:nrow(sample_train)
set.seed(SEED)
i_binvalid <- sample(i_nontest, N_realvalid)
sample_binvalid <- sample_train[i_binvalid,]
set.seed(SEED)
i_bintrain <- sample(i_nontest[-i_binvalid], N_realtrain)
sample_bintrain <- sample_train[i_bintrain,]

# save data sets ----
datasets <- list(
  sample_test = sample_test,
  sample_train = sample_train,
  sample_bintrain = sample_bintrain,
  sample_binvalid = sample_binvalid,
  sample_realtrain = sample_realtrain,
  sample_realvalid = sample_realvalid
  )

# for (i in seq_along(datasets)) {
#   saveDataSet(dataset = datasets[[i]],
#               filename = names(datasets)[i],
#               newpath = cache_path)
# }

 # standardize data sets ----
real_s_pars <- getStandardizationPars(sample_realtrain)

real_datasets <- list(
  sample_realtrain = sample_realtrain,
  sample_realvalid = sample_realvalid
  )

real_datasets <- applyStandardizationPars(real_s_pars, real_datasets)

# for (i in seq_along(real_datasets)) {
#   saveDataSet(dataset = real_datasets[[i]],
#               filename = names(real_datasets)[i],
#               tag = "standard",
#               newpath = cache_path)
# }

bin_s_pars <- getStandardizationPars(sample_bintrain)

bin_datasets <- list(
  sample_bintrain = sample_bintrain,
  sample_binvalid = sample_binvalid
)

bin_datasets <- applyStandardizationPars(bin_s_pars, bin_datasets)

# for (i in seq_along(bin_datasets)) {
#   saveDataSet(dataset = bin_datasets[[i]],
#               filename = names(bin_datasets)[i],
#               tag = "standard",
#               newpath = cache_path)
# }

test_s_pars <- getStandardizationPars(sample_train)

test_datasets <- list(
  sample_train = sample_train,
  sample_trainpos = sample_trainpos,
  sample_test = sample_test
)

test_datasets <- applyStandardizationPars(test_s_pars, test_datasets)

# for (i in seq_along(test_datasets)) {
#   saveDataSet(dataset = test_datasets[[i]],
#               filename = names(test_datasets)[i],
#               tag = "standard",
#               newpath = cache_path)
# }
browser()
