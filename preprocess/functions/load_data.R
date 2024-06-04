# Load data

# Register data set
register_data_2019 <- read.table(
  file.path("no_vc",
            "raw",
            "rek-400kotos2019.tab"),
  header = TRUE,
  sep = "\t",
  dec = ".")

saveRDS(register_data_2019,
        file = file.path(
          "no_vc",
          "raw",
          "register_data_2019.Rds"
        ))

# SISU-model estimates with KOKOSimul program,
# using the register data set of 2019
sisu_est_register_data_2019 <- read.table(file.path(
  "no_vc",
  "raw",
  "koko_simul_07SEP23_1_koti.txt"
),
header=T,
sep = "\t")

saveRDS(sisu_est_register_data_2019,
        file = file.path(
          "no_vc",
          "raw",
          "sisu_est_register_data_2019.Rds"
        ))
