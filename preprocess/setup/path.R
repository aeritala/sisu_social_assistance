
library(stringr)

original_path <- Sys.getenv("PATH")
original_path <- str_remove(original_path, "C:\\ProgramData\\Anaconda3\\Scripts;C:\\ProgramData\\Anaconda3;")
