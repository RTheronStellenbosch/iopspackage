## code to prepare `Auto_Value_Chain` dataset goes here
Auto_Value_Chain <- read.csv("Auto_Value_Chain.csv")

usethis::use_data(Auto_Value_Chain, overwrite = TRUE)
