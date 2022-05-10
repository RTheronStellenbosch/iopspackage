## code to prepare `ExampleValueChain` dataset goes here

ExampleValueChain <- read.csv("Auto Value Chain - updated for H5 - v2.csv", header = FALSE)
colnames(ExampleValueChain) <- c("tierNumber", "GVCactivityNumber", "HScode")

usethis::use_data(ExampleValueChain, overwrite = TRUE, compress = "xz")
