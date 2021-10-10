## code to prepare `BACI_HS92_Y2018_V202001` dataset goes here
BACI_HS92_Y2018_V202001<- read.csv("BACI_HS92_Y2018_V202001.csv", header = TRUE)

usethis::use_data(BACI_HS92_Y2018_V202001, overwrite = TRUE, compress = "xz")
