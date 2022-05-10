## code to prepare `ExampleTradeData` dataset goes here

ReadTradeData <-read.csv("BACI_HS17_Y2020_V202201.csv")
ExampleTradeData<-ReadTradeData[ReadTradeData$i == c(4, 710),]

usethis::use_data(ExampleTradeData, overwrite = TRUE, compress = "xz")
