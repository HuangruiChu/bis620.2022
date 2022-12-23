## code to prepare `DATASET` dataset goes here

ukb_accel <- readRDS("accel.rds")
usethis::use_data(ukb_accel, overwrite = TRUE)


zero_covid_2022<- read.csv("integrated_data_cn_2022.csv")
usethis::use_data(zero_covid_2022, overwrite = TRUE)

zero_covid_2021<- read.csv("integrated_data_cn_2021.csv")
usethis::use_data(zero_covid_2021, overwrite = TRUE)
