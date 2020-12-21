## code to prepare `data_kunta_1001`, 'data_seutukunta_1001', data_maakunta_1001', 'data_kokomaa_1001' dataset goes here

# kuntadataan tulisi lisätä kooditä käyttäen hallintoaluekey-funktiota

library(statfiLaborMarkets)
library(statfitools)
library(tidyverse)
library(pxweb)

url <- "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_1001.px"

query <-
  list("Alue"=c("*"),
       "Kuukausi"= c("*"),
       "Tiedot"=c("TYOTTOMAT","TYOVOIMA","AVPAIKAT"))

# Hae data

data <- as.data.frame(pxweb::pxweb_get(url, query))

# Clean data

data <- data %>%
  statfitools::clean_times2() %>%
  statfitools::clean_names()

# Filter different regional level data

data_kunta_1001 <- filter_region_level(data, "kunta")
data_seutukunta_1001 <- filter_region_level(data, "seutukunta")
data_maakunta_1001 <- filter_region_level(data, "maakunta")
data_kokomaa_1001 <- filter_region_level(data, "koko maa")

usethis::use_data(data_kunta_1001, data_seutukunta_1001, data_maakunta_1001, data_kokomaa_1001,
                  overwrite = TRUE)
