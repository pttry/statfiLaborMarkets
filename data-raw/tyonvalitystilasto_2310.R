# 2310, Ei ammatteja

# Luo query

url <-  "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_2310.px"

query <-
  list("Alue"=c("*"),
       "Ammattiryhmä" = c("SSS"),
       "Kuukausi" = c("*"),
       "Tiedot" = c("*"))

# Hae data

pxd <- pxweb_get(url, query)
data <- clean_names(as.data.frame(pxd))

data <- data %>%
  statfitools::clean_names() %>%
  mutate(Vuosi = substring(Kuukausi, 1,4),
         Kuukausi = substring(Kuukausi, 5)) %>%
  statfitools::clean_times(sub_year_col = "Kuukausi")

data <- data %>% dplyr::select(-Ammattiryhma)

data_kunta_2310 <- filter_region(data, "kunta")
data_seutukunta_2310 <- filter_region(data, "seutukunta")
data_maakunta_2310 <- filter_region(data, "maakunta")
data_kokomaa_2310 <- filter_region(data, "koko maa")

usethis::use_data(data_kunta_2310, data_seutukunta_2310, data_maakunta_2310, data_kokomaa_2310,
                  overwrite = TRUE)
