## code to prepare `tyonvalitystilasto_1270_ammatit` dataset goes here

url <- "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_1270.px"

query <-
  list("Alue"=c("SSS"),
       "Kuukausi"= c("*"),
       "Ammattiryhmä" = c("*"),
       "Tiedot"=c("HAKIJALOPUSSA","AVPAIKATLOPUSSA"))

# Hae data

pxd <- pxweb_get(url, query)
data <- as.data.frame(pxd)

data <- data %>%
  # statfitools::clean_names() %>%
  mutate(Vuosi = substring(Kuukausi, 1,4),
         Kuukausi = substring(Kuukausi, 5)) %>%
  statfitools::clean_times(sub_year_col = "Kuukausi")  %>%
  rename(Tyottomat = Tyottomia_tyonhakijoita_laskentapaivana,
         Avoimet_tyopaikat = Avoimia_tyopaikkoja_laskentapaivana) %>%
  select(-Alue)

data_ammatit_1270 <- data

usethis::use_data(data_ammatit_1270, overwrite = TRUE)
