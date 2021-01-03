#' Filters the desired region from the dataset.
#'
#' Tilastokeskuksen taulukoissa on usein tietoja eri aggregointitasolla. Koska suurimmalla
#' aggregointitasolla on vähiten suojattuja tietoja (vähemmän kuin 5 havaintoa) tulisi anlyysissä
#' käyttää aina suurinta mahdollista aggregointitasoa siten että, jos haluaa aggregoida alueita,
#' joita pystyy aggregoimaan seutukunnista, ei tulisi aggregoida kuntatiedoista, vaan seutukunta
#' tiedoista. Jos aggregointiin voi käyttää maakuntia, vielä parempi.
#'
#' Onko tämä koodi yleistettävissä muihin pxwebin taulukoihin?
#'
#' @param data data.frame, contains the regional data
#' @param region character, the desired region.
#' @return data.frame
#' @export

filter_region <- function(data, region) {

  if(region == "kunta") {
    output <- filter(data,!grepl("MK", Alue) & !grepl("SK", Alue) &
                       !grepl("ELY", Alue) & !grepl("KOKO", Alue)) %>%
      mutate(kunta = Alue) %>%
      select(-Alue)
  } else if (region == "seutukunta") {
    output <- filter(data, grepl("SK", Alue))  %>%
              mutate(seutukunta_koodi = substring(Alue, 3,5),
                     seutukunta = substring(Alue, 7)) %>%
              select(-Alue, -seutukunta_koodi)
  } else if (region == "maakunta") {
    output <- filter(data, grepl("MK", Alue)) %>%
              mutate(maakunta_koodi = substring(Alue, 3,4),
                     maakunta = substring(Alue, 6)) %>%
              select(-Alue, -maakunta_koodi)
  } else if (region %in% c("koko maa", "KOKO MAA")) {
      output <- filter(data, grepl("KOKO MAA", Alue)) %>%
                select(-Alue)
  } else {stop("Input region not applicable.")}
  output
}
