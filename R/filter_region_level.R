#' Filters the desired region from the dataset
#'
#' #' Tilastokeskuksen taulukoissa on usein tietoja eri aggregointitasolla. Koska suurimmalla
#' aggregointitasolla on vähiten suojattuja tietoja (vähemmän kuin 5 havaintoa) tulisi anlyysissä
#' käyttää aina suurinta mahdollista aggregointitasoa siten että, jos haluaa aggregoida alueita,
#' joita pystyy aggregoimaan seutukunnista, ei tulisi aggregoida kuntatiedoista, vaan seutukunta
#' tiedoista. Jos aggregointiin voi käyttää maakuntia, vielä parempi.
#'
#' Kunnille ei tule koodeja suoraan pxwebin aineistosta
#'
#'
#'
#' @param data data.frame, contains the regional data
#' @param region character, the desired region.
#' @return data.frame
#' @export

filter_region_level <- function(data, region) {

  if(region == "kunta") {
    output <- filter(data,!grepl("MK", alue) & !grepl("SK", alue) &
                       !grepl("ELY", alue) & !grepl("KOKO", alue)) %>%
      mutate(kunta_name = alue) %>%
      select(-alue)
  } else if (region == "seutukunta") {
    output <- filter(data, grepl("SK", alue))  %>%
              mutate(seutukunta_code = factor(statfitools::extract_code(alue, numbers_as_numeric = FALSE)),
                     seutukunta_name = factor(statfitools::extract_name(alue))) %>%
              select(-alue)
  } else if (region == "maakunta") {
    output <- filter(data, grepl("MK", alue)) %>%
              mutate(maakunta_code = factor(statfitools::extract_code(alue, numbers_as_numeric = FALSE)),
                     maakunta_name = factor(statfitools::extract_name(alue))) %>%
              select(-alue)
  } else if (region %in% c("koko maa", "KOKO MAA", "SSS")) {
      output <- filter(data, grepl("KOKO MAA", alue) | grepl("SSS", alue)) %>%
                select(-alue)
  } else {stop("Input region not applicable.")}
  output
}
