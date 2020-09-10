#' Region classification keys
#'
#' Imports NUTS-region classification keys from Statistics Finland.
#'
#' @param region character, the smallest region desired in the resulting classification key.
#' @param codes logical, whether the classification key contains region names or region codes. Defaults to FALSE.
#' @param year character or numerical, the year of desired classification key. Defaults to current year.
#' @return data.frame Returns a classification key as a data.frame.
#' @export
#' @examples
#'

hallintoaluekey <- function(region = "kunta",
                            codes = FALSE,
                            year = format(Sys.Date(), "%Y")) {

  alueet <- factor(c("kunta", "seutukunta", "maakunta", "suuralue"), levels = c("kunta", "seutukunta", "maakunta", "suuralue"))
  hallintoalueet <- c("seutukunta", "maakunta", "suuralue")
  source_region = "kunta"
  hallintoaluekey <- data.frame()

  for(target_region in hallintoalueet) {

    url = "https://data.stat.fi/api/classifications/v2/correspondenceTables/"
    endpoint = paste(source_region, "_1_", as.character(year), "0101%23", target_region, "_1_", as.character(year), "0101/maps", sep = "")

    key <- as.data.frame(
      jsonlite::fromJSON(
        rawToChar(
          httr::GET(url = paste(url, endpoint, sep = ""),
                    query = list(content = "data", meta = "min"))$content
        )
      )
    )
    key <- data.frame(source_code = key$sourceItem$code,
                      target_code = key$targetItem$code,
                      source_name = unlist(lapply(key$sourceItem$classificationItemNames, '[', "name")),
                      target_name = unlist(lapply(key$targetItem$classificationItemNames, '[', "name")))

    key <- fix_encoding(key)
    assign(paste(target_region, "key", sep = "_"), key)

  }

  hallintoaluekey <- dplyr::left_join(seutukunta_key, maakunta_key, by = c("source_code", "source_name"))
  hallintoaluekey <- dplyr::left_join(hallintoaluekey, suuralue_key, by = c("source_code", "source_name"))

  if(codes) {
    output <- dplyr::select(hallintoaluekey, contains("code"))
    names(output) <- paste(c("kunta", hallintoalueet), "koodi", sep = "_")
  } else {
    output <- dplyr::select(hallintoaluekey, contains("name"))
    names(output) <- c("kunta", hallintoalueet)
  }
  output <- output[,as.double(alueet[alueet == region]) <= as.double(alueet)]
  output[!duplicated(output),]
}
