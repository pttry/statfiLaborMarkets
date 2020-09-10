#' Recovers NA-values in data
#'
#' Using information disaggregated at different levels recovers protected information: NAs
#' in cells that would otherwise have values 1-4. Options to impute NA-values that cannot
#' be perfectly recovered.
#'
#' @param alaluokkadata data.frame, contains the data for which NAs are to be recovered.
#' @param ylaluokkadata data.frame, contains the date on one aggregation level above. This data
#' contains the additional information that is used to recovers NAs.
#' @param alaluokka character, is the name of the aggregation class in alaluokkadata and ylaluokkadata.
#' @param ylaluokka character, is the name of the aggregation class in ylaluokkadata and alaluokkadata.
#' @param muuttuja character, is the name of the variable for which NAs are to be recovered.
#' @param onlyAccurate logical, if TRUE, only replaces NA-values that can be recovered accurately.
#' Otherwise see \code{randomizeRest}. Defaults to FALSE.
#' @param randomizeRest logical, if TRUE, randomizes a value from 1:4 for those NA-values that cannot
#' be accurately recovered. If FALSE, evenly allocates the missing values as inferred from \code{ylaluokkadata}
#' to replace NA-values.
#' @return data.frame Output is a data.frame alaluokkadata with NAs recovered.
#' @export

recover_na <- function(alaluokkadata, ylaluokkadata, alaluokka, ylaluokka, muuttuja,
                       onlyAccurate = FALSE, randomizeRest = FALSE) {

  ylaluokitukset <- unlist(unique(ylaluokkadata[ylaluokka]))
  names(ylaluokitukset) <- NULL
  ajat <- unique(ylaluokkadata$time)
  outputdata <- data.frame()
  counter <- 1
  counter_accurate <- 0
  counter_allocated <- 0
  counter_randomized <- 0
  loop_length <- length(ylaluokitukset)*length(ajat)
  print("Recovering NAs")
  pb <- txtProgressBar(min = 0, max = loop_length, style = 3)
  for(luokka in ylaluokitukset) {
    for(t in ajat) {
      counter <- counter + 1
      data_temp <- dplyr::filter(alaluokkadata, !!as.symbol(ylaluokka) == luokka, time == t)
      na_indicator <- is.na(data_temp[muuttuja])
      na_number <- sum(na_indicator)
      if(na_number > 0) {
        if(onlyAccurate & na_number > 1) {next}
        if(randomizeRest & na_number > 1) {
          replacements <- sample(1:4, na_number, replace = TRUE)
          data_temp[muuttuja][na_indicator] <- replacements
          counter_randomized <- counter_randomized + na_number
          next
        }
        x <- sum(data_temp[muuttuja], na.rm = TRUE)
        y <- as.numeric(dplyr::filter(ylaluokkadata, !!as.symbol(ylaluokka) == luokka, time == t)[muuttuja])
        z <- floor((y - x) / na_number)
        left <- (y-x)-z*na_number
        replacements <- rep(z, na_number)
        while(left != 0) {
          ran <- sample((1:length(replacements))[replacements < 5], 1)
          replacements[ran] <- replacements[ran] +1
          left <- left-1
        }
        data_temp[muuttuja][na_indicator] <- replacements
        if(na_number > 1) {
          counter_allocated <- counter_allocated + na_number
        } else {
          counter_accurate <- counter_accurate + na_number
        }
      }
      outputdata <- rbind(outputdata, data_temp)
    }
    Sys.sleep(0.01)
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  print(paste(as.character(counter_accurate), " NAs accurately recovered, ",
              as.character(counter_allocated), " NAs allocated evenly, ",
              as.character(counter_randomized), " NAs randomized out of ",
              as.character(sum(is.na(alaluokkadata[muuttuja]))), " NAs", sep = ""))
  outputdata
}

