#' Recovers NA-values in data
#'
#' Using information disaggregated at different levels recovers protected information: NAs
#' in cells that would otherwise have values 1-4. Options to impute NA-values that cannot
#' be perfectly recovered.
#'
#' Note that randomization of the values that cannot be accurately allocated does not
#' guarantee that the values of resulting disaggregate data will summ to the values
#' of the aggregate data. Even allocation does ensure this.
#'
#' @param disaggr_data data.frame, contains the data for which NAs are to be recovered.
#' @param aggr_data data.frame, contains the date on one aggregation level above. This data
#' contains the additional information that is used to recovers NAs.
#' @param disaggr_col character, is the name of the aggregation class in disaggr_data and aggr_data.
#' @param aggr_col character, is the name of the aggregation class in aggr_data and disaggr_data.
#' @param var character, is the name of the variable for which NAs are to be recovered.
#' @param only_accurate logical, if TRUE, only replaces NA-values that can be recovered accurately.
#' Otherwise see \code{randomize_rest}. Defaults to FALSE.
#' @param randomize_rest logical, if TRUE, randomizes a value from 1:4 for those NA-values that cannot
#' be accurately recovered. If FALSE, evenly allocates the missing values as inferred from \code{aggr_data}
#' to replace NA-values.
#' @return data.frame Output is a data.frame disaggr_data with NAs recovered.
#' @export

recover_na <- function(disaggr_data, aggr_data, disaggr_col, aggr_col, var,
                       only_accurate = FALSE, randomize_rest = FALSE) {

  # Save the names of the aggregate categories
     ylaluokitukset <- as.character(unlist(unique(aggr_data[aggr_col])))
     names(ylaluokitukset) <- NULL

  # Save all times
     times <- unique(aggr_data$time)

  # Initialize output data.frame
     outputdata <- data.frame()

  # Set counters to track what the functions does to the data
     counter_accurate <- 0
     counter_allocated <- 0
     counter_randomized <- 0

  # Report function progress to user, loop_length is the total number of iterations,
  # counter tracks the evolution of for-loop iteration
     counter <- 1
     loop_length <- length(ylaluokitukset)*length(times)
     print("Recovering NAs")
     pb <- txtProgressBar(min = 0, max = loop_length, style = 3)

  # Loop over each time point and aggregate category
     for(luokka in ylaluokitukset) {
       for(t in times) {
         # Update counter for progress bar
            counter <- counter + 1

         # Get the disaggregated data corresponding to the aggregate category
            data_temp <- dplyr::filter(disaggr_data, !!as.symbol(aggr_col) == luokka, time == t)
         # Record NAs and their number in the disaggregated data
            na_indicator <- is.na(data_temp[[var]])
            na_number <- sum(na_indicator)

         # If there are NAs, proceed, otherwise skip
            if(na_number > 0) {
              # If the number of NAs is more than 1, accurate recovery is not possible. If user has
              # set only_accurate = TRUE, skip, otherwise proceed
              if(only_accurate & na_number > 1) {next}

              # If also cases with more than one NA are to be dealt with, there are two options: either
              # each NA is randomized a value of 1,2,3 or 4, or the missing values are allocated evenly
              # to NAs. If user has set randomize_rest = TRUE, randomization follows:
                 if(randomize_rest & na_number > 1) {
                    replacements <- sample(1:4, na_number, replace = TRUE)
                    data_temp[var][na_indicator] <- replacements
                    counter_randomized <- counter_randomized + na_number
                    next
                 }
              # Else if randomize_rest is FALSE, remaining values are allocated evenly to NAs
              # x is the number of non-NA values in the disaggregate data
              # y is the total sum of values in the aggregate category in the aggregate data
                 x <- sum(data_temp[var], na.rm = TRUE)
                 y <- as.numeric(dplyr::filter(aggr_data, !!as.symbol(aggr_col) == luokka, time == t)[[var]])
              # y-x is the number of missing values, the sum of the protected NA values across
              # disaggregated categories, z is the even split and left is remainder
                 z <- floor((y - x) / na_number)
                 left <- (y-x)-z*na_number
              # replacements collects a vector that will replace the NAs
                 replacements <- rep(z, na_number)

              # The values remaining from the even split are allocated to NAs randomly one at
              # time
                 while(left != 0) {
                   # randomize the NA to which 1 of the remaining values is allocated. A value
                   # should not be allocated such that an observation gets five or more values
                   # since in this case it would not have been cencured in the first place.
                     ran <- sample((1:length(replacements))[replacements < 5], 1)
                     replacements[ran] <- replacements[ran] +1
                     left <- left-1
                 }
              # With the NA replacements determined replace the NAs
                  data_temp[[var]][na_indicator] <- replacements

              # Update the counters that tract what the function does
                  if(na_number > 1) {
                      counter_allocated <- counter_allocated + na_number
                  } else {
                      counter_accurate <- counter_accurate + na_number
                  }
           }
      # Add to output
      outputdata <- rbind(outputdata, data_temp)
    }
    # Update progress bar
       Sys.sleep(0.01)
       setTxtProgressBar(pb, counter)
  }
  # Close progress bar
     close(pb)
  # Let user know what the function did
     print(paste(as.character(counter_accurate), " NAs accurately recovered, ",
                 as.character(counter_allocated), " NAs allocated evenly, ",
                 as.character(counter_randomized), " NAs randomized out of ",
                 as.character(sum(is.na(disaggr_data[var]))), " NAs", sep = ""))
  # Return
     outputdata
}

