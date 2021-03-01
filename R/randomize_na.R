#' Imputes NAs by randomization
#'
#' @param df data.frame, the data containing the NAs to be imputed.
#' @param from A vector containing the possible imputed values. Defaults to \code{1:4}
#' @return data.frame Returns a data.frame with NAs in numeric columns imputed by randomization.
#' @export randomize_na

randomize_na <- function(df, from = 1:4) {
  num_cols <- names(df)[as.vector(sapply(df, class)) == "numeric"]
  for(col in num_cols) {
    new_values <- sample(from, sum(is.na(df[col])), replace = TRUE)
    df[[col]][is.na(df[[col]])] <- new_values
  }
  df
}
