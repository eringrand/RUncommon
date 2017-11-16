#' @title Root Mean Squared Error
#' @param pred vector of predictions
#' @param actual vector of actual values
#' @export

rmse <- function(pred, actual) {
  return(sqrt(mean((pred - actual) ^ 2)))
}

#' Mean Absolute Percent Error
# @export
mape <- function(pred, actual) {
  mape <- sum(abs(pred - actual)) / sum(actual)
  return(mape)
}

#' This calculates the gini coeff
#' @export
#'
mygini <- function(pred, actual) {
  w <- rep(1, length(pred))
  v <- data.frame(o = pred, p = pred, a = actual, w = w)
  v <- v[order(v$o), ]
  v$cumm_w <- cumsum(v$w)
  v$cumm_y <- cumsum(v$w * v$a)
  total_w <- sum(v$w)
  gini <- with(v, 1 - 2 * sum(cumm_y * w) / (sum(a * w) * total_w))
  return(gini)
}

#' True Positive Rate
#' @export
#'
tpr <- function(pred, actual) {
  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 1 & smry_table$actual == 1]
  den <- sum(smry_table$Freq[smry_table$actual == 1])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' True Negative Rate
#' @export
tnr <- function(pred, actual) {
  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 0 & smry_table$actual == 0]
  den <- sum(smry_table$Freq[smry_table$actual == 0])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' False Positive Rate
#' @export
fpr <- function(pred, actual) {
  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 1 & smry_table$actual == 0]
  den <- sum(smry_table$Freq[smry_table$actual == 0])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' False Negative Rate
#' @export
fnr <- function(pred, actual) {
  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 0 & smry_table$actual == 1]
  den <- sum(smry_table$Freq[smry_table$actual == 1])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' @title Classification accuracy
#' @export
cls_acc <- function(pred, actual) {
  out <- sum(abs(pred - actual)) / length(actual)
  out <- round((1 - out) * 100, 2)
  # print(paste("The classification accuracy is ",out,"%.",sep=''))
  return(out)
}
