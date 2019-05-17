length_error <- function(pred, actual) {
  if (length(pred) != length(actual))
    stop("pred and actual are not the same length")
}

#' @title Statiscs Functions
#' @param pred vector of predictions
#' @param actual vector of actual values
#' @describeIn stats root mean standard error
#' @export
rmse <- function(pred, actual) {
  length_error(pred, actual)

  rmse <- sqrt(sum((pred - actual)^2) / length(actual))
  return(rmse)
}

#' @describeIn stats Mean Absolute Percent Error
#' @export
mape <- function(pred, actual) {
  length_error(pred, actual)

  mape <- sum(abs(pred - actual)) / sum(actual)
  return(mape)
}


#' @describeIn stats gini coeff
#' @export
mygini <- function(pred, actual) {
  length_error(pred, actual)

  length_error(pred, actual)

  w <- rep(1, length(pred))
  v <- data.frame(o = pred, p = pred, a = actual, w = w)
  v <- v[order(v$o), ]
  v$cumm_w <- cumsum(v$w)
  v$cumm_y <- cumsum(v$w * v$a)
  total_w <- sum(v$w)
  gini <- with(v, 1 - 2 * sum(cumm_y * w) / (sum(a * w) * total_w))
  return(gini)
}

#' @describeIn stats True Positive Rate
#' @export
tpr <- function(pred, actual) {
  length_error(pred, actual)

  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 1 & smry_table$actual == 1]
  den <- sum(smry_table$Freq[smry_table$actual == 1])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' @describeIn stats  True Negative Rate
#' @export
tnr <- function(pred, actual) {
  length_error(pred, actual)

  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 0 & smry_table$actual == 0]
  den <- sum(smry_table$Freq[smry_table$actual == 0])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' @describeIn stats False Positive Rate
#' @export
fpr <- function(pred, actual) {
  length_error(pred, actual)

  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 1 & smry_table$actual == 0]
  den <- sum(smry_table$Freq[smry_table$actual == 0])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' @describeIn stats False Negative Rate
#' @export
fnr <- function(pred, actual) {
  length_error(pred, actual)

  smry_table <- data.frame(table(pred, actual))
  num <- smry_table$Freq[smry_table$pred == 0 & smry_table$actual == 1]
  den <- sum(smry_table$Freq[smry_table$actual == 1])
  out <- num / den
  if (length(out) == 0) {
    out <- NA
  }
  return(out)
}

#' @describeIn stats Classification accuracy
#' @export
cls_acc <- function(pred, actual) {
  length_error(pred, actual)

  out <- sum(abs(pred - actual)) / length(actual)
  return(out)
}
