# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#
#' Calculate difference (in months) between 2 dates
#' @param d1 A date, in format mdy
#' @param d2 A date, in format mdy
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' Months_mdy("12112016", "15122016")
#' @export
Months_mdy <- function(d1, d2) {
  lubridate::interval(
    lubridate::mdy(d1),
    lubridate::mdy(d2)) %/% months(1)
}

#
#' Calculate difference (in months) between 2 dates
#' @param d1 A date, in format dmy
#' @param d2 A date, in format dmy
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' Months_dmy("11122016", "12152016")
#' @export
Months_dmy <- function(d1, d2) {
  lubridate::interval(
    lubridate::dmy(d1),
    lubridate::dmy(d2)) %/% months(1)
}

#
#' Calculate difference (in months) between 2 dates
#' @param d1 A date, in format ymd
#' @param d2 A date, in format ymd
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' Months_ymd("201161211", "20161215")
#' @export
Months_ymd <- function(d1, d2) {
  lubridate::interval(
    lubridate::ymd(d1),
    lubridate::ymd(d2)) %/% months(1)
}
