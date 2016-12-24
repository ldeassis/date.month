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

#' Calculate difference (in months) between 2 dates (mdy format)
#' @param d1 A date, in format mdy
#' @param d2 A date, in format mdy
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' months_mdy("12112016", "15122016")
#' @export
months_mdy <- function(d1, d2) {
  lubridate::interval(
    lubridate::mdy(d1),
    lubridate::mdy(d2)) %/% months(1)
}

#' Calculate difference (in months) between 2 dates (dmy format)
#' @param d1 A date, in format dmy
#' @param d2 A date, in format dmy
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' months_dmy("11122016", "12152016")
#' @export
months_dmy <- function(d1, d2) {
  lubridate::interval(
    lubridate::dmy(d1),
    lubridate::dmy(d2)) %/% months(1)
}

#' Calculate difference (in months) between 2 dates (ymd format)
#' @param d1 A date, in format ymd
#' @param d2 A date, in format ymd
#'
#' @return difference (in months) between d1 and d2
#' @examples
#' months_ymd("201161211", "20161215")
#'
#' @export
months_ymd <- function(d1, d2) {
  lubridate::interval(
    lubridate::ymd(d1),
    lubridate::ymd(d2)) %/% months(1)
}

#
#' Calculate difference (in months) between 2 dates (ymd format). it assumes that you are interestedin the begining of each month.
#' @param begin A date, in format ymd
#' @param end A date, in format ymd
#'
#' @return difference (in months) between begin and end
#' @examples
#' months_begin_ymd("201161211", "20161215")
#' @export
months_begin_ymd <-function (begin, end) {
  begin<-paste(substr(begin,1,6),"01",sep="")
  end<-paste(substr(end,1,6),"01",sep="")
  mob1<-lubridate::as.period(
    lubridate::interval(
      lubridate::ymd(begin),
      lubridate::ymd(end)))
  mob<-mob1@year*12+mob1@month
  mob
}

#
#' Calculate difference (in months) between 2 dates. it assumes that you are interestedin the REAL difference between each date
#'
#' @param begin A date, in format ymd
#' @param end A date, in format ymd
#'
#' @return difference (in months) between begin and end
#' @examples
#' mos_ymd("201161211", "20161215")
#' @export
mos_ymd<-function (begin, end) {
  mos1<-lubridate::as.period(
    lubridate::interval(
      lubridate::ymd(begin),
      lubridate::ymd(end)))
  mos<-mos1@year*12+mos1@month
  mos
}