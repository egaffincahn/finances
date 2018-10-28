#' @title Get date info
#'
#' @importFrom Hmisc monthDays
#'
#' @description The \code{current} series gets today's year, month, or day,
#'   returning just an integer. The \code{last} series (including
#'   \code{yesterday}) returns a full \code{Date} class for a recent date for
#'   easily providing values to the \code{from} input argument in other
#'   functions. For custom dates, use \code{componentsToDate}. To return today's
#'   date in the proper format, call \code{today}, which is a wrapper for
#'   \code{componentsToDate}.
#'
#' @return Each of the \code{current*} functions return an integer. \code{last*}
#'   and \code{componentsToDate} return a \code{Date} class.
#'
#' @name ViewDate
#'

#' @export
#' @rdname ViewDate
currentDay <- function() {
    as.numeric(format(Sys.Date(), "%d"))
}

#' @export
#' @rdname ViewDate
currentMonth <- function() {
    as.numeric(format(Sys.Date(), "%m"))
}

#' @export
#' @rdname ViewDate
currentYear <- function() {
    as.numeric(format(Sys.Date(), "%Y"))
}

#' @export
#' @rdname ViewDate
today <- function() {
    componentsToDate()
}

#' @export
#' @rdname ViewDate
yesterday <- function() {
    .wrapDate(currentYear(), currentMonth(), currentDay() - 1)
}

#' @export
#' @rdname ViewDate
lastWeek <- function() {
    .wrapDate(currentYear(), currentMonth(), currentDay() - 7)
}

#' @export
#' @rdname ViewDate
lastMonth <- function() {
    .wrapDate(currentYear(), currentMonth() - 1, currentDay())
}

#' @export
#' @rdname ViewDate
lastYear <- function() {
    .wrapDate(currentYear() - 1, currentMonth(), currentDay())
}

#' @export
#' @rdname ViewDate
componentsToDate <- function(yr = currentYear(), mo = currentMonth(), dy = currentDay()) {
    as.Date(paste(yr, mo, dy, sep = "-"), format = "%Y-%m-%d")
}

#' @export
.wrapDate <- function(year, month, day) {
    while (month < 1) {
        month <- month + 12
        year <- year - 1
    }
    while (month > 12) {
        month <- month - 12
        year <- year + 1
    }
    while (day < 1) {
        month <- month - 1
        while (month < 1) {
            month <- month + 12
            year <- year - 1
        }
        day <- day + monthDays(componentsToDate(mo = month))
    }
    while (day > monthDays(componentsToDate(mo = month))) {
        day <- day - monthDays(componentsToDate(mo = month))
        month <- month + 1
        while (month > 12) {
            month <- month - 12
            year <- year + 1
        }
    }
    componentsToDate(year, month, day)
}
