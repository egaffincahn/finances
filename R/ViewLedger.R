#' @title Load, save, and view a ledger
#'
#' @name ViewLedger
#'
#' @import dplyr
#' @importFrom tidyr gather
#'
#' @description These are the lowest level functions that have the default
#'   ledger locations for loading, saving, and viewing. Higher functions call
#'   these to perform these functions or by passing down the chain custom
#'   ledgers and file paths or by allowing these to use the defaults.
#'
#' @param ledger Data frame ledger. The ledger where each row is a transaction
#'   and with columns for \code{year}, \code{month}, \code{day},
#'   \code{description}, \code{budget.category}, and one column for each
#'   financial account. All columns should be numeric except for
#'   \code{description} and \code{budget.category}.
#' @param file The path for the ledger for loading and saving. Has an underlying
#'   default that needs to be changed in the \code{ViewLedger} file.
#' @param yr,mo,dy Year, month, day. When one of these is \code{NULL}, function
#'   behavior is to allow all possible values.
#' @param from,to Boundaries on the ledger dates. Requires Date class.
#' @param trunc removes the account columns which has all values == 0.
#' @param suppress Prevents it from printing to the console. All low-level
#'   functions default to \code{TRUE}, while high-level functions that do not
#'   have callers and exist for the sole purpose of printing out information
#'   default to \code{FALSE}.
#'
#' @details \code{viewLedger} does the loading of the ledger and is called by
#'   many other functions. Can be used to manually manipulate by calling, and to
#'   view it by calling with \code{suppress = FALSE}. Therefore, calling
#'   \code{viewLedger} with \code{suppress = FALSE} is not recommended, as it
#'   will print out a large dataset to the console.
#'
#' @return \code{viewLedger} and each of its date components return a data frame
#'   ledger. \code{viewLedgerFile} returns the character path to the ledger.
#'

#' @rdname ViewLedger
#' @export
saveLedger <- function(ledger, file = viewLedgerFile()) {
    ledger <- arrange(ledger, year, month, day)
    write.csv(ledger, file = file, row.names = FALSE, quote = FALSE)
}

#' @rdname ViewLedger
#' @export
viewLedger <- function(ledger = read.csv(file), file = viewLedgerFile(), suppress = TRUE, trunc = FALSE) {
    ledger$description <- as.character(ledger$description)
    if (trunc) ledger <- truncateLedger(ledger)
    if (!suppress) print(ledger)
    return(ledger)
}

#' @rdname ViewLedger
#' @export
viewLedgerFile <- function(file = NULL, suppress = TRUE) {
    if (is.null(file)) load(.dataLocation())
    if (!suppress) print(file)
    return(file)
}

#' @export
truncateLedger <- function(ledger) {
    account.cells <- select(ledger, one_of(viewAccountCategories()))
    blank.accounts <- viewAccountCategories()[apply(account.cells, 2, function(x) all(x == 0))]
    ledger <- select(ledger, -one_of(blank.accounts))
}

#' @export
.dataLocation <- function() {
    file.path(path.finances(), "Data", "finances.Rdata")
}