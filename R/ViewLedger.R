#' @title Load, save, and view a ledger
#'
#' @name ViewLedger
#'
#' @import tidyverse
#'
#' @description These are the lowest level functions that have the default
#'   ledger locations for loading, saving, and viewing. Higher functions call
#'   these to perform these functions or by passing down the chain custom
#'   ledgers and file paths or by allowing these to use the defaults.
#'
#' @param ledger Data frame ledger. The ledger where each row is a transaction
#'   and with columns for \code{year}, \code{month}, \code{day},
#'   \code{description}, \code{budget}, and one column for each
#'   financial account. All columns should be numeric except for
#'   \code{description} and \code{budget}.
#' @param file The path for the ledger for loading and saving. Has an underlying
#'   default that needs to be changed in the \code{ViewLedger} file.
#' @param yr,mo,dy Year, month, day. When one of these is \code{NULL}, function
#'   behavior is to allow all possible values.
#' @param from,to Boundaries on the ledger dates. Requires Date class.
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
    ledger <- arrange(ledger, ID)
    write.csv(ledger, file = file, row.names = FALSE, quote = FALSE)
}

#' @rdname ViewLedger
#' @export
viewLedger <- function(ledger = read.csv(file), file = viewLedgerFile(),
                       from = as.Date("1900-01-01"), to = Sys.Date(), on = NULL,
                       descriptions = NULL, budgets = NULL, amount.ops = NULL, amounts = NULL, accounts = NULL,
                       suppress = TRUE) {

    ledger$description <- as.character(ledger$description)
    ledger$date <- as.Date(ledger$date)

    ledger.mini <- ledger
    if (!is.null(on))   ledger.mini <- filter(ledger.mini, date == on)
    if (!is.null(from)) ledger.mini <- filter(ledger.mini, from <= date)
    if (!is.null(to))   ledger.mini <- filter(ledger.mini, date <= to)

    if (!is.null(descriptions)) {
        valid.transactions <- logical(nrow(ledger.mini))
        for (i in seq(descriptions)) valid.transactions <- valid.transactions | regexec(descriptions[i], ledger.mini$description, ignore.case = TRUE) > -1
        ledger.mini <- ledger.mini[valid.transactions, ]
    }
    if (!is.null(budgets)) {
        ledger.mini <- filter(ledger.mini, budget %in% budgets)
    }
    if (!is.null(amounts)) {
        stopifnot(length(amounts) == length(amount.ops))
        valid.rows <- logical(nrow(ledger.mini))
        for (i in 1:length(amounts)) {
            amount.ops.ind <- ifelse(length(amount.ops) > 1, amount.ops[[i]], amount.ops)
            valid.transactions <- if (is.character(amount.ops.ind)) {
                eval(call(amount.ops.ind, ledger.mini[, viewAccountCategories(ledger.mini)], amounts[i]))
            } else {
                amount.ops.ind(ledger.mini$amount, amounts[i])
            }
            valid.rows <- valid.rows | valid.transactions
        }
        ledger.mini <- ledger.mini[valid.rows, ]
    }
    if (!is.null(accounts)) {
        ledger.mini <- filter(ledger.mini, account %in% accounts)
        ledger.mini$account <- factor(ledger.mini$account, levels = accounts)
    }
    if (nrow(ledger.mini) == 0) stop("No transactions found for the criteria specified. Remember that from, to, and on all apply.")
    if (!suppress) print(ledger.mini)
    return(ledger.mini)
}

#' @rdname ViewLedger
#' @export
viewLedgerFile <- function(file = NULL, suppress = TRUE) {
    if (is.null(file)) load(.dataLocation())
    if (!suppress) print(file)
    return(file)
}

#' @export
.dataLocation <- function() {
    file.path(path.finances(), "Data", "finances.Rdata")
}
