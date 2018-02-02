#' @title Add, edit or view transactions
#'
#' @name AddTransaction
#'
#' @description Add a transaction to the ledger manually via inputs to the
#'   console with \code{addTransactionManual}, or automatically by reading from
#'   a file with \code{addTransactionAuto}. You can see what the transactions
#'   are that would be automatically entered with
#'   \code{viewTransactionsAutoLoad}.\cr \code{editTransaction} will display the
#'   transactions for the given year, month, day. It defaults to show the
#'   current month's. If \code{yr}, \code{mo}, \code{dy} is \code{NULL}, it will
#'   show transactions from all years, months, days, respectively, within the
#'   others. It also takes other arguments to narrow the options to show. These
#'   options are the same as for \code{viewTransactions}, because
#'   \code{editTransaction} will call the former to limit the options shown. \cr
#'   \code{viewTransactions} searches for transactions based on the provided
#'   criteria which can be passed as arguments or queries in the console. If
#'   user gives any criteria, then by default, won't query the user for
#'   additional criteria. \code{viewTransactionsDate} is a convenient wrapper to
#'   show the transactions for just the current month.Instead, the functions
#'   with date subdivisions are more appropriate. This is the appropriate way to
#'   view a reasonable number of transactions at one time. It takes the
#'   \code{year}, \code{month}, and \code{day} to prune down the ledger. If any
#'   date specification is \code{NULL}, it allows all values for that specific
#'   date column. \code{year} and \code{month} default to \code{currentYear()}
#'   and \code{currentMonth()}, while \code{day} defaults to all values via
#'   \code{NULL}. The ledger can further be restricted by including an early or
#'   late bound with \code{from} and \code{to}. These require a \code{POSIXct}
#'   type. You can use \code{componentsToDate} to build one using a year, month,
#'   and day. Both methods can be done alone or in tandem.\cr For a transaction
#'   to be shown, it has to fit ALL of the input argument criteria. However,
#'   each of the input arguments can be a single value or can be a vector. If
#'   one is a vector, a transaction that fits any of the values in the vector
#'   will be shown.
#'
#'
#' @inheritParams viewLedger
#' @param descriptions Values in the descriptions column to search for. Uses
#'   regular expressions.
#' @param budget.categories Values from the factor column of budget category to
#'   search for. Must be exact.
#' @param amount.ops,account.amounts Operators to search transactions. For
#'   example, to find transactions where more than one thousand dollars were
#'   spent, use \code{amount.ops = `<`, account.amounts = -1000}.
#' @param account.categories Restricts the transactions shown to ones where the
#'   account(s) in this argument are not 0.
#' @param query Asks the user for descriptions, transaction amounts, etc.
#'   manually. Queried values will overwrite the input arguments. Default
#'   behavior is to query if all the input options are \code{NULL}.
#'
#'

#' @rdname AddTransaction
#' @export
addTransactionManual <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), dy = currentDay()) {
    ledger <- add_row(ledger)
    n <- nrow(ledger)
    ledger[n, viewAccountCategories(ledger)] <- 0
    ledger <- ledger %>%
        .addTransactionYear(yr = yr, index = n) %>%
        .addTransactionMonth(mo = mo, index = n) %>%
        .addTransactionDay(dy = dy, index = n) %>%
        .addTransactionDescription(index = n) %>%
        .addTransactionBudgetCategory(index = n) %>%
        .addTransactionAccountCategory(index = n)
    saveLedger(ledger, file)
}

#' @rdname AddTransaction
#' @export
addTransactionAuto <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    load(.dataLocation())
    transactions <- viewTransactionsAutoLoad()
    for (i in 1:nrow(transactions)) {
        ledger <- add_row(ledger)
        n <- nrow(ledger)
        ledger[n, viewAccountCategories(ledger)] <- 0
        ledger <- ledger %>%
            .addTransactionYear(yr = transactions[i, 1], index = n) %>%
            .addTransactionMonth(mo = transactions[i, 2], index = n) %>%
            .addTransactionDay(dy = transactions[i, 3], index = n) %>%
            .addTransactionDescription(description = transactions[i, 4], index = n) %>%
            .addTransactionBudgetCategory(budget.category = transactions[i, 5], index = n) %>%
            .addTransactionAccountCategory(account.category = paste("acct.", transactions[i, 6], sep = ""), account.amount = transactions[i, 7], index = n)
        print("Wrote charge:")
        viewLedger(ledger[nrow(ledger), ], suppress = FALSE, trunc = TRUE)
    }
    write(data.frame(), file = auto.add.transaction, ncolumns = 1)
    saveLedger(ledger, file)
}

#' @rdname AddTransaction
#' @export
viewTransactionsAutoLoad <- function(suppress = TRUE) {
    load(.dataLocation())
    transactions <- read.csv(auto.add.transaction, header = FALSE, stringsAsFactors = FALSE) # try catch?
    for (i in 1:ncol(transactions)) if (is.character(transactions[, i])) transactions[, i] <- trimws(transactions[, i])
    return(transactions)
}

#' @rdname AddTransaction
#' @export
editTransaction <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), dy = NULL, from = NULL, to = NULL,
                            descriptions = NULL, budget.categories = NULL, amount.ops = `==`, account.amounts = NULL, account.categories = NULL, suppress = TRUE, trunc = TRUE) {

    ledger$index <- 1:nrow(ledger)
    ledger.mini <- viewTransactions(ledger = ledger, yr = yr, mo = mo, dy = dy, from = from, to = to, descriptions = descriptions,
                                    budget.categories = budget.categories, amount.ops = amount.ops, account.amounts = account.amounts,
                                    account.categories = account.categories, query = FALSE, trunc = FALSE)

    ledger.mini.viewing <- select(ledger.mini, -index)
    rownames(ledger.mini.viewing) <- seq(nrow(ledger.mini.viewing))
    viewLedger(ledger.mini.viewing, suppress = FALSE, trunc = TRUE)
    mini.index <- 0
    transaction.index <- NA
    while (!is.na(mini.index) || (is.na(transaction.index))) {
        mini.index <- as.numeric(readline("Enter transaction number to edit: Leave blank when correct. "))
        if (!is.na(mini.index)) {
            transaction.index <- ledger.mini[mini.index, ]$index
            viewLedger(select(filter(ledger, index == transaction.index), -index), suppress = FALSE, trunc = TRUE)
        }
    }
    change.fields.allowed <- c("date", "description", "budget category", "account category")
    print(change.fields.allowed)
    change.field <- "-" # placeholder that's not empty
    while (change.field != "") {
        change.field <- readline("Which field to change: Or enter delete. ")
        if (change.field == "delete") {
            ledger <- filter(ledger, index != transaction.index)
            break()
        } else if (change.field == "date") {
            ledger <- ledger %>%
                .addTransactionYear(index = transaction.index) %>%
                .addTransactionMonth(index = transaction.index) %>%
                .addTransactionDay(index = transaction.index)
        } else if (change.field == "description") {
            ledger <- .addTransactionDescription(ledger, index = transaction.index)
        } else if (change.field == "budget category") {
            ledger <- .addTransactionBudgetCategory(ledger, index = transaction.index)
        } else if (change.field == "account category") {
            ledger <- .addTransactionAccountCategory(ledger, index = transaction.index)
        } else if (change.field != "") {
            print(paste("Unknown field to change:", change.field))
        }
    }
    saveLedger(select(ledger, -index))
}

#' @export
.addTransactionYear <- function(ledger, yr = readline("Enter year or leave blank for current: "), index) {
    ledger$year[index] <- ifelse(yr == "", currentYear(), yr)
    return(ledger)
}

#' @export
.addTransactionMonth <- function(ledger, mo = readline("Enter month or leave blank for current: "), index) {
    ledger$month[index] <- ifelse(mo == "", currentMonth(), mo)
    return(ledger)
}

#' @export
.addTransactionDay <- function(ledger, dy = readline("Enter day or leave blank for current: "), index) {
    ledger$day[index] <- ifelse(dy == "", currentDay(), dy)
    return(ledger)
}

#' @export
.addTransactionDescription <- function(ledger, description = readline("Enter description: "), index) {
    ledger$description[index] <- description
    return(ledger)
}

#' @export
.addTransactionBudgetCategory <- function(ledger, budget.category = NULL, index) {
    if (is.null(budget.category)) {
        viewBudgetCategoriesAll(suppress = FALSE)
        budget.category <- readline("Enter budget category from above or its index: ")
        if (suppressWarnings(!is.na(as.numeric(budget.category)))) budget.category <- viewBudgetCategoriesAll()[as.numeric(budget.category)]
    }
    if (budget.category %in% levels(ledger$budget.category)) {
        ledger$budget.category[index] <- budget.category
    } else {
        budget.category.temp <- readline(paste(budget.category, "is not a current budget category. Enter a new one or leave blank to add. "))
        ledger$budget.category[index] <- ifelse(budget.category.temp == "", budget.category, budget.category.temp)
    }
    return(ledger)
}

#' @export
.addTransactionAccountCategory <- function(ledger, account.category = NULL, account.amount = NULL, index) {
    if (!is.null(account.category) && !is.null(account.amount)) {
        account.index <- which(colnames(ledger) == account.category)
        ledger[index, account.index] <- account.amount
        return(ledger)
    }
    viewAccountCategories(suppress = FALSE)
    while (is.null(account.category) || account.category != "") {
        account.category <- readline("Enter account category from above (cannot leave out <acct.>) or its index: ")
        if (suppressWarnings(!is.na(as.numeric(account.category)))) account.category <- viewAccountCategories()[as.numeric(account.category)]
        account.index <- which(colnames(ledger) == account.category)
        if (length(account.index) == 0) next()
        account.amount <- as.numeric(readline(paste("Enter amount for ", sub("^acct.", "", account.category), ": ", sep = "")))
        ledger[index, account.index] <- account.amount
    }
    return(ledger)
}

#' @rdname AddTransaction
#' @export
viewTransactionsDate <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), dy = NULL) {
    viewTransactions(ledger = ledger, yr = yr, mo = mo, dy = dy)
}

#' @rdname AddTransaction
#' @export
viewTransactions <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = NULL, mo = NULL, dy = NULL, from = NULL, to = componentsToDate(),
                             descriptions = NULL, budget.categories = NULL, amount.ops = `==`, account.amounts = NULL, account.categories = NULL,
                             query = is.null(yr) && is.null(mo) && is.null(dy) && is.null(from) && is.null(to) &&
                                 is.null(descriptions) && is.null(budget.categories) && is.null(account.amounts) && is.null(account.categories),
                             suppress = TRUE, trunc = TRUE) {
    if (query) {
        yr <- trimws(strsplit(readline("Enter comma-separated years to search for (treated as OR) or leave blank for any: "), ",")[[1]])
        if (identical(yr, character(0))) yr <- NULL
        mo <- trimws(strsplit(readline("Enter comma-separated months to search for (treated as OR) or leave blank for any: "), ",")[[1]])
        if (identical(mo, character(0))) mo <- NULL
        dy <- trimws(strsplit(readline("Enter comma-separated days to search for (treated as OR) or leave blank for any: "), ",")[[1]])
        if (identical(dy, character(0))) dy <- NULL

        descriptions <- trimws(strsplit(readline("Enter comma-separated descriptions to search for (treated as OR) or leave blank for any: "), ",")[[1]])
        if (identical(descriptions, character(0))) descriptions <- NULL

        viewBudgetCategoriesAll()
        budget.categories <- trimws(strsplit(readline("Enter comma-separated budget categories (treated as OR) from above or leave blank for any: "), ",")[[1]])
        if (identical(budget.categories, character(0))) budget.categories <- NULL

        operations <- trimws(strsplit(readline("Enter comma-separated operators and amounts (treated as OR) to compare to transaction amounts. \nExample: '<= 100, > 0'"), ",")[[1]])
        if (identical(operations, character(0))) {
            account.amounts <- NULL
        } else {
            matches <- gregexpr("[[:punct:]]+", operations)
            amount.ops <- sapply(seq(matches), function(x) substr(operations[x], as.numeric(matches[[x]]), as.numeric(matches[[x]]) + attr(matches[[x]], "match.length") - 1))
            matches <- gregexpr("[[:digit:]]+", operations)
            account.amounts <- sapply(seq(matches), function(x) substr(operations[x], as.numeric(matches[[x]]), as.numeric(matches[[x]]) + attr(matches[[x]], "match.length") - 1))
        }

        viewAccountCategories(suppress = FALSE)
        account.categories <- trimws(strsplit(readline("Enter comma-separated acount categories (with <acct.>; treated as OR) from above or leave blank for any: "), ",")[[1]])
        if (identical(account.categories, character(0))) account.categories <- NULL
    }

    ledger.mini <- ledger
    if (!is.null(dy))   ledger.mini <- filter(ledger.mini, day %in% dy)
    if (!is.null(mo))   ledger.mini <- filter(ledger.mini, month %in% mo)
    if (!is.null(yr))   ledger.mini <- filter(ledger.mini, year %in% yr)
    if (!is.null(from)) ledger.mini <- filter(ledger.mini, from <= componentsToDate(year, month, day))
    if (!is.null(to))   ledger.mini <- filter(ledger.mini, to >= componentsToDate(year, month, day))

    if (!is.null(descriptions)) {
        valid.transactions <- logical(nrow(ledger.mini))
        for (i in seq(descriptions)) valid.transactions <- valid.transactions | regexec(descriptions[i], ledger.mini$description, ignore.case = TRUE) > -1
        ledger.mini <- ledger.mini[valid.transactions, ]
    }
    if (!is.null(budget.categories)) {
        ledger.mini <- ledger.mini[ledger.mini$budget.category %in% budget.categories, ]
    }
    if (!is.null(account.amounts)) {
        stopifnot(length(account.amounts) == length(amount.ops))
        valid.rows <- logical(nrow(ledger.mini))
        for (i in 1:length(account.amounts)) {
            amount.ops.ind <- ifelse(length(amount.ops) > 1, amount.ops[[i]], amount.ops)
            valid.transactions <- if (is.character(amount.ops.ind)) {
                eval(call(amount.ops.ind, ledger.mini[, viewAccountCategories(ledger.mini)], account.amounts[i]))
            } else {
                amount.ops.ind(ledger.mini[, viewAccountCategories(ledger.mini)], account.amounts[i])
            }
            valid.rows <- valid.rows | apply(valid.transactions, 1, any)
        }
        ledger.mini <- ledger.mini[valid.rows, ]
    }
    if (!is.null(account.categories)) {
        ledger.mini <- ledger.mini[apply(select(ledger.mini, one_of(account.categories)), 1, function(x) any(x != 0)), ]
    }
    if (trunc) ledger.mini <- truncateLedger(ledger.mini)
    if (nrow(ledger.mini) == 0) stop("No transactions found for the criteria specified. Remember that yr/mo default to current.")
    if (!suppress) print(ledger.mini)
    return(ledger.mini)
}

#' @rdname AddTransaction
#' @export
viewDuplicates <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), ..., suppress = TRUE, trunc = TRUE) {
    ledger <- viewTransactions(ledger, ..., suppress = suppress, trunc = trunc)
    duplicates <- as.data.frame(matrix(numeric(), ncol = ncol(ledger)))
    colnames(duplicates) <- colnames(ledger)
    dates <- componentsToDate(ledger$year, ledger$month, ledger$day)
    for (i in 1:(nrow(ledger)-1)) {
        sameDates <- which(dates[i] == dates)
        for (j in sameDates[sameDates > i]) {
            if (all(ledger[i, ] == ledger[j, ])) {
                duplicates[nrow(duplicates)+1, ] <- ledger[i, ]
                duplicates[nrow(duplicates)+1, ] <- ledger[j, ]
            }
        }
    }
    if (trunc) duplicates <- truncateLedger(duplicates)
    if (!suppress) print(duplicates)
    return(duplicates)
}
