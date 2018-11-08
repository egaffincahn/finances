#' @title Add, edit or view transactions
#'
#' @name AddTransaction
#'
#' @import tidyverse
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
#'   options are the same as for \code{viewLedger}, because
#'   \code{editTransaction} will call the former to limit the options shown. \cr
#'   \code{viewLedgerMonth} is a convenient wrapper to show the transactions for
#'   just the current month. Instead, the functions with date subdivisions are
#'   more appropriate. This is the appropriate way to view a reasonable number
#'   of transactions at one time. It takes the \code{year}, \code{month}, and
#'   \code{day} to prune down the ledger. If any date specification is
#'   \code{NULL}, it allows all values for that specific date column.
#'   \code{year} and \code{month} default to \code{currentYear()} and
#'   \code{currentMonth()}, while \code{day} defaults to all values via
#'   \code{NULL}. The ledger can further be restricted by including an early or
#'   late bound with \code{from} and \code{to}. These require a \code{Date}
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
addTransactionManual <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), description = NULL,
                                 budget = NULL, date = NULL, account = NULL, amount = NULL) {
    new.rows <- ledger[1,]
    new.rows$ID <- NA
    new.rows$description <- ifelse(is.null(description), .addTransactionDescription(), description)
    new.rows$date <- if (is.null(date)) {
        as.Date(paste(.addTransactionYear(), .addTransactionMonth(), .addTransactionDay(), sep = "-"))
    } else {
        date
    }
    new.rows$budget <- ifelse(is.null(budget), .addTransactionBudgetCategory(), budget)
    account.amounts <- if (is.null(account) || is.null(amount)) {
        .addTransactionAccountCategory(account = account, amount = amount)
    } else {
        data.frame(account = account, amount = amount)
    }
    for (i in 1:nrow(account.amounts)) {
        old.levels <- levels(new.rows$account)
        new.levels <- levels(account.amounts$account)
        if (any(!(new.levels %in% old.levels))) {
            new.rows$account <- factor(new.rows$account, levels = c(old.levels, new.levels[!(new.levels %in% old.levels)]))
        }
        new.rows$account[i] <- account.amounts$account[i]
        new.rows$amount[i] <- account.amounts$amount[i]
        if (i != nrow(account.amounts)) {
            new.rows <- rbind(new.rows, new.rows[1,])
        }
    }
    ledger <- .shiftIDs(ledger, new.rows)
    saveLedger(ledger, file)
}

#' @rdname AddTransaction
#' @export
addTransactionAuto <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    load(.dataLocation())
    transactions <- viewTransactionsAutoLoad()
    new.rows <- ledger[1,]
    new.rows$ID <- NA
    for (i in 1:nrow(transactions)) {
        new.rows$date[i] <- as.Date(transactions$V1[i], format = "%Y-%m-%d")
        new.rows$description[i] <- transactions$V2[i]
        new.rows$budget[i] <- transactions$V3[i]
        new.rows$account[i] <- transactions$V4[i]
        new.rows$amount[i] <- transactions$V5[i]
        if (i != nrow(transactions)) {
            new.rows <- rbind(new.rows, new.rows[1,])
        }
    }
    id <- 0
    ids <- numeric(nrow(new.rows))
    new.rows.mini <- select(new.rows, description, budget, date)
    for (i in 1:nrow(new.rows)) {
        if (i == 1 || !all(sapply(1:ncol(new.rows.mini), function(x) new.rows.mini[i,x] == new.rows.mini[i-1,x]))) id <- id + 1
        ids[i] <- id
    }
    for (i in seq(unique(ids))) {
        ledger <- .shiftIDs(ledger, filter(new.rows, ids == i))
        ledger <- arrange(ledger, ID)
    }
    write(data.frame(), file = auto.add.transaction, ncolumns = 1)
    saveLedger(ledger, file)
}

#' @rdname AddTransaction
#' @export
viewTransactionsAutoLoad <- function(suppress = TRUE) {
    load(.dataLocation())
    transactions <- read.csv(auto.add.transaction, header = FALSE, stringsAsFactors = FALSE) # try catch?
    return(transactions)
}

#' @rdname AddTransaction
#' @export
editTransaction <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), ...) {
    ledger.mini <- viewLedger(ledger = ledger, ..., suppress = FALSE)
    id <- NA
    while (is.na(id)) id <- as.numeric(readline("Enter transaction ID to edit: "))
    ledger.mini <- filter(ledger.mini, ID == id)
    print(ledger.mini)

    change.fields.allowed <- c("date", "description", "budget", "account")
    print(change.fields.allowed)
    change.field <- "placeholder that's not empty"
    while (change.field != "") {
        change.field <- readline("Which field to change: Or enter delete. ")
        if (change.field == "delete") {
            ledger <- filter(ledger, ID != id)
            ledger <- .shiftIDs(ledger, new.rows = NULL)
            break()
        } else if (change.field == "date") {
            new.rows <- filter(ledger, ID == id)
            new.rows$ID <- NA
            new.rows$date <- as.Date(paste(.addTransactionYear(), .addTransactionMonth(), .addTransactionDay(), sep = "-"))
            ledger <- filter(ledger, ID != id)
            ledger <- .shiftIDs(ledger, NULL)
            ledger <- .shiftIDs(ledger, new.rows)
        } else if (change.field == "description") {
            ledger$description[ledger$ID == id] <- readline("Enter description: ")
        } else if (change.field == "budget") {
            ledger$budget[ledger$ID == id] <- .addTransactionBudgetCategory()
        } else if (change.field == "account") {
            print("Warning: this resets all the account charges from this transaction ID.")
            account.amounts <- .addTransactionAccountCategory()
            tempRow <- ledger[which(ledger$ID == id)[1], ]
            ledger <- filter(ledger, ID != id)
            for (i in 1:nrow(account.amounts)) {
                tempRow$account <- account.amounts$account[i]
                tempRow$amount <- account.amounts$amount[i]
                ledger <- rbind(ledger, tempRow)
            }
            ledger <- arrange(ledger, ID)
        } else if (change.field != "") {
            print(paste("Unknown field to change:", change.field))
        }
    }
    saveLedger(ledger, file)
}

#' @export
.shiftIDs <- function(ledger, new.rows) {
    if (is.null(new.rows)) { # shifting from deleted transaction
        startRow <- (which(diff(ledger$ID) == 2)+1)
        if (length(startRow) == 0) return(ledger) # transaction was last one in ledger
        shiftRows <- startRow:nrow(ledger)
        ledger$ID[shiftRows] <- ledger$ID[shiftRows] - 1
        return(ledger)
    }
    # assumes only one new transaction (one transaction ID, all same date, etc)
    startRow <- which(ledger$date <= new.rows$date[1]) %>% {.[length(.)]} + 1
    if (startRow > nrow(ledger)) {
        ledger <- rbind(ledger, new.rows)
        ledger$ID[startRow:(startRow+nrow(new.rows)-1)] <- ledger$ID[startRow-1] + 1
    } else {
        ledger <- rbind(ledger[1:(startRow-1),], new.rows, ledger[startRow:nrow(ledger),])
        ledger$ID[startRow:(startRow+nrow(new.rows)-1)] <- ledger$ID[startRow+nrow(new.rows)] # ID of new transaction
        ledger$ID[(startRow+nrow(new.rows)):nrow(ledger)] <- ledger$ID[(startRow+nrow(new.rows)):nrow(ledger)] + 1 # update IDs transactions already included
    }
    return(ledger)
}

#' @export
.addTransactionYear <- function() {
    yr <- readline("Enter year or leave blank for current: ")
    yr <- ifelse(yr == "", currentYear(), yr)
    return(yr)
}

#' @export
.addTransactionMonth <- function() {
    mo <- readline("Enter month or leave blank for current: ")
    mo <- ifelse(mo == "", currentMonth(), mo)
    return(mo)
}

#' @export
.addTransactionDay <- function() {
    dy <- readline("Enter day or leave blank for current: ")
    dy <- ifelse(dy == "", currentDay(), dy)
    return(dy)
}

#' @export
.addTransactionDescription <- function() {
    description <- ""
    while (description == "") description <- readline("Enter description: ")
    return(description)
}

#' @export
.addTransactionBudgetCategory <- function() {
    existing.categories <- viewBudgetCategoriesAll(suppress = FALSE)
    budget <- readline("Enter budget category from above or its index: ")
    while (TRUE) {
        budget.numeric <- suppressWarnings(as.numeric(budget))
        if (!is.na(budget.numeric)) { # budget is numeric
            if (budget.numeric > 0 && budget.numeric <= length(existing.categories)) return(existing.categories[budget.numeric])
            budget <- readline("Not a valid index. Enter a new budget category from above or its index: ")
        } else { # budget is character, probably
            if (budget %in% existing.categories) return(budget)
            if (budget == "") {
                budget <- readline("Budget cannot be blank. Enter a new one or leave blank to add. ")
                next()
            }
            budget.temp <- budget
            budget <- readline(paste(budget, "is not a current budget category. Enter a new one or leave blank to add. "))
            if (budget == "") return(budget.temp)
        }
    }
}

#' @export
.addTransactionAccountCategory <- function(account = NULL, amount = NULL) {
    account.amounts <- data.frame(account = numeric(), amount = numeric())
    existing.categories <- viewAccountCategories(suppress = !is.null(account))
    inputs.supplied <- !is.null(account) || !is.null(amount) # assumes no account or amount was provided - if one was, don't ask for addtl accounts
    while (TRUE) {
        if (is.null(account)) {
            account <- readline("Enter account category from above or its index: ")
        } else {
            inputs.supplied <- TRUE
        }
        if (account == "") break()
        if (suppressWarnings(!is.na(as.numeric(account)))) account <- viewAccountCategories()[as.numeric(account)]
        account.temp <- "placeholder"
        while (!(account %in% existing.categories) && account.temp != "") {
            account.temp <- readline(paste(account, "is not a current account. Enter a new one or leave blank to add. "))
            if (account.temp != "") account <- account.temp
        }
        if (is.null(amount)) amount <- as.numeric(readline(paste("Enter amount for ", account, ": ", sep = "")))
        account.amounts <- rbind(account.amounts, data.frame(account = account, amount = amount))
        if (inputs.supplied) break()
        account <- NULL
        amount <- NULL
    }
    return(account.amounts)
}


#' @rdname AddTransaction
#' @export
viewLedgerMonth <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    viewLedger(ledger = ledger, from = componentsToDate(dy = 1))
}
