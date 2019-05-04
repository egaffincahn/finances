#' @title Add, edit or view transactions
#'
#' @name AddTransaction
#'
#' @import tidyverse
#'
#' @description Add a transaction to the ledger via inputs to the console with
#'   \code{addTransaction}.\cr \code{editTransaction} will display the
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
addTransaction <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), description = NULL,
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
    account.amounts <- .addTransactionAccountCategory(account = account, amount = amount)
    for (i in 1:nrow(account.amounts)) { # in case a new account was added
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
    existing.categories <- viewAccountCategories(suppress = !is.null(account))
    if (length(amount) > length(account)) warning("Careful: more amounts than accounts", call. = FALSE, immediate. = TRUE)
    count <- 0 # counts the number of account,amount pairs in the transaction
    account.temp <- NULL
    while (TRUE) {
        count <- count + 1
        message <- "Enter account category from above or its index. Leave blank to finish: "
        account.temp.new <- NULL
        try.again <- FALSE
        while (TRUE) {
            if (count > length(account) || try.again) {
                account.temp <- readline(message)
                if (account.temp == "") {
                    if (!is.null(account.temp.new)) account[count] <- account.temp.new
                    break()
                }
            } else if (is.numeric(account[count]) && !(account[count] >= 1 && account[count] <= length(existing.categories))) {
                account.temp <- account[count] # invalid account, will fail later and try again
            } else if (!is.numeric(account[count]) && !(account[count] %in% existing.categories)) {
                account.temp <- account[count] # invalid account, will fail later and try again
            } else {
                break()
            }
            account.temp.numeric <- suppressWarnings(as.numeric(account.temp))
            if (is.na(account.temp.numeric)) {
                if (account.temp %in% existing.categories) {
                    account[count] <- account.temp
                    break()
                }
            } else {
                if (account.temp.numeric >= 1 && account.temp.numeric <= length(existing.categories)) {
                    account[count] <- existing.categories[account.temp.numeric]
                    break()
                }
            }
            message <- paste(account.temp, "is not a current account. Enter a new one or leave blank to add new account. ")
            account.temp.new <- account.temp
            try.again <- TRUE
        }
        if (!is.null(account.temp) && account.temp == "" && is.null(account.temp.new) && length(account) == length(amount)) break() # want to do this when actually finished

        if (is.numeric(account[count])) account[count] <- existing.categories[account[count]]
        message <- paste0("Enter amount for ", account[count], ": ")
        while (TRUE) {
            if (count > length(amount)) {
                amount.temp <- suppressWarnings(as.numeric(readline(message)))
            } else {
                amount.temp <- amount[count]
            }
            if (!is.na(amount.temp)) {
                amount[count] <- amount.temp
                break()
            }
            message <- paste0(amount.temp, " is not a valid number. Enter a valid number: ")
        }
    }
    account.amounts <- data.frame(account = account, amount = amount)
    return(account.amounts)
}


#' @rdname AddTransaction
#' @export
viewLedgerMonth <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    viewLedger(ledger = ledger, from = componentsToDate(dy = 1))
}
