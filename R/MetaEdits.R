#' @title High-level edits
#'
#' @name MetaEdits
#'
#'
#' @description These functions have the power to add new accounts, create new
#'   ledgers, etc.
#'
#' @inheritParams viewLedger
#'
#' @details \code{createLedger} builds a new ledger in the standard form. It
#'   queries the user for account names, initial values, budget categories and
#'   amounts, and file location.\cr \code{addAccount} can take a ledger and add
#'   a new account name to it. It queries the user for the account name and the
#'   column position in the ledger.
#'

#' @rdname MetaEdits
#' @export
createLedger <- function() {
    # ...
    saveLedger(ledger, file = file)
}

#' @rdname MetaEdits
#' @export
addAccount <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    print("Current account names:")
    viewAccountCategories(ledger = ledger, suppress = FALSE)
    new.account <- readline("Enter new account name (including <acct.>) ")
    ledger[[new.account]] <- numeric(nrow(ledger))
    non.account.columns <- ncol(ledger) - length(viewAccountCategories(ledger = ledger))
    account.index <- as.numeric(readline("Enter position number: ")) + non.account.columns - .5
    ledger <- ledger[, order(c(1:(ncol(ledger)-1), account.index))]
    saveLedger(ledger, file = file)
}

#' @rdname MetaEdits
#' @export
setBudgets <- function(ledger = viewLedger(file = file), file = viewLedgerFile()) {
    load(.dataLocation())
    budget.categories <- viewBudgetCategoriesAll()
    budget.amounts <- numeric(length(budget.categories))
    print("For each budget category, enter the budget value you expect per month. Income should be positive, costs should be negative. If it is a special category, type NA.")
    # suppressWarnings(warning("NAs introduced by coercion"))
    for (i in seq(budget.categories)) {
        budget.amounts[i] <- as.numeric(readline(paste(budget.categories[i], ": ", sep = "")))
    }
    budgets <- budget.amounts
    names(budgets) <- budget.categories
    save("budgets", "file", file = dataLocation())
}
