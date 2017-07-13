#' @title Plot financial data
#'
#' @name PlotFinances
#'
#' @import ggplot2
#' @importFrom Hmisc monthDays
#' @param budget.category A character vector of budget category names.
#'
#' @description Various plotting functions to show assets, accounts, budgets, etc. over different time periods.
#'
#' @details All \code{Plot} functions show the dollar amounts over some
#'   specified time. \code{Change} shows the monthly adjustment to the ledger
#'   (i.e., income and spending per month) while \code{Cumulative} shows the
#'   total amounts up until that month. \cr\code{Assets} combines all accounts,
#'   while \code{Accounts} splits each financial account up for viewing. \cr The
#'   \code{Budget} series shows amounts for each budget category. \code{History}
#'   shows all months in the ledger, while \code{plotBudgetsBar} allows the user
#'   to input a specific month and give breakdowns of spending compared to the
#'   set budget values. This defaults to the current month. This takes a
#'   character vector of budget category names. This can be a custom list such
#'   as \code{c("Entertainment", "Food")}, or you can use the package functions
#'   \code{viewBudgetCategoriesAll()}, \code{viewBudgetCategoriesSpending()}, or
#'   \code{viewBudgetCategoriesReal()}. \code{plotBudgetsPie} combines all the
#'   budgets and shows the amount spent and remaining next to the days left in
#'   the month for easy visual tracking of progress.
#'
#' @inheritParams viewLedger
#'
#' @return \code{ggplot} object, which is automatically printed if not stored in a variable.
#'

#' @rdname PlotFinances
#' @export
plotAccountsChange <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), from = NULL, to = NULL) {
    accounts <- viewAccountCategories()
    monthly.accounts <- ledger %>% viewTransactions(from = from, to = to, query = FALSE, trunc = FALSE) %>%
        select(-day, -description, -budget.category) %>% group_by(year, month) %>%
        do(data.frame(assets.change = colSums(.[accounts]), account = accounts)) %>% as.data.frame %>%
        mutate(abs.month = componentsToDate(yr = .$year, mo = .$month, dy = 15))
    monthly.accounts$account <- factor(monthly.accounts$account, levels = viewAccountCategories(ledger), labels = viewAccountCategories(ledger, formal = TRUE))
    g <- ggplot(monthly.accounts, aes(abs.month, assets.change, fill = account)) +
        geom_area() + theme_minimal() +
        labs(x = "Date", y = "Change in Account ($)", title = "Account Value by Month", fill = "Account")
    return(g)
}

#' @rdname PlotFinances
#' @export
plotAccountsCumulative <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), from = NULL, to = NULL) {
    accounts <- viewAccountCategories()
    monthly.accounts <- ledger %>% viewTransactions(from = from, to = to, query = FALSE, trunc = FALSE) %>%
        select(-day, -description, -budget.category) %>% group_by(year, month) %>%
        do(data.frame(as.list(colSums(.[accounts])))) %>%
        {as.data.frame(cbind(year = .$year, month = .$month, apply(.[, accounts], 2, function(x) cumsum(x))))} %>%
        mutate(abs.month = componentsToDate(yr = .$year, mo = .$month, dy = 15)) %>%
        gather(account, assets.net, -year, -month, -abs.month)
    monthly.accounts$account <- factor(monthly.accounts$account, levels = viewAccountCategories(ledger), labels = viewAccountCategories(ledger, formal = TRUE))
    g <- ggplot(monthly.accounts, aes(abs.month, assets.net, fill = account)) +
        geom_area() + theme_minimal() +
        labs(x = "Date", y = "Account Value ($)", title = "Cumulative Account Value", fill = "Account")
    return(g)
}

#' @rdname PlotFinances
#' @export
plotAssetsChange <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), from = NULL, to = NULL) {
    account.categories <- viewAccountCategories()
    monthly.assets.change <- ledger %>% viewTransactions(from = from, to = to, query = FALSE, trunc = FALSE) %>%
        select(-day, -description, -budget.category) %>% group_by(year, month) %>%
        do(data.frame(assets.change = sum(.[, account.categories]))) %>% as.data.frame() %>%
        mutate(abs.month = componentsToDate(yr = .$year, mo = .$month, dy = 15))
    g <- ggplot(monthly.assets.change, aes(abs.month, assets.change)) +
        geom_area()  + theme_minimal() +
        labs(x = "Date", y = "Amount ($)", title = "Net Change")
    return(g)
}

#' @rdname PlotFinances
#' @export
plotAssetsCumulative <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), from = NULL, to = NULL) {
    account.categories <- viewAccountCategories()
    monthly.assets <- ledger %>% viewTransactions(from = from, to = to, query = FALSE, trunc = FALSE) %>%
        select(-day, -description, -budget.category) %>% group_by(year, month) %>%
        do(data.frame(assets.change = sum(.[, account.categories]))) %>% as.data.frame %>%
        mutate(assets.net = cumsum(assets.change), abs.month = componentsToDate(yr = .$year, mo = .$month, dy = 15))
    g <- ggplot(monthly.assets, aes(abs.month, assets.net)) +
        geom_area()  + theme_minimal() +
        labs(x = "Date", y = "Amount ($)", title = "Net Assets")
    return(g)
}

#' @rdname PlotFinances
#' @export
plotBudgetsHistory <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), budget.categories = viewBudgetCategoriesAll(), from = NULL, to = NULL) {
    account.categories <- viewAccountCategories()
    monthly.spending <- ledger %>% viewTransactions(from = from, to = to, query = FALSE, trunc = FALSE) %>%
        select(-day, -description) %>% filter(budget.category %in% budget.categories) %>%
        group_by(year, month, budget.category) %>% do(data.frame(spending = sum(.[, account.categories]))) %>% as.data.frame %>%
        mutate(abs.month = componentsToDate(yr = .$year, mo = .$month, dy = 15))
    g <- ggplot(monthly.spending, aes(abs.month, spending, fill = budget.category)) +
        geom_area() + theme_minimal() +
        labs(x = "Date", y = "Amount ($)", title = "Budgeted Categories", fill = "Budget Category")
    return(g)
}

#' @rdname PlotFinances
#' @export
plotBudgetsBar <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), budget.categories = viewBudgetCategoriesAll()) {
    ledger.budgets <- .plotBudgetsDate(ledger, yr, mo, budget.categories)
    g <- ggplot(ledger.budgets, aes(budget.category, amount, fill = cat)) +
        geom_bar(stat = "identity") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Budget Category", y = "Amount ($)", title = "All Budgets", fill = "") +
        scale_fill_manual(values = c("overearned" = "blue", "overspent" = "red", "remaining" = "green", "accounted" = "black"))
    return(g)
}

#' @rdname PlotFinances
#' @export
plotBudgetsPie <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), budget.categories = viewBudgetCategoriesSpendingSmall()) {
    ledger.budgets <- .plotBudgetsDate(ledger, yr, mo, budget.categories) %>% group_by(cat) %>% do(data.frame(amount = sum(.$amount)))
    overspent_updated <- filter(ledger.budgets, cat == "overspent")$amount + filter(ledger.budgets, cat == "overearned")$amount
    remaining_updated <- overspent_updated - filter(ledger.budgets, cat == "remaining")$amount
    if (remaining_updated > 0) { # some remaining
        overspent_updated <- 0
        # keep remaining_updated the same
        accounted_updated <- -filter(ledger.budgets, cat == "accounted")$amount
    } else { # truly overspent
        overspent_updated <- -remaining_updated
        remaining_updated <- 0
        accounted_updated <- -(filter(ledger.budgets, cat == "accounted")$amount + filter(ledger.budgets, cat == "remaining")$amount)
    }
    ledger.budgets <- data.frame(amount = c(accounted_updated, remaining_updated, overspent_updated),
                                 cat = factor(c("accounted", "remaining", "overspent"), levels = c("overspent", "remaining", "accounted")),
                                 UNIT = factor("money", levels = c("money", "days")))
    past <- ifelse(mo == currentMonth() && yr == currentYear(), currentDay(), monthDays(componentsToDate(yr = yr, mo = mo)))
    future <- monthDays(componentsToDate(yr = yr, mo = mo)) - past
    ledger.budgets <- ledger.budgets %>% rbind(c(past, "accounted", "days")) %>% rbind(c(future, "remaining", "days")) %>%
        filter(!(amount == 0 & UNIT == "money")) %>% group_by(UNIT) %>% do({
            accounted.percent <- as.numeric(filter(., cat == "accounted")$amount) / sum(as.numeric(.$amount))
            is.accounted <- .$cat == "accounted"
            data.frame(amount = as.numeric(.$amount), cat = .$cat, UNIT = .$UNIT,
                       annotation.positions = (is.accounted * accounted.percent + (!is.accounted) * (accounted.percent + 1)) / 2)
        })
    g <- ggplot(ledger.budgets, aes(1, y = amount, fill = cat)) +
        geom_bar(stat = "identity", position = "fill") + facet_grid(.~UNIT) + coord_polar(theta = "y") +
        theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
        labs(title = "Days and Budgets Remaining in Month", fill = "") +
        scale_fill_manual(values = c("overspent" = "red", "remaining" = "green", "accounted" = "black")) +
        geom_text(aes(1, annotation.positions, label = amount), color = "white")
    return(g)
}

#' Gets a reduced ledger for easier plotting based on a given month and certain specifications
#' @export
.plotBudgetsDate <- function(ledger, yr, mo, budget.categories) {
    account.categories <- viewAccountCategories()
    budgeted <- unname(viewBudgets(suppress = TRUE)[budget.categories])
    ledger.budgets <- data.frame(budget.category = factor(budget.categories, levels = budget.categories[order(abs(budgeted))]),
                                 overearned = numeric(length(budget.categories)),
                                 overspent = numeric(length(budget.categories)),
                                 remaining = numeric(length(budget.categories)),
                                 accounted = numeric(length(budget.categories)))
    happened <- lapply(budget.categories, function(x) {
        ledger.mini <- filter(ledger, year == yr, month == mo, budget.category == x)
        ifelse(nrow(ledger.mini) == 0, 0, sum(ledger.mini[, account.categories]))
    }) %>% unlist
    for (i in seq(budget.categories)) {
        if (budget.categories[i] %in% viewBudgetCategoriesSpending()) {
            ledger.budgets[i, ]$overearned <- max(happened[i], 0)
            ledger.budgets[i, ]$overspent <- min(happened[i] - budgeted[i], 0)
            ledger.budgets[i, ]$remaining <- min(budgeted[i] - happened[i], 0)
            ledger.budgets[i, ]$accounted <- min(max(budgeted[i], happened[i]), 0)
        } else if (budget.categories[i] %in% viewBudgetCategoriesReal()) { # adds wages
            ledger.budgets[i, ]$overearned <- max(happened[i] - budgeted[i], 0)
            ledger.budgets[i, ]$overspent <- min(happened[i], 0)
            ledger.budgets[i, ]$remaining <- min(max(budgeted[i] - happened[i], 0), budgeted[i])
            ledger.budgets[i, ]$accounted <- min(max(happened[i], 0), budgeted[i])
        } else if (budget.categories[i] %in% viewBudgetCategoriesAll()) { # adds the weird ones
            ledger.budgets[i, ]$overearned <- max(happened[i], 0)
            ledger.budgets[i, ]$overspent <- min(happened[i], 0)
        }
    }
    ledger.budgets <- gather(ledger.budgets, cat, amount, -budget.category)
    ledger.budgets$cat <- factor(ledger.budgets$cat, levels = c("overearned", "remaining", "overspent", "accounted"))
    return(ledger.budgets)
}
