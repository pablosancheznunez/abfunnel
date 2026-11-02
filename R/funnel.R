#' Analyse a conversion funnel
#'
#' Computes, for each group and step of a funnel (for example visit, product
#' page, cart, purchase), the share of participants who reach the step from
#' the previous one and from the start, with Wilson intervals, and the
#' difference from the control at each step.
#'
#' @param data A data frame with one row per group and step.
#' @param step,arm,users Columns with the step name, the group and the
#'   number of participants who reached the step.
#' @param steps Step names in funnel order; by default, the order of first
#'   appearance.
#' @param control Name of the control group; by default the first group.
#' @param level Confidence level.
#' @return An object of class `af_funnel`: a data frame with one row per group
#'   and step.
#' @export
#' @examples
#' f <- af_funnel(funnel_demo)
#' f[f$step == "purchase", ]
af_funnel <- function(data, step = "step", arm = "arm", users = "users", steps = NULL,
                      control = NULL, level = 0.95) {
  miss <- setdiff(c(step, arm, users), names(data))
  if (length(miss)) stop("Columns not found: ", paste(miss, collapse = ", "), call. = FALSE)
  d <- data.frame(step = as.character(data[[step]]), arm = as.character(data[[arm]]),
                  users = as.numeric(data[[users]]), stringsAsFactors = FALSE)
  steps <- steps %||% unique(d$step)
  if (!setequal(steps, unique(d$step))) stop("`steps` must list every step exactly once.", call. = FALSE)
  arms <- unique(d$arm)
  control <- control %||% arms[1]
  if (!control %in% arms) stop("Control group \"", control, "\" not found.", call. = FALSE)
  arms <- c(control, setdiff(arms, control))
  rows <- list()
  for (a in arms) {
    da <- d[d$arm == a, ]
    u <- da$users[match(steps, da$step)]
    if (anyNA(u)) stop("Group \"", a, "\" lacks some steps.", call. = FALSE)
    if (any(diff(u) > 0)) stop("Group \"", a, "\" has more users at a later step than an earlier one.", call. = FALSE)
    prev <- c(u[1], u[-length(u)])
    ci_step <- wilson(u, prev, level)
    ci_total <- wilson(u, u[1], level)
    rows[[a]] <- data.frame(arm = a, step = steps, position = seq_along(steps), users = u,
                            from_previous = u / prev, from_previous_low = ci_step[, 1], from_previous_high = ci_step[, 2],
                            from_start = u / u[1], from_start_low = ci_total[, 1], from_start_high = ci_total[, 2],
                            stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  ctrl <- rows[[control]]
  out$diff_from_start <- out$from_start - ctrl$from_start[match(out$step, ctrl$step)]
  out$diff_from_start[out$arm == control] <- NA
  rownames(out) <- NULL
  structure(out, class = c("af_funnel", "data.frame"), control = control, steps = steps, level = level)
}

#' @export
print.af_funnel <- function(x, ...) {
  if (is.null(attr(x, "steps"))) return(print(as.data.frame(unclass(x)), ...))
  pc <- function(v) ifelse(is.na(v), "", paste0(formatC(100 * v, format = "f", digits = 1), "%"))
  cat("Funnel (control: ", attr(x, "control"), ")\n", sep = "")
  shown <- data.frame(arm = x$arm, step = x$step, users = format(x$users, big.mark = ","),
                      from_previous = pc(x$from_previous), from_start = pc(x$from_start),
                      vs_control = ifelse(is.na(x$diff_from_start), "",
                                          paste0(ifelse(x$diff_from_start >= 0, "+", ""), formatC(100 * x$diff_from_start, format = "f", digits = 2), " pp")))
  print(shown, row.names = FALSE)
  invisible(x)
}
