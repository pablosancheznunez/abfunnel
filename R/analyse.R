wilson <- function(x, n, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  out <- cbind(centre - half, centre + half)
  out[!(n > 0), ] <- NA
  pmin(pmax(out, 0), 1)
}

#' Prepare experiment results
#'
#' Accepts either one row per group with participants and conversions, or one
#' row per participant with a 0/1 outcome, and returns one row per group.
#'
#' @param data A data frame.
#' @param arm Column naming the group (control or variant).
#' @param n Column with the number of participants per group (aggregated data).
#' @param conversions Column with conversions per group (aggregated data) or a
#'   0/1 outcome per participant (when `n` is `NULL`).
#' @param control Name of the control group; by default the first group.
#' @return An object of class `af_experiment`.
#' @export
#' @examples
#' af_experiment(data.frame(arm = c("control", "new_cta"), users = c(10000, 10000),
#'                          orders = c(400, 452)),
#'               n = "users", conversions = "orders")
#'
#' # One row per participant
#' set.seed(1)
#' users <- data.frame(arm = rep(c("A", "B"), each = 500), bought = rbinom(1000, 1, 0.1))
#' af_experiment(users, conversions = "bought", n = NULL)
af_experiment <- function(data, arm = "arm", n = "n", conversions = "conversions", control = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  need <- c(arm, n, conversions)
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Columns not found: ", paste(miss, collapse = ", "), call. = FALSE)
  groups <- as.character(data[[arm]])
  if (is.null(n)) {
    y <- data[[conversions]]
    if (!all(y %in% c(0, 1, TRUE, FALSE))) stop("Per-participant outcomes must be 0/1 or TRUE/FALSE.", call. = FALSE)
    lv <- unique(groups)
    out <- data.frame(arm = lv, n = as.numeric(table(factor(groups, lv))),
                      conversions = as.numeric(tapply(as.numeric(y), factor(groups, lv), sum)),
                      stringsAsFactors = FALSE)
  } else {
    out <- data.frame(arm = groups, n = as.numeric(data[[n]]), conversions = as.numeric(data[[conversions]]),
                      stringsAsFactors = FALSE)
    out <- stats::aggregate(cbind(n, conversions) ~ arm, data = out, FUN = sum)
    out <- out[match(unique(groups), out$arm), ]
  }
  if (nrow(out) < 2) stop("An experiment needs at least two groups.", call. = FALSE)
  if (any(out$conversions > out$n) || any(out$n < 0) || any(out$conversions < 0)) {
    stop("Conversions must be between 0 and the number of participants.", call. = FALSE)
  }
  control <- control %||% out$arm[1]
  if (!control %in% out$arm) stop("Control group \"", control, "\" not found.", call. = FALSE)
  out <- rbind(out[out$arm == control, ], out[out$arm != control, ])
  rownames(out) <- NULL
  structure(out, class = c("af_experiment", "data.frame"), control = control)
}

prob_better <- function(x1, n1, x2, n2, prior = c(1, 1)) {
  # P(p2 > p1) for independent Beta posteriors, by numerical integration:
  # integral over p of f2(p) * F1(p).
  a1 <- prior[1] + x1; b1 <- prior[2] + n1 - x1
  a2 <- prior[1] + x2; b2 <- prior[2] + n2 - x2
  sd2 <- sqrt(a2 * b2 / ((a2 + b2)^2 * (a2 + b2 + 1)))
  m2 <- a2 / (a2 + b2)
  lo <- max(0, m2 - 12 * sd2); hi <- min(1, m2 + 12 * sd2)
  stats::integrate(function(p) stats::dbeta(p, a2, b2) * stats::pbeta(p, a1, b1), lo, hi,
                   rel.tol = 1e-8, subdivisions = 500L)$value
}

#' Analyse an experiment
#'
#' Compares every variant with the control. Following current guidance for
#' online experiments, the emphasis is on the size of the effect and its
#' uncertainty rather than on a significance verdict:
#'
#' * conversion rate of each group with a Wilson interval;
#' * absolute difference with Newcombe's hybrid score interval (Newcombe,
#'   1998);
#' * relative lift (risk ratio minus one) with a log-scale interval (Katz et
#'   al., 1978);
#' * two-sided test of proportions, with Holm's adjustment when there are
#'   several variants;
#' * optionally, the Bayesian probability that the variant beats the control,
#'   with independent Beta priors, computed exactly by numerical integration.
#'
#' A sample ratio check ([af_srm()]) is included; if it fails, interpret
#' nothing else.
#'
#' @param x An `af_experiment`.
#' @param level Confidence level.
#' @param expected Planned allocation for the sample ratio check (equal by
#'   default).
#' @param bayes Also compute the probability that each variant beats the
#'   control.
#' @param prior Beta prior parameters for `bayes` (uniform by default).
#' @return An object of class `af_result`: a data frame with one row per
#'   variant, plus the sample ratio check as attribute `srm`.
#' @export
#' @examples
#' res <- af_analyse(experiment_demo)
#' res
af_analyse <- function(x, level = 0.95, expected = NULL, bayes = TRUE, prior = c(1, 1)) {
  if (!inherits(x, "af_experiment")) stop("`x` must be an af_experiment; see af_experiment().", call. = FALSE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  ci <- wilson(x$conversions, x$n, level)
  rate <- x$conversions / x$n
  c_rate <- rate[1]; c_lo <- ci[1, 1]; c_hi <- ci[1, 2]
  v <- seq_len(nrow(x))[-1]
  diff <- rate[v] - c_rate
  rr <- rate[v] / c_rate
  se_log <- sqrt(1 / x$conversions[v] - 1 / x$n[v] + 1 / x$conversions[1] - 1 / x$n[1])
  p <- vapply(v, function(i) suppressWarnings(stats::prop.test(x$conversions[c(i, 1)], x$n[c(i, 1)], correct = FALSE)$p.value), 0)
  out <- data.frame(
    variant = x$arm[v], control = x$arm[1],
    n = x$n[v], conversions = x$conversions[v],
    rate = rate[v], rate_low = ci[v, 1], rate_high = ci[v, 2],
    control_rate = c_rate,
    diff = diff,
    diff_low = diff - sqrt((rate[v] - ci[v, 1])^2 + (c_hi - c_rate)^2),
    diff_high = diff + sqrt((ci[v, 2] - rate[v])^2 + (c_rate - c_lo)^2),
    lift = rr - 1,
    lift_low = exp(log(rr) - z * se_log) - 1,
    lift_high = exp(log(rr) + z * se_log) - 1,
    p_value = p,
    p_adjusted = stats::p.adjust(p, method = "holm"),
    stringsAsFactors = FALSE
  )
  if (bayes) {
    out$prob_better <- vapply(v, function(i) prob_better(x$conversions[1], x$n[1], x$conversions[i], x$n[i], prior), 0)
  }
  structure(out, class = c("af_result", "data.frame"), srm = af_srm(x, expected), level = level,
            control_n = x$n[1], control_conversions = x$conversions[1])
}

#' @export
print.af_result <- function(x, digits = 3, ...) {
  srm <- attr(x, "srm")
  if (is.null(srm)) return(print(as.data.frame(unclass(x)), ...))
  pc <- function(v) paste0(formatC(100 * v, format = "f", digits = 2), "%")
  pp <- function(v) paste0(formatC(100 * v, format = "f", digits = 2), " pp")
  cat("Experiment results (", round(100 * attr(x, "level")), "% intervals)\n", sep = "")
  cat("Control \"", x$control[1], "\": ", pc(x$control_rate[1]), " (", format(attr(x, "control_n"), big.mark = ","),
      " participants)\n\n", sep = "")
  for (i in seq_len(nrow(x))) {
    r <- x[i, ]
    cat(sprintf("%s: %s, difference %s [%s, %s], relative lift %s [%s, %s], adjusted p = %s%s\n",
                r$variant, pc(r$rate), pp(r$diff), pp(r$diff_low), pp(r$diff_high),
                pc(r$lift), pc(r$lift_low), pc(r$lift_high), format.pval(r$p_adjusted, digits = 2),
                if (!is.null(r$prob_better)) sprintf(", P(better) = %.3f", r$prob_better) else ""))
  }
  cat("\n")
  if (srm$mismatch) {
    cat("WARNING: sample ratio mismatch (p = ", format.pval(srm$p_value, digits = 2),
        "). Do not interpret these results until it is explained.\n", sep = "")
  } else {
    cat("Sample ratio check passed (p = ", format.pval(srm$p_value, digits = 2), ").\n", sep = "")
  }
  invisible(x)
}
