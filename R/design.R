#' Plan the sample size of an experiment
#'
#' Sample size per variant needed to detect a given difference in conversion
#' rate between a control and one or more variants, with the normal
#' approximation for two independent proportions (Fleiss, Levin and Paik,
#' 2003, chapter 4, without continuity correction). With several variants the
#' significance level is divided among the comparisons (Bonferroni), which is
#' conservative and keeps the planning simple and transparent.
#'
#' @param baseline Expected conversion rate of the control (between 0 and 1).
#' @param mde Minimum detectable effect: the smallest difference worth
#'   detecting, relative to `baseline` (`relative = TRUE`, e.g. `0.10` for a
#'   10% lift) or in absolute percentage points (`relative = FALSE`, e.g.
#'   `0.005`).
#' @param relative Whether `mde` is relative to the baseline.
#' @param alpha Significance level for the whole experiment.
#' @param power Probability of detecting the effect if it exists.
#' @param variants Number of variants compared with the control.
#' @param ratio Allocation ratio: participants per variant for each one in the
#'   control (1 for an even split).
#' @param alternative `"two.sided"` or `"greater"` (the variant is expected to
#'   be better).
#' @param daily_traffic Optional number of eligible participants per day, to
#'   estimate the duration.
#' @return An object of class `af_design` with the sample size per group, the
#'   total and, if `daily_traffic` is given, the days needed.
#' @export
#' @examples
#' # Detect a 10% relative lift on a 4% conversion rate
#' af_design(baseline = 0.04, mde = 0.10, daily_traffic = 5000)
#'
#' # Two variants against the control, one-sided
#' af_design(0.04, 0.10, variants = 2, alternative = "greater")
af_design <- function(baseline, mde, relative = TRUE, alpha = 0.05, power = 0.8,
                      variants = 1, ratio = 1, alternative = c("two.sided", "greater"),
                      daily_traffic = NULL) {
  alternative <- match.arg(alternative)
  stopifnot(is_prob(baseline), is.numeric(mde), length(mde) == 1, mde != 0, is_prob(alpha),
            is_prob(power), variants >= 1, ratio > 0)
  p1 <- baseline
  p2 <- if (relative) baseline * (1 + mde) else baseline + mde
  if (!(p2 > 0 && p2 < 1)) stop("The baseline and the effect give a rate outside (0, 1).", call. = FALSE)
  a <- alpha / variants
  z_a <- stats::qnorm(1 - if (alternative == "two.sided") a / 2 else a)
  z_b <- stats::qnorm(power)
  k <- ratio
  pbar <- (p1 + k * p2) / (1 + k)
  n_control <- (z_a * sqrt(pbar * (1 - pbar) * (1 + 1 / k)) + z_b * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))^2 / (p2 - p1)^2
  n_control <- ceiling(n_control)
  n_variant <- ceiling(n_control * k)
  total <- n_control + variants * n_variant
  structure(list(baseline = p1, target = p2, mde = mde, relative = relative, alpha = alpha,
                 alpha_per_comparison = a, power = power, variants = variants, ratio = ratio,
                 alternative = alternative, n_control = n_control, n_variant = n_variant, total = total,
                 days = if (is.null(daily_traffic)) NA_real_ else ceiling(total / daily_traffic)),
            class = "af_design")
}

is_prob <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0 && x < 1

#' @export
print.af_design <- function(x, ...) {
  pc <- function(v) paste0(formatC(100 * v, format = "f", digits = 2), "%")
  cat("Experiment plan\n")
  cat("  Control rate     ", pc(x$baseline), "\n", sep = "")
  cat("  Rate to detect   ", pc(x$target), if (x$relative) sprintf(" (%+.1f%% relative)", 100 * x$mde) else "", "\n", sep = "")
  cat("  Alpha            ", x$alpha, if (x$variants > 1) sprintf(" (%.4f per comparison)", x$alpha_per_comparison) else "",
      ", ", x$alternative, "\n", sep = "")
  cat("  Power            ", x$power, "\n", sep = "")
  cat("  Per group        ", format(x$n_control, big.mark = ","), " control, ",
      format(x$n_variant, big.mark = ","), " per variant\n", sep = "")
  cat("  Total            ", format(x$total, big.mark = ","), "\n", sep = "")
  if (!is.na(x$days)) cat("  Duration         ", x$days, " days (run whole weeks to cover weekly cycles)\n", sep = "")
  invisible(x)
}

#' Check for sample ratio mismatch
#'
#' Compares the number of participants assigned to each group with the
#' planned allocation using a chi-squared goodness-of-fit test. A mismatch
#' (by convention, p < 0.001) signals a problem with randomisation, tracking or
#' eligibility, and results should not be trusted until it is explained
#' (Fabijan et al., 2019, \doi{10.1145/3292500.3330722}).
#'
#' @param observed Participants per group, in the same order as `expected`.
#'   A named vector or an `af_experiment`.
#' @param expected Planned share of each group; equal by default.
#' @param threshold p-value below which a mismatch is flagged.
#' @return An object of class `af_srm` with the test and a `mismatch` flag.
#' @export
#' @examples
#' af_srm(c(control = 50210, variant = 49790))   # fine
#' af_srm(c(control = 50900, variant = 49100))   # mismatch
af_srm <- function(observed, expected = NULL, threshold = 0.001) {
  if (inherits(observed, "af_experiment")) observed <- stats::setNames(observed$n, observed$arm)
  if (!is.numeric(observed) || length(observed) < 2 || any(observed < 0)) {
    stop("`observed` needs two or more non-negative counts.", call. = FALSE)
  }
  if (is.null(expected)) expected <- rep(1, length(observed))
  if (length(expected) != length(observed)) stop("`expected` must have one share per group.", call. = FALSE)
  share <- expected / sum(expected)
  test <- suppressWarnings(stats::chisq.test(observed, p = share))
  structure(list(observed = observed, expected = sum(observed) * share, statistic = unname(test$statistic),
                 df = unname(test$parameter), p_value = test$p.value, threshold = threshold,
                 mismatch = test$p.value < threshold), class = "af_srm")
}

#' @export
print.af_srm <- function(x, ...) {
  cat("Sample ratio check\n")
  tab <- data.frame(group = names(x$observed) %||% seq_along(x$observed),
                    observed = as.numeric(x$observed), expected = round(x$expected, 1))
  print(tab, row.names = FALSE)
  cat(sprintf("Chi-squared = %.2f, df = %d, p = %s\n", x$statistic, as.integer(x$df), format.pval(x$p_value, digits = 3)))
  cat(if (x$mismatch) "MISMATCH: investigate assignment and tracking before reading the results.\n"
      else "No evidence of sample ratio mismatch.\n")
  invisible(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
