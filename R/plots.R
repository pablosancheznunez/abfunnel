palette_af <- c("#0072B2", "#D55E00", "#007A5E", "#AA3377", "#7A6A00", "#332288")

pct_lab <- function(v, digits = 1) paste0(formatC(100 * v, format = "f", digits = digits), "%")

#' Plot the effect of each variant
#'
#' Relative lift of each variant over the control with its confidence
#' interval. The vertical line marks no effect; intervals that cross it are
#' compatible with no difference.
#'
#' @param x An `af_result` from [af_analyse()].
#' @param main Chart title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' af_plot_lift(af_analyse(experiment_demo))
af_plot_lift <- function(x, main = "Relative lift over the control") {
  if (!inherits(x, "af_result")) stop("`x` must come from af_analyse().", call. = FALSE)
  n <- nrow(x)
  xlim <- range(c(0, x$lift_low, x$lift_high))
  op <- graphics::par(mai = c(0.7, max(graphics::strwidth(x$variant, units = "inches", cex = 0.9), 0.6) + 0.4, 0.6, 0.3),
                      las = 1, bty = "n", cex.axis = 0.85)
  on.exit(graphics::par(op))
  graphics::plot(NA, xlim = xlim, ylim = c(0.5, n + 0.5), xaxt = "n", yaxt = "n", xlab = "", ylab = "",
                 main = main, font.main = 2, cex.main = 1)
  ticks <- pretty(xlim)
  graphics::abline(v = ticks, col = grDevices::adjustcolor("black", 0.1))
  graphics::abline(v = 0, lwd = 1.5)
  graphics::axis(1, at = ticks, labels = pct_lab(ticks, 0), lwd = 0, lwd.ticks = 0)
  graphics::axis(2, at = n:1, labels = x$variant, lwd = 0, lwd.ticks = 0, cex.axis = 0.9)
  graphics::segments(x$lift_low, n:1, x$lift_high, n:1, col = palette_af[1], lwd = 3)
  graphics::points(x$lift, n:1, pch = 21, bg = "white", col = palette_af[1], cex = 1.4, lwd = 2)
  graphics::mtext(sprintf("Points: estimated lift. Lines: %d%% confidence interval.", round(100 * attr(x, "level"))),
                  side = 1, line = 2.2, cex = 0.75, col = "grey30")
  invisible(x)
}

#' Plot a funnel
#'
#' Share of participants reaching each step from the start, one bar per group,
#' with confidence intervals.
#'
#' @param x An `af_funnel` from [af_funnel()].
#' @param main Chart title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' af_plot_funnel(af_funnel(funnel_demo))
af_plot_funnel <- function(x, main = "Share of participants reaching each step") {
  if (!inherits(x, "af_funnel")) stop("`x` must come from af_funnel().", call. = FALSE)
  steps <- attr(x, "steps")
  arms <- unique(x$arm)
  m <- matrix(x$from_start, nrow = length(arms), byrow = TRUE, dimnames = list(arms, steps))
  lo <- matrix(x$from_start_low, nrow = length(arms), byrow = TRUE)
  hi <- matrix(x$from_start_high, nrow = length(arms), byrow = TRUE)
  cols <- rep(palette_af, length.out = length(arms))
  op <- graphics::par(mai = c(0.8, 0.8, 0.6, 0.2), las = 1, bty = "n", cex.axis = 0.85)
  on.exit(graphics::par(op))
  mids <- graphics::barplot(m, beside = TRUE, col = cols, border = NA, ylim = c(0, 1.08), yaxt = "n",
                            main = main, font.main = 2, cex.main = 1, cex.names = 0.85)
  graphics::axis(2, at = seq(0, 1, 0.25), labels = pct_lab(seq(0, 1, 0.25), 0), lwd = 0, lwd.ticks = 0)
  # Whiskers as segments: arrows() warns on zero-length intervals (the first step).
  cap <- 0.15
  graphics::segments(mids, lo, mids, hi, col = "grey20")
  graphics::segments(mids - cap, lo, mids + cap, lo, col = "grey20")
  graphics::segments(mids - cap, hi, mids + cap, hi, col = "grey20")
  graphics::legend("topright", legend = arms, fill = cols, border = NA, bty = "n", cex = 0.85)
  invisible(x)
}
