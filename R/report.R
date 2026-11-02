base64 <- function(raw) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(raw)
  pad <- (3 - n %% 3) %% 3
  m <- matrix(as.integer(c(raw, as.raw(rep(0, pad)))), nrow = 3)
  idx <- rbind(bitwShiftR(m[1, ], 2),
               bitwOr(bitwShiftL(bitwAnd(m[1, ], 3L), 4), bitwShiftR(m[2, ], 4)),
               bitwOr(bitwShiftL(bitwAnd(m[2, ], 15L), 2), bitwShiftR(m[3, ], 6)),
               bitwAnd(m[3, ], 63L))
  out <- chars[as.vector(idx) + 1]
  if (pad) out[(length(out) - pad + 1):length(out)] <- "="
  paste(out, collapse = "")
}

open_png <- function(file, width, height, res = 150) {
  # Try the platform's default bitmap device first (quartz on macOS), then the
  # alternatives: capabilities() can report cairo even when it cannot load.
  types <- unique(c(getOption("bitmapType"), if (isTRUE(capabilities("cairo"))) "cairo",
                    if (isTRUE(capabilities("aqua"))) "quartz"))
  for (type in types) {
    before <- grDevices::dev.cur()
    ok <- tryCatch({
      grDevices::png(file, width = width, height = height, units = "in", res = res, type = type)
      TRUE
    }, warning = function(w) FALSE, error = function(e) FALSE)
    if (ok) return(invisible(type))
    if (grDevices::dev.cur() != before) grDevices::dev.off()
  }
  stop("No PNG graphics device is available to draw the charts.", call. = FALSE)
}

plot_uri <- function(width, height, code) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  open_png(f, width, height)
  tryCatch(force(code), finally = grDevices::dev.off())
  if (!file.exists(f) || file.size(f) == 0) stop("The chart could not be drawn.", call. = FALSE)
  paste0("data:image/png;base64,", base64(readBin(f, "raw", file.size(f))))
}

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

html_table <- function(df) {
  head <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) paste0("<td>", esc(r), "</td>", collapse = ""))
  paste0("<div class=\"scroll\"><table><thead><tr>", head, "</tr></thead><tbody>",
         paste0("<tr>", rows, "</tr>", collapse = ""), "</tbody></table></div>")
}

pc <- function(v, d = 2) ifelse(is.na(v), "", paste0(formatC(100 * v, format = "f", digits = d), "%"))
signed_pc <- function(v, d = 2) ifelse(is.na(v), "", paste0(ifelse(v >= 0, "+", ""), formatC(100 * v, format = "f", digits = d), "%"))
# Differences between two rates are in percentage points, not percent.
signed_pp <- function(v, d = 2) ifelse(is.na(v), "", paste0(ifelse(v >= 0, "+", ""), formatC(100 * v, format = "f", digits = d), " pp"))

#' Write an experiment report
#'
#' Creates a self-contained HTML report (one file, no internet needed to open
#' it) that follows good practice for reporting online experiments: the
#' pre-registered hypothesis and plan if given, the sample ratio check first,
#' effects with confidence intervals rather than significance alone, the
#' funnel, the planned versus achieved sample size, and notes on method.
#'
#' @param x An `af_experiment`.
#' @param file Output HTML file.
#' @param title Report title.
#' @param hypothesis Optional text of the hypothesis, as pre-registered.
#' @param preregistration Optional link to the pre-registration.
#' @param design Optional `af_design` with the planned sample size.
#' @param funnel Optional `af_funnel` for the same experiment.
#' @param level Confidence level.
#' @return `file`, invisibly.
#' @export
#' @examples
#' f <- file.path(tempdir(), "experiment.html")
#' af_report(experiment_demo, f, title = "Checkout button test",
#'           hypothesis = "A clearer call to action increases purchases.",
#'           design = af_design(0.04, 0.10), funnel = af_funnel(funnel_demo))
#' file.exists(f)
#' unlink(f)
af_report <- function(x, file = "experiment-report.html", title = "Experiment report", hypothesis = NULL,
                      preregistration = NULL, design = NULL, funnel = NULL, level = 0.95) {
  res <- af_analyse(x, level = level)
  srm <- attr(res, "srm")
  lvl <- paste0(round(100 * level), "%")

  best <- res[which.max(res$lift), ]
  verdict <- if (srm$mismatch) {
    "The sample ratio check failed, so the results below are not reliable until the cause is found."
  } else if (best$diff_low > 0) {
    sprintf("%s converted at %s against %s for the control: a relative lift of %s (%s interval %s to %s).",
            best$variant, pc(best$rate), pc(best$control_rate), signed_pc(best$lift, 1), lvl,
            signed_pc(best$lift_low, 1), signed_pc(best$lift_high, 1))
  } else {
    sprintf("No variant clearly outperformed the control. The largest estimated lift was %s for %s, with a %s interval from %s to %s, which includes no effect.",
            signed_pc(best$lift, 1), best$variant, lvl, signed_pc(best$lift_low, 1), signed_pc(best$lift_high, 1))
  }

  groups <- data.frame(check.names = FALSE, Group = x$arm, Participants = format(x$n, big.mark = ","),
                       Conversions = format(x$conversions, big.mark = ","),
                       Rate = pc(x$conversions / x$n),
                       `Expected share` = pc(srm$expected / sum(srm$expected), 1),
                       `Observed share` = pc(x$n / sum(x$n), 1))
  effects <- data.frame(check.names = FALSE, Variant = res$variant,
                        `Difference (pp)` = paste0(signed_pp(res$diff), " (", signed_pp(res$diff_low), " to ", signed_pp(res$diff_high), ")"),
                        `Relative lift` = paste0(signed_pc(res$lift, 1), " (", signed_pc(res$lift_low, 1), " to ", signed_pc(res$lift_high, 1), ")"),
                        `Adjusted p` = format.pval(res$p_adjusted, digits = 2),
                        `P(better than control)` = if (!is.null(res$prob_better)) formatC(res$prob_better, format = "f", digits = 3) else "")
  sections <- c(
    if (!is.null(hypothesis) || !is.null(preregistration)) paste0(
      "<section><h2>Hypothesis</h2>", if (!is.null(hypothesis)) paste0("<p>", esc(hypothesis), "</p>"),
      if (!is.null(preregistration)) sprintf("<p>Pre-registration: <a href=\"%s\">%s</a></p>", esc(preregistration), esc(preregistration)),
      "</section>"),
    sprintf("<section><h2>1. Sample ratio check</h2><p class=\"%s\">%s</p>%s</section>",
            if (srm$mismatch) "alert" else "ok",
            if (srm$mismatch) sprintf("Mismatch: chi-squared = %.2f, p = %s. Investigate assignment, tracking and eligibility before using any result.", srm$statistic, format.pval(srm$p_value, digits = 2))
            else sprintf("Passed: chi-squared = %.2f, p = %s. Allocation matches the plan.", srm$statistic, format.pval(srm$p_value, digits = 2)),
            html_table(groups)),
    sprintf("<section><h2>2. Effects</h2><p>Each variant compared with the control group, &ldquo;%s&rdquo;. Intervals are %s; p-values adjusted with Holm's method.</p>%s<img src=\"%s\" alt=\"Relative lift of each variant with confidence intervals\"></section>",
            esc(res$control[1]), lvl, html_table(effects), plot_uri(7, 1.4 + 0.5 * nrow(res), af_plot_lift(res))),
    if (!is.null(funnel)) {
      ft <- data.frame(check.names = FALSE, Group = funnel$arm, Step = funnel$step, Participants = format(funnel$users, big.mark = ","),
                       `From previous step` = pc(funnel$from_previous, 1), `From start` = pc(funnel$from_start, 1),
                       `vs control` = ifelse(is.na(funnel$diff_from_start), "", signed_pp(funnel$diff_from_start)))
      sprintf("<section><h2>3. Funnel</h2><img src=\"%s\" alt=\"Share of participants reaching each funnel step by group\">%s</section>",
              plot_uri(7, 3.6, af_plot_funnel(funnel)), html_table(ft))
    },
    if (!is.null(design)) sprintf(
      "<section><h2>Planned and achieved sample</h2><p>The plan required %s participants in the control and %s per variant to detect a change from %s to %s with %s power at alpha = %s (%s). The experiment reached %s in the control%s.</p></section>",
      format(design$n_control, big.mark = ","), format(design$n_variant, big.mark = ","), pc(design$baseline), pc(design$target),
      pc(design$power, 0), design$alpha, design$alternative, format(x$n[1], big.mark = ","),
      if (x$n[1] < design$n_control) " - fewer than planned, so the experiment is underpowered for the planned effect" else "")
  )
  css <- paste(
    "body{margin:0;background:#fff;color:#1d1c1a;font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;line-height:1.5}",
    "main{max-width:880px;margin:0 auto;padding:32px 20px 56px}h1{margin:0;font-size:1.7rem}",
    "header{border-bottom:4px solid #0072B2;padding-bottom:12px}h2{color:#0072B2;font-size:1.2rem;margin-top:34px}",
    ".summary{font-size:1.1rem;background:#f3f7fb;border-left:4px solid #0072B2;padding:12px 16px;margin-top:20px}",
    ".alert{border-left:4px solid #b3261e;padding:8px 12px;background:#fdf1f0}.ok{border-left:4px solid #007A5E;padding:8px 12px;background:#eef8f4}",
    "img{max-width:100%;height:auto}.scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.88rem;margin:8px 0}",
    "th,td{text-align:left;padding:5px 8px;border-bottom:1px solid #e2dfda}.notes{font-size:.85rem;color:#3a3734}footer{margin-top:40px;font-size:.8rem;color:#5f5c57}")
  page <- paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", esc(title), "</title><style>", css, "</style></head><body><main><header><h1>", esc(title), "</h1></header>",
    "<p class=\"summary\">", esc(verdict), "</p>", paste(sections, collapse = ""),
    "<section class=\"notes\"><h2>Method</h2><ul>",
    "<li>Rates with Wilson intervals; differences with Newcombe's hybrid score intervals; relative lift with a log-scale interval for the risk ratio.</li>",
    "<li>Two-sided tests of proportions without continuity correction, adjusted for the number of variants with Holm's method.</li>",
    "<li>Probability of being better than the control: Bayesian, with uniform Beta(1, 1) priors, computed by numerical integration.</li>",
    "<li>Sample ratio check: chi-squared goodness of fit against the planned allocation, flagged when p &lt; 0.001.</li>",
    "<li>Results are valid for the population, period and metric of this experiment. Report the full plan, all variants and all pre-registered metrics, not only those that moved.</li>",
    "</ul></section><footer>Made with abfunnel ", as.character(utils::packageVersion("abfunnel")), " on ", format(Sys.Date()), ".</footer></main></body></html>\n")
  writeLines(page, file, useBytes = TRUE)
  invisible(file)
}

#' Copy a pre-registration template
#'
#' Copies a Markdown template for pre-registering an online experiment before
#' it starts: hypotheses, primary and secondary metrics, population and unit of
#' randomisation, sample size and stopping rule, analysis plan and exclusion
#' criteria. Fill it in and deposit it (for example on OSF or Zenodo) to
#' obtain a time-stamped record.
#'
#' @param dir Folder where the template is copied.
#' @param overwrite Overwrite an existing file.
#' @return The path of the copied file, invisibly.
#' @export
#' @examples
#' p <- af_preregistration_template(tempdir(), overwrite = TRUE)
#' head(readLines(p))
#' unlink(p)
af_preregistration_template <- function(dir = ".", overwrite = FALSE) {
  src <- system.file("templates", "preregistration.md", package = "abfunnel", mustWork = TRUE)
  dest <- file.path(dir, "preregistration.md")
  if (file.exists(dest) && !overwrite) stop(dest, " exists; use overwrite = TRUE.", call. = FALSE)
  file.copy(src, dest, overwrite = TRUE)
  invisible(dest)
}
