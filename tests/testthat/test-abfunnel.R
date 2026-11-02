test_that("sample sizes match the standard formula", {
  d <- af_design(0.04, 0.10)
  ref <- stats::power.prop.test(p1 = 0.04, p2 = 0.044, power = 0.8)$n
  expect_lt(abs(d$n_control - ref) / ref, 0.01)
  expect_equal(d$n_variant, d$n_control)
  expect_equal(d$total, 2 * d$n_control)
  abs <- af_design(0.04, 0.004, relative = FALSE)
  expect_equal(abs$n_control, d$n_control)
  one <- af_design(0.04, 0.10, alternative = "greater")
  expect_lt(one$n_control, d$n_control)
  three <- af_design(0.04, 0.10, variants = 2)
  expect_gt(three$n_control, d$n_control)
  expect_equal(three$alpha_per_comparison, 0.025)
  expect_equal(af_design(0.04, 0.10, daily_traffic = 1000)$days, ceiling(d$total / 1000))
  expect_error(af_design(0.95, 0.10), "outside")
  expect_output(print(d), "Experiment plan")
})

test_that("uneven allocation increases the total sample", {
  even <- af_design(0.05, 0.2)
  uneven <- af_design(0.05, 0.2, ratio = 2)
  expect_gt(uneven$total, even$total)
  expect_equal(uneven$n_variant, ceiling(uneven$n_control * 2))
})

test_that("sample ratio mismatch is detected", {
  ok <- af_srm(c(a = 50210, b = 49790))
  bad <- af_srm(c(a = 50900, b = 49100))
  expect_false(ok$mismatch)
  expect_true(bad$mismatch)
  expect_false(af_srm(c(a = 20000, b = 10000), expected = c(2, 1))$mismatch)
  expect_error(af_srm(1), "two or more")
  expect_output(print(bad), "MISMATCH")
})

test_that("af_experiment accepts aggregated and per-participant data", {
  agg <- af_experiment(data.frame(arm = c("A", "B", "A"), n = c(100, 100, 50), conversions = c(5, 9, 2)))
  expect_equal(agg$n, c(150, 100))
  expect_equal(agg$conversions, c(7, 9))
  set.seed(1)
  raw <- data.frame(g = rep(c("B", "A"), each = 50), y = rbinom(100, 1, 0.3))
  ind <- af_experiment(raw, arm = "g", n = NULL, conversions = "y", control = "A")
  expect_equal(ind$arm[1], "A")
  expect_equal(sum(ind$n), 100)
  expect_error(af_experiment(data.frame(arm = "A", n = 1, conversions = 0)), "two groups")
  expect_error(af_experiment(data.frame(arm = c("A", "B"), n = c(10, 10), conversions = c(11, 1))), "between 0")
  expect_error(af_experiment(raw, arm = "g", n = NULL, conversions = "y", control = "Z"), "not found")
})

test_that("analysis recovers the effect in the demo experiment", {
  res <- af_analyse(experiment_demo)
  expect_s3_class(res, "af_result")
  cta <- res[res$variant == "clear_cta", ]
  expect_gt(cta$diff_low, 0)
  expect_true(cta$lift_low < 0.15 && 0.15 < cta$lift_high)
  expect_gt(cta$prob_better, 0.99)
  sp <- res[res$variant == "social_proof", ]
  expect_true(sp$diff_low < 0 && 0 < sp$diff_high)
  expect_true(all(res$p_adjusted >= res$p_value))
  expect_false(attr(res, "srm")$mismatch)
  expect_output(print(res), "Sample ratio check passed")
})

test_that("intervals and probabilities are consistent", {
  x <- af_experiment(data.frame(arm = c("c", "v"), n = c(1000, 1000), conversions = c(50, 50)))
  r <- af_analyse(x)
  expect_equal(r$diff, 0)
  expect_equal(r$lift, 0)
  expect_equal(r$prob_better, 0.5, tolerance = 1e-6)
  expect_true(r$diff_low < 0 && r$diff_high > 0)
  expect_equal(r$diff_high, -r$diff_low, tolerance = 1e-12)
  r2 <- af_analyse(x, bayes = FALSE)
  expect_null(r2$prob_better)
})

test_that("funnels compute step and overall rates", {
  f <- af_funnel(funnel_demo)
  expect_s3_class(f, "af_funnel")
  ctrl <- f[f$arm == "control", ]
  expect_equal(ctrl$from_start[1], 1)
  expect_equal(ctrl$from_previous[3], ctrl$users[3] / ctrl$users[2])
  expect_true(all(is.na(ctrl$diff_from_start)))
  last <- f[f$step == "purchase" & f$arm == "clear_cta", ]
  expect_gt(last$diff_from_start, 0)
  bad <- data.frame(arm = "a", step = c("s1", "s2"), users = c(10, 20))
  expect_error(af_funnel(bad), "more users")
  expect_error(af_funnel(funnel_demo, steps = c("visit", "cart")), "every step")
  expect_output(print(f), "Funnel")
})

test_that("plots draw without error", {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  grDevices::png(f)
  expect_invisible(af_plot_lift(af_analyse(experiment_demo)))
  expect_invisible(af_plot_funnel(af_funnel(funnel_demo)))
  grDevices::dev.off()
  expect_error(af_plot_lift(1), "af_analyse")
})

test_that("reports are self-contained and honest", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  af_report(experiment_demo, f, title = "Test <1>", hypothesis = "H1", design = af_design(0.04, 0.10),
            funnel = af_funnel(funnel_demo), preregistration = "https://example.org/prereg")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "Test &lt;1&gt;", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_match(html, "Sample ratio check", fixed = TRUE)
  expect_match(html, "underpowered", fixed = TRUE)
  skewed <- af_experiment(data.frame(arm = c("c", "v"), n = c(51000, 49000), conversions = c(2000, 2100)))
  af_report(skewed, f)
  expect_match(paste(readLines(f, warn = FALSE), collapse = ""), "not reliable", fixed = TRUE)
})

test_that("base64 and template helpers work", {
  expect_equal(base64(charToRaw("Man")), "TWFu")
  expect_equal(base64(charToRaw("M")), "TQ==")
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  p <- af_preregistration_template(d)
  expect_true(file.exists(p))
  expect_error(af_preregistration_template(d), "exists")
})
