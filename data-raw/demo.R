# Synthetic experiment and funnel used in examples, tests and the vignette.
# All values are simulated; the true effects are documented in R/data.R.
# Run from the package root after devtools::load_all().

set.seed(20261102)
arms <- c("control", "clear_cta", "social_proof")
n <- c(control = 25000, clear_cta = 25000, social_proof = 25000) + stats::rbinom(3, 200, 0.5) - 100
# True step-by-step probabilities: visit -> product -> cart -> checkout -> purchase.
p_steps <- rbind(
  control      = c(0.55, 0.30, 0.60, 0.4040),
  clear_cta    = c(0.55, 0.30, 0.60, 0.4646),   # +15% at the last step
  social_proof = c(0.55, 0.31, 0.60, 0.4040)    # small change upstream only (+3.3% overall)
)
steps <- c("visit", "product", "cart", "checkout", "purchase")
funnel_demo <- do.call(rbind, lapply(arms, function(a) {
  u <- n[[a]]
  for (p in p_steps[a, ]) u <- c(u, stats::rbinom(1, u[length(u)], p))
  data.frame(arm = a, step = steps, users = u, stringsAsFactors = FALSE)
}))
rownames(funnel_demo) <- NULL
purchases <- funnel_demo$users[funnel_demo$step == "purchase"]
experiment_demo <- af_experiment(data.frame(arm = arms, n = as.numeric(n), conversions = purchases))
save(experiment_demo, file = "data/experiment_demo.rda", compress = "xz")
save(funnel_demo, file = "data/funnel_demo.rda", compress = "xz")
