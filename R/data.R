#' A synthetic experiment and its funnel
#'
#' A simulated checkout experiment with about 25,000 participants per group:
#' the control, a clearer call to action (`clear_cta`) and a social proof
#' message (`social_proof`). **All values are synthetic.** The true effects
#' are known, so the data are suited to teaching and to checking an analysis.
#'
#' True probabilities of moving from one step to the next (visit to product,
#' product to cart, cart to checkout, checkout to purchase):
#'
#' | Group | product | cart | checkout | purchase |
#' |---|---|---|---|---|
#' | control | 0.55 | 0.30 | 0.60 | 0.404 |
#' | clear_cta | 0.55 | 0.30 | 0.60 | 0.465 |
#' | social_proof | 0.55 | 0.31 | 0.60 | 0.404 |
#'
#' So `clear_cta` raises purchases by 15% (its effect is at the last step) and
#' `social_proof` by about 3% (through the cart step), an effect this sample
#' is too small to detect reliably.
#'
#' @format
#' `experiment_demo`: an `af_experiment` with columns `arm`, `n` (participants)
#' and `conversions` (purchases).
#'
#' `funnel_demo`: a data frame with columns `arm`, `step` (`visit`, `product`,
#' `cart`, `checkout`, `purchase`) and `users`.
#' @source Simulated with `data-raw/demo.R` in the package sources.
#' @examples
#' experiment_demo
#' head(funnel_demo)
#' @name demo-data
NULL

#' @rdname demo-data
"experiment_demo"

#' @rdname demo-data
"funnel_demo"
