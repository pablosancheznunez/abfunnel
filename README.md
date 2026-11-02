# abfunnel <img src="man/figures/logo.png" align="right" height="139" alt="abfunnel logo" />

[![R-CMD-check](https://github.com/pablosancheznunez/abfunnel/actions/workflows/ci.yml/badge.svg)](https://github.com/pablosancheznunez/abfunnel/actions/workflows/ci.yml)

**Design, check and analyse digital marketing experiments and funnels, to research standards.**

abfunnel follows an online experiment (A/B or A/B/n test) from plan to report:

| Step | Function | What it does |
| --- | --- | --- |
| Plan | `af_design()` | Sample size and duration for the effect you need to detect |
| Register | `af_preregistration_template()` | Template to pre-register hypotheses, metrics and analysis |
| Check | `af_srm()` | Sample ratio mismatch: is the randomisation working? |
| Estimate | `af_experiment()`, `af_analyse()` | Differences and relative lifts with confidence intervals, Holm-adjusted tests, probability of beating the control |
| Funnel | `af_funnel()` | Where in the funnel each variant acts, step by step |
| Report | `af_plot_lift()`, `af_plot_funnel()`, `af_report()` | Charts and a self-contained report that puts effect sizes before significance |

## Installation

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/abfunnel")
```

## Example

```r
library(abfunnel)

af_design(baseline = 0.04, mde = 0.10, daily_traffic = 6000)  # plan

exp <- af_experiment(data.frame(arm = c("control", "new_cta"),
                                n = c(25000, 25000), conversions = c(1000, 1150)))
af_srm(exp)        # check
af_analyse(exp)    # estimate
af_report(exp, "experiment.html", title = "Checkout test")
```

Try it with the built-in synthetic data: `af_analyse(experiment_demo)` and `af_funnel(funnel_demo)`.

## Why

Online experiments are the strongest evidence in digital marketing research, and easy to get wrong: underpowered designs, broken randomisation that nobody checks, uncorrected multiple comparisons and reports with a p-value but no effect size. abfunnel makes each safeguard the default, depends only on base R and produces outputs that can be archived with a pre-registration.

## Learn more

- [Get started](https://pablosancheznunez.com/abfunnel/articles/abfunnel.html)
- [Example report](https://pablosancheznunez.com/abfunnel/example/report.html)
- [Reference](https://pablosancheznunez.com/abfunnel/reference/)

## Citation

`citation("abfunnel")`. MIT License.
