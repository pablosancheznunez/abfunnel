# Pre-registration of an online experiment

Complete this document **before** the experiment starts and deposit it (for
example on OSF or Zenodo) to obtain a time-stamped record. Changes after the
start must be reported as deviations.

## 1. Study information

- Title:
- Authors and affiliations:
- Date of registration:
- Planned start and end dates:

## 2. Hypotheses

State each hypothesis, its direction and the reasoning behind it.

- H1:
- H2 (if any):

## 3. Design

- Variants (describe the control and each variant exactly):
- Unit of randomisation (user, session, account, cluster):
- Allocation (e.g. 50/50):
- Population and eligibility criteria:
- Channel, platform and placement:

## 4. Metrics

- Primary metric (only one), with its exact definition and time window:
- Secondary metrics:
- Guardrail metrics (outcomes that must not get worse):

## 5. Sample size and stopping rule

- Baseline rate and its source:
- Minimum detectable effect and why it matters:
- Significance level, power and sidedness:
- Sample size per group (e.g. from `abfunnel::af_design()`):
- Stopping rule (fixed duration or sample; no early stopping on results):

## 6. Analysis plan

- Sample ratio check (chi-squared, p < 0.001 flags a mismatch):
- Estimate and interval for the primary metric (e.g. `abfunnel::af_analyse()`):
- Correction for multiple variants or metrics:
- Subgroup analyses (pre-specified only):

## 7. Exclusions and data quality

- Exclusion criteria (bots, internal traffic, repeated exposure):
- How missing or duplicated tracking will be handled:

## 8. Other

- Ethics review, consent and privacy:
- Data and code availability:
