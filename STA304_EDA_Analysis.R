# =============================================================================
# STA304H5F Group 33: Exploratory Data Analysis
# Factors Affecting Student Housing Choices
#
# Standalone R script with the same analysis as STA304_EDA_Report.qmd.
# Put this file in the same folder as
#   "Factors Affecting Student Housing Survey (Responses).csv"
# and either source() it or run it line by line in RStudio.
# Plots appear in the Plots pane; tables and test results print to the console.
#
# Main objects created by this script:
#   survey                - cleaned data (58 respondents)
#   survey_no_residence   - same data without the single residence respondent (57)
#   model_ttest, ttest_midpoint     - proposal simple test (one-sample t-test vs 5.5)
#   model_mlr_full                  - proposal advanced test (multiple linear regression)
#   model_mlr_selected, model_comparison - model selection by backward stepwise AIC
#   rq1_friedman          - RQ1: which housing factors matter most
#   rq2_chisq, rq2_fisher - RQ2: housing type vs student status
#   model_anova, anova_rq3 - RQ3: housing satisfaction by housing type
#
# One-time installs (run once in the console, not every time):
#   install.packages(c("tidyverse", "easystats", "knitr", "car", "patchwork"))
# =============================================================================

## ----setup--------------------------------------------------------------------
# ---- Packages ----
library(tidyverse)
library(easystats)   # datawizard, performance, report, effectsize, see
library(knitr)

theme_set(theme_minimal(base_size = 11))

# ---- Helper functions ----
# Format a p-value for inline text
format_p <- function(p) ifelse(p < .001, "< .001", paste("=", formatC(p, format = "f", digits = 3)))

# Print a report::report() result with readable variable names instead of code-book names
readable_names <- c(transportation_satisfaction = "transportation satisfaction",
                    housing_satisfaction        = "housing satisfaction",
                    commute_time                = "commute time",
                    housing_type                = "housing type",
                    student_status              = "student status",
                    cost_bracket                = "monthly housing cost (bracket)",
                    distance_importance         = "importance of distance to campus",
                    amenities_importance        = "importance of amenities",
                    roommates_importance        = "importance of roommates")
pretty_report <- function(x) {
  txt <- paste(as.character(x), collapse = "\n")
  for (v in names(readable_names)) {
    txt <- gsub(v, readable_names[[v]], txt, fixed = TRUE)
    # report() sometimes writes the names with spaces already, e.g. "cost bracket"
    txt <- gsub(paste0("effect of ", gsub("_", " ", v)), paste("effect of", readable_names[[v]]),
                txt, fixed = TRUE)
  }
  # report() cannot recover variable names from a table, so add them to the chi-square sentence
  txt <- gsub("independence between\\s+(and\\s+)?suggests",
              "independence between housing type and student status suggests", txt)
  txt <- gsub("\n\\s+- ", "\n\n- ", txt)   # make report's bullets render as a markdown list
  # move report's "Effect sizes were labelled..." notes to the end of the paragraph
  paras <- trimws(strsplit(txt, "\n\\s*\n")[[1]])
  notes <- grepl("^Effect sizes were labelled", paras)
  cat(paste(c(paras[!notes], paras[notes]), collapse = "\n\n"), "\n")
}

# ---- Colours for the housing-type plots ----
housing_colours <- c("Off-campus" = "#2C6E91", "Living with family" = "#E67E22",
                  "Living with roommate" = "#27AE60")


## ----load-clean---------------------------------------------------------------
# ---- Load the raw_export Google Forms export ----
# fileEncoding removes the byte-order mark that the export puts in front of "studentID"
raw_export <- read.csv("Factors Affecting Student Housing Survey (Responses).csv",
                check.names = FALSE, fileEncoding = "UTF-8-BOM")
n_raw_rows <- nrow(raw_export)
n_raw_cols <- ncol(raw_export)

# ---- Fix the column headers ----
# commute_time contains a line break and housing_type has a trailing space
names(raw_export) <- trimws(gsub("[\r\n]", "", names(raw_export)))
raw_export <- raw_export[, names(raw_export) != ""]                       # drop the empty trailing columns
raw_export <- raw_export |> rename(transportation_satisfaction = transportation_satisfacton)  # header typo
n_empty_cols <- n_raw_cols - ncol(raw_export)

# ---- Remove the duplicate submission ----
duplicate_ids <- raw_export$studentID[duplicated(raw_export$studentID)]
survey <- raw_export |> distinct(studentID, .keep_all = TRUE)   # rows are in submission order: keep the first

# ---- Apply the code book labels ----
importance_items <- c("distance_importance", "rent_importance", "commute_importance",
                  "amenities_importance", "roommates_importance")

survey <- survey |>
  mutate(
    gender = factor(gender, levels = 0:2, labels = c("Female", "Male", "Other")),
    student_status = factor(student_status, levels = 0:1,
                            labels = c("Domestic", "International")),
    housing_type = factor(housing_type, levels = 0:3,
                          labels = c("Residence", "Off-campus", "Living with family",
                                     "Living with roommate")),
    cost = factor(cost, levels = 0:6,
                  labels = c("I do not pay for housing", "Less than $500", "$501 to $1000",
                             "$1001 to $1500", "$1501 to $2000", "$2001 to $2500",
                             "More than $2500"))
  )

n_respondents <- nrow(survey)
n_missing <- sum(is.na(survey))

# ---- Range checks against the code book ----
values_in_range <- all(survey[importance_items] >= 1 & survey[importance_items] <= 5) &&
  all(survey$housing_satisfaction %in% 1:10) &&
  all(survey$transportation_satisfaction %in% 1:10) &&
  all(survey$commute_time > 0)


## ----tbl-quant----------------------------------------------------------------
#| tbl-cap: "Summary statistics for the quantitative variables (n = 58)."
quant_var_labels <- c("Importance of distance to campus (1-5)" = "distance_importance",
                "Importance of rent (1-5)"               = "rent_importance",
                "Importance of commute time (1-5)"       = "commute_importance",
                "Importance of amenities (1-5)"          = "amenities_importance",
                "Importance of roommates (1-5)"          = "roommates_importance",
                "Housing satisfaction (1-10)"            = "housing_satisfaction",
                "Commute time (minutes)"                 = "commute_time",
                "Transportation satisfaction (1-10)"     = "transportation_satisfaction")

# centrality = "all" gives Mean and Median (older datawizard versions accept only one value here)
quant_summary <- datawizard::describe_distribution(survey, select = unname(quant_var_labels),
                                               centrality = "all") |>
  as.data.frame() |>
  mutate(Variable = names(quant_var_labels)[match(Variable, quant_var_labels)])

print(
quant_summary |>
  select(Variable, Mean, Median, SD, IQR, Min, Max, n) |>
  kable(digits = 2, format = "pipe")
)


## ----tbl-qual-----------------------------------------------------------------
#| tbl-cap: "Counts and percentages for the qualitative variables (n = 58)."
qual_vars <- survey |>
  select(Gender = gender, `Student status` = student_status,
         `Housing type` = housing_type, `Monthly housing cost` = cost)

qual_summary <- datawizard::data_tabulate(qual_vars, remove_na = TRUE) |>
  map_dfr(as.data.frame) |>
  filter(!is.na(Value)) |>
  group_by(Variable) |>
  mutate(Variable = if_else(row_number() == 1, Variable, "")) |>
  ungroup() |>
  select(Variable, Category = Value, Count = N, `Percent (%)` = `Raw %`)

print(
kable(qual_summary, digits = 1, format = "pipe")
)


## ----stats-inline-------------------------------------------------------------
quant_stats  <- quant_summary |> column_to_rownames("Variable")
count_level <- function(v, lvl) sum(survey[[v]] == lvl)
percent_level <- function(v, lvl) round(100 * mean(survey[[v]] == lvl))


## ----fig-hist-----------------------------------------------------------------
#| fig-cap: "Distribution of housing satisfaction (n = 58). The dashed line marks the scale midpoint of 5.5, the hypothesised mean in our one-sample t-test, and the solid line marks the sample mean. Almost every student scores above the midpoint, so mean satisfaction appears well above neutral. The distribution is left-skewed and bunched at the top of the scale."
#| fig-height: 3.2
#| fig-width: 6.5
fig_hist <- ggplot(survey, aes(x = housing_satisfaction)) +
  geom_bar(fill = "#2C6E91", alpha = 0.85, width = 0.8) +
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = "#C0392B", linewidth = 0.8) +
  geom_vline(xintercept = mean(survey$housing_satisfaction), colour = "#1B2631", linewidth = 0.8) +
  annotate("text", x = 5.5, y = Inf, label = "Scale midpoint = 5.5", vjust = 1.5, hjust = 1.05,
           size = 3.3, colour = "#C0392B") +
  annotate("text", x = mean(survey$housing_satisfaction), y = Inf,
           label = sprintf("Sample mean = %.2f", mean(survey$housing_satisfaction)),
           vjust = 1.5, hjust = 1.05, size = 3.3) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5, 10.5)) +
  labs(x = "Housing satisfaction (1 = very dissatisfied, 10 = very satisfied)",
       y = "Number of respondents", title = "Distribution of housing satisfaction")
print(fig_hist)


## ----fig-importance-----------------------------------------------------------
#| fig-cap: "Distribution of importance ratings for each housing factor, ordered by mean rating (shown on the right). Rent, distance to campus and commute time are rated 4 or 5 by most students. Amenities and roommates get more middling ratings, which suggests these factors matter less when students choose housing."
#| fig-height: 3.4
#| fig-width: 7
importance_labels <- c(distance_importance  = "Distance to campus",
                   rent_importance      = "Rent",
                   commute_importance   = "Commute time",
                   amenities_importance = "Amenities",
                   roommates_importance = "Roommates")

importance_long <- survey |>
  select(all_of(importance_items)) |>
  pivot_longer(everything(), names_to = "factor", values_to = "rating") |>
  mutate(factor = importance_labels[factor])

importance_means <- importance_long |> group_by(factor) |> summarise(mean = mean(rating))
importance_long  <- importance_long |>
  mutate(factor = factor(factor, levels = importance_means$factor[order(importance_means$mean)]),
         rating = factor(rating, levels = 5:1))

fig_importance <- ggplot(importance_long, aes(y = factor, fill = rating)) +
  geom_bar(position = "fill", width = 0.7) +
  geom_text(data = importance_means, aes(y = factor, x = 1.06, label = sprintf("%.2f", mean)),
            inherit.aes = FALSE, size = 3.4) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = c("5" = "#1B4F72", "4" = "#5DADE2", "3" = "#D5D8DC",
                               "2" = "#F0B27A", "1" = "#BA4A00"),
                    breaks = as.character(1:5), name = "Importance\n(1 = least, 5 = most)") +
  labs(x = "Share of respondents", y = NULL,
       title = "How important is each factor when choosing housing?") +
  theme(legend.position = "right", panel.grid.major.y = element_blank())
print(fig_importance)


## ----tbl-cross----------------------------------------------------------------
#| tbl-cap: "Housing type by student status, with row percentages. Most students living with family are domestic (85%), while most students living off-campus (75%) or with roommates (90%) are international, so housing type appears strongly related to student status."
housing_by_status <- survey |>
  count(housing_type, student_status) |>
  group_by(housing_type) |>
  mutate(cell = sprintf("%d (%.0f%%)", n, 100 * n / sum(n))) |>
  select(-n) |>
  pivot_wider(names_from = student_status, values_from = cell, values_fill = "0 (0%)") |>
  left_join(count(survey, housing_type, name = "Total"), by = "housing_type") |>
  select(`Housing type` = housing_type, Domestic, International, Total)

print(
kable(housing_by_status, format = "pipe", align = "lrrr")
)


## ----fig-box------------------------------------------------------------------
#| fig-cap: "Housing satisfaction by housing type (the single residence respondent is excluded). Diamonds mark group means. Medians are 8 to 9 in every group and the boxes overlap heavily. Students living with family are slightly more satisfied on average, and off-campus students show the widest spread, but the differences look small."
#| fig-height: 3.6
#| fig-width: 6.5
survey_no_residence <- survey |> filter(housing_type != "Residence") |> droplevels()

fig_boxplot <- ggplot(survey_no_residence, aes(x = housing_type, y = housing_satisfaction, fill = housing_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.12, height = 0.12, alpha = 0.5, size = 1.4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = housing_colours) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10.3)) +
  labs(x = "Housing type", y = "Housing satisfaction (1 to 10)",
       title = "Housing satisfaction by housing type") +
  guides(fill = "none")
print(fig_boxplot)


## ----fit-models---------------------------------------------------------------
# ---- Proposal simple test: one-sample t-test (H0: mu = 5.5, the midpoint of the 1-10 scale) ----
model_ttest    <- lm(housing_satisfaction ~ 1, data = survey)    # intercept-only model = one-sample t-test
ttest_normality <- performance::check_normality(model_ttest)
# ---- RQ2: chi-square test of independence (residence excluded, n = 1) ----
rq2_table  <- table(survey_no_residence$housing_type, survey_no_residence$student_status)
rq2_chisq  <- chisq.test(rq2_table)    # warning about small expected counts is expected
rq2_min_expected <- min(rq2_chisq$expected)
rq2_fisher   <- fisher.test(table(survey$housing_type, survey$student_status))  # all four housing types

# ---- RQ3: one-way ANOVA ----
model_anova    <- lm(housing_satisfaction ~ housing_type, data = survey_no_residence)
anova_normality <- performance::check_normality(model_anova)
# Levene's test via car directly (performance's print method errors in older versions)
anova_levene  <- car::leveneTest(housing_satisfaction ~ housing_type, data = survey_no_residence)
anova_kruskal   <- kruskal.test(housing_satisfaction ~ housing_type, data = survey_no_residence)


## ----fig-check-t--------------------------------------------------------------
#| fig-cap: "Normality check for the one-sample t-test of housing satisfaction (performance package): Q-Q plot of the deviations from the mean (left) and their density against a normal curve (right)."
#| fig-height: 3.4
#| fig-width: 8
fig_check_ttest <- performance::check_model(model_ttest, check = c("qq", "normality"))
print(fig_check_ttest)


## ----fig-check-aov------------------------------------------------------------
#| fig-cap: "Assumption checks for the one-way ANOVA of housing satisfaction on housing type (performance package). Left: homogeneity of variance (residual spread across fitted values). Middle and right: normality of residuals (Q-Q plot and density against a normal curve)."
#| fig-height: 6
#| fig-width: 8
fig_check_anova <- performance::check_model(model_anova, check = c("homogeneity", "qq", "normality"))
print(fig_check_anova)


## ----mlr-prelim---------------------------------------------------------------
# Multiple linear regression exactly as specified in the proposal:
# housing satisfaction ~ housing arrangement + rent range + commute time + distance to campus
#                        + amenities + roommates
# "Distance to campus", "amenities" and "roommates" are the importance ratings, since those are
# what the survey measured. Monthly housing cost (rent range) enters as its bracket number
# (0 = pays nothing, ..., 6 = over $2500), because some brackets are empty or tiny.
# Residence (n = 1) is excluded.
survey_no_residence <- survey_no_residence |> mutate(cost_bracket = as.integer(cost) - 1)
model_mlr_full <- lm(housing_satisfaction ~ housing_type + cost_bracket + commute_time +
               distance_importance + amenities_importance + roommates_importance,
             data = survey_no_residence)
mlr_normality <- performance::check_normality(model_mlr_full)
mlr_heteroscedasticity  <- performance::check_heteroscedasticity(model_mlr_full)
n_significant     <- sum(parameters::model_parameters(model_mlr_full)$p[-1] < 0.05)


## ----fig-check-mlr------------------------------------------------------------
#| fig-cap: "Assumption checks for the multiple linear regression of housing satisfaction on housing type, monthly housing cost, commute time and the importance of distance to campus, amenities and roommates (performance package). Top: homogeneity of variance. Bottom: normality of residuals (Q-Q plot and density against a normal curve)."
#| fig-height: 6
#| fig-width: 8
fig_check_mlr <- performance::check_model(model_mlr_full, check = c("homogeneity", "qq", "normality"))
print(fig_check_mlr)


## ----test-t, results='asis'---------------------------------------------------
ttest_midpoint  <- t.test(survey$housing_satisfaction, mu = 5.5)   # 5.5 = midpoint of the 1-10 scale
ttest_cohens_d  <- effectsize::cohens_d(survey$housing_satisfaction, mu = 5.5)
ttest_proposal_value   <- t.test(survey$housing_satisfaction, mu = 5)     # value stated in the proposal, for comparison


## ----test-mlr, results='asis'-------------------------------------------------
pretty_report(report::report(model_mlr_full))
mlr_fstat <- summary(model_mlr_full)$fstatistic
mlr_pvalue <- pf(mlr_fstat[1], mlr_fstat[2], mlr_fstat[3], lower.tail = FALSE)


## ----model-selection----------------------------------------------------------
# Model selection method: backward stepwise elimination by AIC (step() in base R).
# Starting from the full model, the predictor whose removal lowers AIC the most is dropped,
# one at a time, until no removal lowers AIC any further.
model_mlr_selected <- step(model_mlr_full, direction = "backward", trace = 0)
selected_predictors <- attr(terms(model_mlr_selected), "term.labels")

# Candidate models compared by AIC, BIC and R-squared
model_mean_only    <- lm(housing_satisfaction ~ 1, data = survey_no_residence)
model_housing_only <- lm(housing_satisfaction ~ housing_type, data = survey_no_residence)
candidate_models <- list("Full model (all six predictors)" = model_mlr_full,
                   "Housing type only"               = model_housing_only,
                   "No predictors (mean only)"       = model_mean_only)
model_comparison <- tibble(Model = names(candidate_models),
                  `Number of terms` = map_int(candidate_models, \(m) length(coef(m)) - 1),
                  R2  = map_dbl(candidate_models, \(m) summary(m)$r.squared),
                  AIC = map_dbl(candidate_models, AIC),
                  BIC = map_dbl(candidate_models, BIC)) |>
  arrange(AIC)


## ----tbl-selection------------------------------------------------------------
#| tbl-cap: "Comparison of candidate models for housing satisfaction (n = 57, residence excluded), ordered from lowest to highest AIC. Lower AIC and BIC indicate a better balance of fit and simplicity. The model with no predictors has the lowest AIC and BIC, so adding predictors does not improve on simply using the mean."
print(
kable(model_comparison, digits = 2, format = "pipe")
)


## ----test-rq1-----------------------------------------------------------------
rq1_friedman <- friedman.test(as.matrix(survey[importance_items]))
rq1_kendalls_w     <- effectsize::kendalls_w(as.matrix(survey[importance_items]))


## ----test-rq2, results='asis'-------------------------------------------------
pretty_report(report::report(rq2_chisq))


## ----test-rq3, results='asis'-------------------------------------------------
anova_rq3 <- aov(housing_satisfaction ~ housing_type, data = survey_no_residence)
pretty_report(report::report(anova_rq3))
anova_rq3_table <- summary(anova_rq3)[[1]]

## ---- Test results (these numbers appear in the text of the report) ----
cat("\n================ RESULTS ================\n")

cat("\n--- Data cleaning ---\n")
cat("Raw rows:", n_raw_rows, " Clean rows:", n_respondents, " Duplicate ID(s) removed:", duplicate_ids,
    " Missing values:", n_missing, " All values in code-book range:", values_in_range, "\n")

cat("\n--- Proposal simple test: one-sample t-test, H0: mean housing satisfaction = 5.5 (scale midpoint) ---\n")
print(ttest_midpoint)
print(ttest_cohens_d); cat("\n")   # Cohen's d
print(ttest_normality)     # Shapiro-Wilk normality check
cat("Same test against 5 (value stated in the proposal): t =", round(ttest_proposal_value$statistic, 2),
    " p =", format.pval(ttest_proposal_value$p.value), "\n")

cat("\n--- Proposal advanced test: multiple linear regression of housing satisfaction ---\n")
print(summary(model_mlr_full))
print(mlr_normality)  # Shapiro-Wilk
print(mlr_heteroscedasticity)   # Breusch-Pagan

cat("\n--- Proposal advanced test: model selection (backward stepwise elimination by AIC) ---\n")
cat("Selected model:", deparse(formula(model_mlr_selected)), "\n")
print(model_comparison)

cat("\n--- RQ1: Friedman test across the five importance ratings ---\n")
print(rq1_friedman)
print(rq1_kendalls_w)

cat("\n--- RQ2: Housing type by student status ---\n")
print(rq2_table)
cat("Smallest expected count:", round(rq2_min_expected, 2), "\n")
print(rq2_chisq)
print(rq2_fisher)

cat("\n--- RQ3: One-way ANOVA of housing satisfaction on housing type ---\n")
print(summary(anova_rq3))
print(anova_normality)   # Shapiro-Wilk normality of residuals
print(anova_levene)    # Levene's test for equal variances (car::leveneTest)
print(anova_kruskal)     # Kruskal-Wallis check
