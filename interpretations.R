# =============================================================================
# interpretations.R  --  plain-language "What does this mean?" text (Bahati)
# =============================================================================
# Each interpret_*() function takes the data currently shown and returns HTML
# for renderUI(). The text is recomputed whenever its inputs change, so it
# always describes the students on screen.
#
#   Summary tab ........ interpret_variable(data, var)
#   Importance tab ..... interpret_importance(data, factors)
#   RQ2 tab ............ interpret_status_housing(data, include_residence)
#   RQ3 tab ............ interpret_satisfaction_by_type(data, include_residence)
#   Regression tab ..... interpret_regression(data)
#
# The server outputs that call these are at the bottom of server_logic.R. They
# all read one reactive, shown_data() (top of server_logic.R). When the
# demographic filter is added, change that one line to return the filtered data
# and every interpretation follows the filter.
#
# The functions accept the raw survey (codes 0, 1, 2, ...) or the labelled data
# from app.R, and they never error on small or empty filtered groups: below
# interp_min_n students they describe the data but skip the test.
# =============================================================================

library(shiny)

interp_min_n <- 10        # fewest students for running a test
interp_min_n_mlr <- 20    # fewest students for fitting the regression


# ---- Readable names ----------------------------------------------------------

interp_vars <- list(
  housing_satisfaction = list(label = "housing satisfaction", max = 10),
  rent_importance      = list(label = "the importance of rent", max = 5),
  distance_importance  = list(label = "the importance of distance to campus", max = 5),
  commute_importance   = list(label = "the importance of commute time", max = 5),
  amenities_importance = list(label = "the importance of amenities", max = 5),
  roommates_importance = list(label = "the importance of roommates", max = 5)
)

interp_factor_names <- c(distance_importance  = "Distance to campus",
                         rent_importance      = "Rent",
                         commute_importance   = "Commute time",
                         amenities_importance = "Amenities",
                         roommates_importance = "Roommates")

# How each housing type reads in a sentence
interp_housing_phrase <- c("Residence"            = "live in residence",
                           "Off-campus"           = "rent off-campus",
                           "Living with family"   = "live with family",
                           "Living with roommate" = "live with roommates")

interp_cost_labels <- c("I do not pay for housing", "Less than $500", "$501 to $1000",
                        "$1001 to $1500", "$1501 to $2000", "$2001 to $2500",
                        "More than $2500")

# Regression terms in plain words
interp_term_phrase <- c(
  "housing_typeLiving with family"   = "living with family (compared with renting off-campus)",
  "housing_typeLiving with roommate" = "living with roommates (compared with renting off-campus)",
  "cost_bracket"                     = "monthly housing cost",
  "commute_time"                     = "commute time",
  "distance_importance"              = "how important distance to campus is to the student",
  "amenities_importance"             = "how important amenities are to the student",
  "roommates_importance"             = "how important roommates are to the student")


# ---- Helpers -----------------------------------------------------------------

# Give coded columns their code book labels (only if they are still numbers)
interp_prepare <- function(data) {
  data <- as.data.frame(data)
  names(data) <- trimws(gsub("[\r\n]", "", names(data)))
  data <- data[, names(data) != "" & !is.na(names(data)), drop = FALSE]
  if ("gender" %in% names(data) && is.numeric(data$gender))
    data$gender <- factor(data$gender, levels = 0:2, labels = c("Female", "Male", "Other"))
  if ("student_status" %in% names(data) && is.numeric(data$student_status))
    data$student_status <- factor(data$student_status, levels = 0:1,
                                  labels = c("Domestic", "International"))
  if ("housing_type" %in% names(data) && is.numeric(data$housing_type))
    data$housing_type <- factor(data$housing_type, levels = 0:3,
                                labels = names(interp_housing_phrase))
  if ("cost" %in% names(data) && is.numeric(data$cost))
    data$cost <- factor(data$cost, levels = 0:6, labels = interp_cost_labels)
  data
}

interp_p <- function(p) {
  if (is.na(p)) return("p not available")
  if (p < 0.001) "p < .001" else paste0("p = ", sub("^0", "", sprintf("%.3f", p)))
}

interp_pct <- function(x) paste0(round(100 * x), "%")
interp_num <- function(x) formatC(x, format = "f", digits = 1)
interp_s   <- function(n) if (n == 1) "" else "s"

# "How to read this chart" line, shown first in every card
interp_howto <- function(text) tags$p(tags$strong("How to read the chart: "), text)

interp_no_data <- function() {
  tags$p(tags$em("No students match the current filter. Widen the filter to see results."))
}

interp_too_few <- function(n, what) {
  tags$p(tags$em(sprintf(
    "Only %d student%s match the current filter, which is too few for %s. The numbers above describe these students, but treat them with caution.",
    n, interp_s(n), what)))
}

interp_observational <- function() {
  tags$p(tags$em(
    "This is an observational survey of volunteers, so it can show that two things go together, not that one causes the other. See the About tab for other caveats."))
}

interp_drop_residence <- function(data, include_residence) {
  if (isTRUE(include_residence)) return(data)
  data[is.na(data$housing_type) | data$housing_type != "Residence", , drop = FALSE]
}


# ---- Summary tab: one variable -----------------------------------------------

interpret_variable <- function(data, var) {
  data <- interp_prepare(data)
  req(var %in% names(interp_vars), var %in% names(data))

  info <- interp_vars[[var]]
  x <- data[[var]]
  x <- x[!is.na(x)]
  n <- length(x)
  is_sat <- info$max == 10
  mid <- (1 + info$max) / 2

  howto <- interp_howto(sprintf(
    "each bar shows how many students (or what percent of them, if you switch the y-axis) gave that rating, from 1 (%s) to %d (%s). The dashed red line is the middle of the scale (%s) and the solid line is the average. Use the slider to zoom in on part of the scale.",
    if (is_sat) "very dissatisfied" else "least important", info$max,
    if (is_sat) "very satisfied" else "most important", format(mid)))
  if (n == 0) return(tagList(howto, interp_no_data()))

  high_cut <- if (is_sat) 8 else 4
  low_cut  <- if (is_sat) 3 else 2
  out <- list(howto, tags$p(sprintf(
    "For the %d student%s shown, the average for %s is %s out of %d, and half of them gave %s or higher. %s gave a high rating (%s) and %s gave a low one (%s).",
    n, interp_s(n), info$label, interp_num(mean(x)), info$max, format(median(x)),
    interp_pct(mean(x >= high_cut)), if (is_sat) "8 to 10" else "4 or 5",
    interp_pct(mean(x <= low_cut)), if (is_sat) "1 to 3" else "1 or 2")))

  if (!is_sat) {
    out <- c(out, list(tags$p(sprintf(
      "The average is %s the middle of the scale (3), so these students %s.",
      if (mean(x) > mid) "above" else if (mean(x) < mid) "below" else "exactly at",
      if (mean(x) > mid) "generally see this factor as important when choosing housing"
      else "do not see this factor as especially important"))))
    return(tagList(out))
  }

  # The proposal's simple test: is average housing satisfaction different from 5.5?
  if (n < interp_min_n) return(tagList(c(out, list(interp_too_few(n, "the test against a neutral score")))))
  if (sd(x) == 0) return(tagList(c(out, list(tags$p("Every student shown gave the same rating, so there is nothing to test.")))))

  tt <- t.test(x, mu = 5.5)
  verdict <- if (tt$p.value < 0.05) {
    sprintf("Yes. The average (%s) is clearly %s the middle of the scale (5.5). If these students were neutral on average, an average this far from 5.5 would be very unlikely (one-sample t-test, %s).",
            interp_num(mean(x)), if (mean(x) > 5.5) "above" else "below", interp_p(tt$p.value))
  } else {
    sprintf("Not clearly. The average (%s) is close enough to the middle of the scale (5.5) that the gap could be due to chance (one-sample t-test, %s).",
            interp_num(mean(x)), interp_p(tt$p.value))
  }
  tagList(c(out, list(tags$p(tags$strong("Are students satisfied with their housing overall? "), verdict))))
}


# ---- RQ1: which factors matter most? -----------------------------------------

interpret_importance <- function(data, factors = NULL) {
  data <- interp_prepare(data)
  cols <- names(interp_factor_names)
  if (!is.null(factors)) cols <- cols[cols %in% factors | interp_factor_names[cols] %in% factors]

  howto <- interp_howto(
    "each row is one factor. In the stacked chart, the dark blue part is the share of students who rated it 5 (most important) and the orange part is the share who rated it 1 or 2; the number on the right is the average rating. The grouped chart shows the same ratings as counts, and the mean chart shows only the averages.")
  if (length(cols) == 0) return(tagList(howto, tags$p(tags$em("Select at least one factor to see an interpretation."))))

  ratings <- data[, cols, drop = FALSE]
  ratings <- ratings[stats::complete.cases(ratings), , drop = FALSE]
  n <- nrow(ratings)
  if (n == 0) return(tagList(howto, interp_no_data()))

  means <- sort(colMeans(ratings), decreasing = TRUE)
  share_high <- colMeans(ratings >= 4)[names(means)]
  avg_items <- lapply(names(means), function(k) tags$li(sprintf(
    "%s: average %s out of 5, rated 4 or 5 by %s", interp_factor_names[[k]],
    interp_num(means[[k]]), interp_pct(share_high[[k]]))))

  out <- list(howto, tags$p(sprintf(
    "Students rated how important each factor was when choosing housing, from 1 (least) to 5 (most). For the %d student%s shown:", n, interp_s(n))),
    tags$ul(avg_items),
    tags$p(if (all(means > 3)) {
      sprintf("%s above the middle of the scale (3), so students treat %s as important.",
              if (length(cols) == 1) "This factor averages" else "Every factor shown averages",
              if (length(cols) == 1) "it" else "all of them")
    } else {
      sprintf("%s at or below the middle of the scale (3): %s.",
              if (sum(means <= 3) == 1) "One factor averages" else "Some factors average",
              paste(interp_factor_names[names(means)[means <= 3]], collapse = ", "))
    }))

  if (length(cols) == 1) return(tagList(c(out, list(tags$p(tags$em("Select two or more factors to compare them."))))))
  if (n < interp_min_n) return(tagList(c(out, list(interp_too_few(n, "comparing the factors")))))

  fr <- friedman.test(as.matrix(ratings))
  if (is.na(fr$p.value)) return(tagList(c(out, list(tags$p("The ratings are too similar to compare with a test.")))))
  q <- tags$strong("Are some factors rated as more important than others? ")
  if (fr$p.value >= 0.05) {
    return(tagList(c(out, list(tags$p(q, sprintf(
      "Not clearly. The differences between these averages are small enough to be explained by chance (Friedman test, %s), so we cannot say that any of these factors matters more than another.",
      interp_p(fr$p.value)))))))
  }

  # The Friedman test only says at least one factor differs. To say which ones,
  # compare every pair (Wilcoxon signed-rank tests) with a Holm correction for
  # making several comparisons.
  pairs <- utils::combn(cols, 2, simplify = FALSE)
  raw_p <- sapply(pairs, function(pr) suppressWarnings(
    wilcox.test(ratings[[pr[1]]], ratings[[pr[2]]], paired = TRUE, exact = FALSE)$p.value))
  raw_p[is.na(raw_p)] <- 1
  adj_p <- p.adjust(raw_p, method = "holm")
  clear <- which(adj_p < 0.05)

  first <- sprintf(
    "The factors are not all rated the same: if students cared about them equally, differences this large would be unlikely (Friedman test, %s). That test does not say which factors differ, so we also compared every pair of factors (%d comparisons), using a stricter cut-off to allow for making so many comparisons (Holm correction).",
    interp_p(fr$p.value), length(pairs))
  second <- if (length(clear) == 0) {
    "No single pair stands out once that correction is made, so we cannot say which factors are rated higher than others. The differences are spread thinly across the factors rather than driven by one clear pair."
  } else {
    lines <- sapply(clear, function(i) {
      a <- pairs[[i]][1]; b <- pairs[[i]][2]
      if (mean(ratings[[a]]) < mean(ratings[[b]])) { tmp <- a; a <- b; b <- tmp }
      sprintf("%s is rated more important than %s (corrected %s)", interp_factor_names[[a]],
              tolower(interp_factor_names[[b]]), interp_p(adj_p[i]))
    })
    paste0("The pairs that differ clearly are: ", paste(lines, collapse = "; "),
           ". Differences between the other pairs could be due to chance.")
  }
  tagList(c(out, list(tags$p(q, first), tags$p(second))))
}


# ---- RQ2: does housing type depend on student status? ------------------------

interpret_status_housing <- function(data, include_residence = FALSE) {
  data <- interp_prepare(data)
  data <- data[!is.na(data$student_status) & !is.na(data$housing_type), , drop = FALSE]
  data <- interp_drop_residence(data, include_residence)
  n <- nrow(data)

  howto <- interp_howto(
    "each bar is one housing type. In the percent chart, the bar is split into domestic and international students, so you can compare who lives where; the counts chart shows the number of students instead.")
  if (n == 0) return(tagList(howto, interp_no_data()))

  statuses <- levels(droplevels(data$student_status))
  describe_group <- function(status) {
    g <- droplevels(data$housing_type[data$student_status == status])
    counts <- sort(table(g), decreasing = TRUE)
    counts <- counts[counts > 0]
    sprintf("%s students (%d): %s.", status, length(g),
            paste(sprintf("%s %s", interp_pct(counts / length(g)), interp_housing_phrase[names(counts)]),
                  collapse = ", "))
  }
  out <- list(howto, tags$p("Where students live, by student status:"),
              tags$ul(lapply(statuses, function(s) tags$li(describe_group(s)))))

  if (length(statuses) < 2) {
    return(tagList(c(out, list(tags$p(tags$em(
      "This comparison needs both domestic and international students, but the current filter keeps only one group."))))))
  }
  if (n < interp_min_n) return(tagList(c(out, list(interp_too_few(n, "testing whether housing type depends on student status")))))
  tab <- table(droplevels(data$housing_type), data$student_status)
  if (nrow(tab) < 2) return(tagList(c(out, list(tags$p("Everyone shown has the same housing type, so there is nothing to test.")))))

  fe <- fisher.test(tab)
  chi_p <- suppressWarnings(chisq.test(tab)$p.value)
  top_type <- function(status) {
    g <- table(droplevels(data$housing_type[data$student_status == status]))
    interp_housing_phrase[[names(g)[which.max(g)]]]
  }
  verdict <- if (fe$p.value < 0.05) {
    sprintf("Yes. If housing type had nothing to do with student status, a difference this large would be very unlikely (Fisher's exact test, %s). The most common choice for domestic students is to %s, while for international students it is to %s.",
            interp_p(fe$p.value), top_type("Domestic"), top_type("International"))
  } else {
    sprintf("Not clearly. The differences between domestic and international students are small enough to be explained by chance (Fisher's exact test, %s).",
            interp_p(fe$p.value))
  }
  tagList(c(out, list(
    tags$p(tags$strong("Does housing type depend on student status? "), verdict),
    tags$p(tags$em(sprintf(
      "We base this answer on Fisher's exact test because some groups are small, and it stays accurate with small counts. The chi-square test in the card below %s (%s).",
      if (is.na(chi_p) || (chi_p < 0.05) == (fe$p.value < 0.05)) "agrees" else "disagrees, which happens when counts are small",
      interp_p(chi_p)))),
    interp_observational())))
}


# ---- RQ3: does satisfaction differ across housing types? ---------------------

interpret_satisfaction_by_type <- function(data, include_residence = FALSE) {
  data <- interp_prepare(data)
  data <- data[!is.na(data$housing_type) & !is.na(data$housing_satisfaction), , drop = FALSE]
  data <- interp_drop_residence(data, include_residence)
  data$housing_type <- droplevels(data$housing_type)

  howto <- interp_howto(
    "in the boxplot, the thick line in each box is the middle (median) rating and the white diamond is the average; each dot is one student. The confidence-interval chart shows each group's average with a line for the range the true average plausibly falls in: when these lines overlap a lot, the groups are hard to tell apart.")
  if (nrow(data) == 0) return(tagList(howto, interp_no_data()))

  sizes <- table(data$housing_type)
  means <- tapply(data$housing_satisfaction, data$housing_type, mean)
  ord <- names(sort(means, decreasing = TRUE))
  avg_items <- lapply(ord, function(k) tags$li(sprintf(
    "Students who %s: average %s out of 10 (%d student%s)", interp_housing_phrase[[k]],
    interp_num(means[[k]]), sizes[[k]], interp_s(sizes[[k]]))))
  out <- list(howto, tags$p("Average housing satisfaction (1 = very dissatisfied, 10 = very satisfied):"),
              tags$ul(avg_items))

  small <- names(sizes)[sizes < 3]
  if (length(small) > 0) {
    out <- c(out, list(tags$p(tags$em(sprintf(
      "%s fewer than 3 students, so %s average is not reliable and %s little weight in the test.",
      paste(sprintf("The group who %s has", interp_housing_phrase[small]), collapse = " and "),
      if (length(small) == 1) "its" else "their", if (length(small) == 1) "carries" else "carry")))))
  }
  if (nlevels(data$housing_type) < 2) {
    return(tagList(c(out, list(tags$p(tags$em("Only one housing type is left under the current filter, so there is nothing to compare."))))))
  }
  if (nrow(data) < interp_min_n) return(tagList(c(out, list(interp_too_few(nrow(data), "comparing housing types")))))

  test_data <- data.frame(satisfaction = data$housing_satisfaction, type = data$housing_type)
  p_anova <- summary(aov(satisfaction ~ type, data = test_data))[[1]][["Pr(>F)"]][1]
  p_kw    <- kruskal.test(satisfaction ~ type, data = test_data)$p.value
  if (is.na(p_anova)) p_anova <- 1

  verdict <- if (p_anova >= 0.05 && p_kw >= 0.05) {
    sprintf("Not clearly. The group averages are at most %s point%s apart, and with groups this size a gap like that could easily happen by chance (one-way ANOVA, %s). A second test that does not assume the ratings follow a bell curve agrees (Kruskal-Wallis, %s). So we have no evidence that satisfaction depends on housing type.",
            interp_num(max(means) - min(means)), if (round(max(means) - min(means), 1) == 1) "" else "s",
            interp_p(p_anova), interp_p(p_kw))
  } else if (p_anova < 0.05 && p_kw < 0.05) {
    sprintf("Yes. Gaps this large would be unlikely if every housing type were equally satisfying (one-way ANOVA, %s; Kruskal-Wallis, %s). These tests show that at least one housing type differs, not which one.",
            interp_p(p_anova), interp_p(p_kw))
  } else {
    sprintf("Unclear. The two tests disagree (one-way ANOVA, %s; Kruskal-Wallis, %s), so any difference between housing types is weak.",
            interp_p(p_anova), interp_p(p_kw))
  }
  tagList(c(out, list(
    tags$p(tags$strong("Does satisfaction differ by housing type? "), verdict),
    tags$p(tags$em("The ANOVA assumes ratings in each group roughly follow a bell curve. Ours are bunched near the top of the scale, which is why we also report the Kruskal-Wallis test.")),
    interp_observational())))
}


# ---- Regression tab: what predicts housing satisfaction? ---------------------

interpret_regression <- function(data) {
  data <- interp_prepare(data)
  n_res <- sum(data$housing_type == "Residence", na.rm = TRUE)
  data <- data[!is.na(data$housing_type) & data$housing_type != "Residence", , drop = FALSE]
  data$housing_type <- droplevels(data$housing_type)
  # Same coding as survey_no_residence in app.R (droplevels() first, so the empty
  # $1501-$2000 bracket is skipped). Keeps this text in step with the plot.
  data$cost_bracket <- as.integer(droplevels(data$cost)) - 1
  vars <- c("housing_satisfaction", "housing_type", "cost_bracket", "commute_time",
            "distance_importance", "amenities_importance", "roommates_importance")
  data <- data[stats::complete.cases(data[, vars]), , drop = FALSE]
  n <- nrow(data)

  howto <- interp_howto(
    "each row is one predictor. The dot shows how much satisfaction changes when that predictor goes up (or compared with renting off-campus, for housing type), and the line shows the range of plausible values. If the line crosses the dashed zero line, the data cannot rule out no effect at all. Blue dots are statistically significant; grey dots are not.")
  if (n == 0) return(tagList(howto, interp_no_data()))
  if (n < interp_min_n_mlr) return(tagList(howto, interp_too_few(n, "fitting a regression with seven terms")))

  preds <- c("cost_bracket", "commute_time", "distance_importance", "amenities_importance", "roommates_importance")
  if (nlevels(data$housing_type) >= 2) preds <- c("housing_type", preds)
  model <- lm(reformulate(preds, "housing_satisfaction"), data = data)
  s <- summary(model)
  f <- s$fstatistic
  p_overall <- unname(pf(f[1], f[2], f[3], lower.tail = FALSE))
  coefs <- coef(s)[-1, , drop = FALSE]
  sig <- rownames(coefs)[coefs[, "Pr(>|t|)"] < 0.05]
  selected <- attr(terms(step(model, direction = "backward", trace = 0)), "term.labels")

  together <- sprintf(
    "Together, the predictors explain about %s of the differences in satisfaction between students (R-squared = %.2f). %s",
    interp_pct(s$r.squared), s$r.squared,
    if (p_overall < 0.05) sprintf("That is more than we would expect by chance (%s), so at least one predictor is related to satisfaction.", interp_p(p_overall))
    else sprintf("That is no more than we would expect from predictors that have nothing to do with satisfaction (%s), so the model as a whole does not predict satisfaction.", interp_p(p_overall)))

  each <- if (length(sig) == 0) {
    "None of the individual predictors is clearly related to satisfaction once the others are taken into account: every confidence line crosses zero. In other words, housing type, cost, commute time and what students say they value do not explain who is more satisfied."
  } else {
    paste0("Taking the other predictors into account, the ones clearly related to satisfaction are: ",
           paste(sapply(sig, function(t) sprintf("%s (%s %s points, %s)",
                                                  if (t %in% names(interp_term_phrase)) interp_term_phrase[[t]] else t,
                                                  if (coefs[t, "Estimate"] > 0) "+" else "",
                                                  sprintf("%.2f", coefs[t, "Estimate"]),
                                                  interp_p(coefs[t, "Pr(>|t|)"]))), collapse = "; "),
           ". The rest could be unrelated.")
  }

  chosen <- if (length(selected) == 0) {
    sprintf("Backward selection by AIC (dropping predictors one at a time while the model's fit-versus-complexity score improves) removes every predictor, so the best model here simply uses the average satisfaction for everyone. %s",
            if (length(sig) == 0) "This agrees with the result above." else "")
  } else {
    sprintf("Backward selection by AIC (dropping predictors one at a time while the model's fit-versus-complexity score improves) keeps: %s.",
            paste(sapply(selected, function(t) {
              hits <- interp_term_phrase[startsWith(names(interp_term_phrase), t)]
              if (t == "housing_type") "housing type" else if (length(hits)) hits[[1]] else t
            }), collapse = ", "))
  }

  tagList(howto,
          tags$p(sprintf("We used multiple linear regression to see whether housing type, monthly housing cost, commute time and the importance students give to distance, amenities and roommates predict housing satisfaction (%d students%s).", n,
                         if (n_res > 0) sprintf("; the %d residence student%s %s left out", n_res, interp_s(n_res), if (n_res == 1) "is" else "are") else "")),
          tags$p(tags$strong("Do these factors predict satisfaction? "), together),
          tags$p(each),
          tags$p(tags$strong("Which model did we choose? "), chosen),
          tags$p(tags$em("Caveats: in the full sample, the spread of satisfaction is not the same at every level of the predictors (one of the regression assumptions), the sample is small, and AIC selection is aimed at prediction rather than at testing which factors matter. Treat these results as exploratory.")))
}
