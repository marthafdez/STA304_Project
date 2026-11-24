# ---- Summary: Histograms -----

#| fig-cap: "Distribution of housing satisfaction (n = 58). The dashed line marks the scale midpoint of 5.5, the hypothesised mean in our one-sample t-test, and the solid line marks the sample mean. Almost every student scores above the midpoint, so mean satisfaction appears well above neutral. The distribution is left-skewed and bunched at the top of the scale."
#| fig-height: 3.2
#| fig-width: 6.5
hist_labels <- c(housing_satisfaction = "housing satisfaction",
                 rent_importance      = "importance of rent",
                 distance_importance  = "importance of distance to campus",
                 commute_importance   = "importance of commute time",
                 roommates_importance = "importance of roommates",
                 amenities_importance = "importance of amenities")

output$hist <- renderPlot({
  req(input$hist_var)
  
  x       <- survey[[input$hist_var]]
  is_sat  <- input$hist_var == "housing_satisfaction"
  max_val <- if (is_sat) 10 else 5          # satisfaction is 1-10, importance items are 1-5
  mid_val <- (1 + max_val) / 2              # scale midpoint: 5.5 or 3
  x_lab   <- if (is_sat) "Housing satisfaction (1 = very dissatisfied, 10 = very satisfied)"
  else "Importance (1 = least important, 5 = most important)"
  
  ggplot(survey, aes(x = .data[[input$hist_var]])) +
    geom_bar(fill = "#2C6E91", alpha = 0.85, width = 0.8) +
    geom_vline(xintercept = mid_val, linetype = "dashed", colour = "#C0392B", linewidth = 0.8) +
    geom_vline(xintercept = mean(x, na.rm = TRUE), colour = "#1B2631", linewidth = 0.8) +
    annotate("text", x = mid_val, y = Inf, label = sprintf("Scale midpoint = %s", mid_val),
             vjust = 1.5, hjust = 1.05, size = 3.3, colour = "#C0392B") +
    annotate("text", x = mean(x, na.rm = TRUE), y = Inf,
             label = sprintf("Sample mean = %.2f", mean(x, na.rm = TRUE)),
             vjust = 3, hjust = 1.05, size = 3.3) +
    scale_x_continuous(breaks = 1:max_val, limits = c(0.5, max_val + 0.5)) +
    labs(x = x_lab, y = "Number of respondents",
         title = paste("Distribution of", hist_labels[[input$hist_var]]),
         subtitle = sprintf("n = %d", sum(!is.na(x)))) +
    theme_minimal(base_size = 14)
})


# ---- RQ1: Stacked Bar -----

output$importance_plot <- renderPlot({
  req(input$factors)
  
  d <- importance_long |> filter(factor %in% input$factors)
  means <- d |> group_by(factor) |> summarise(mean = mean(rating))
  lv <- means$factor[order(means$mean)]          # order factors by mean rating
  d$factor     <- factor(d$factor, levels = lv)
  means$factor <- factor(means$factor, levels = lv)
  
  if (input$plot_type == "Rating distribution") {
    d$rating <- factor(d$rating, levels = 5:1)
    ggplot(d, aes(y = factor, fill = rating)) +
      geom_bar(position = "fill", width = 0.7) +
      geom_text(data = means, aes(y = factor, x = 1.06, label = sprintf("%.2f", mean)),
                inherit.aes = FALSE, size = 4) +
      scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25),
                         expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = c("5" = "#1B4F72", "4" = "#5DADE2", "3" = "#D5D8DC",
                                   "2" = "#F0B27A", "1" = "#BA4A00"),
                        breaks = as.character(1:5), name = "Importance\n(1 = least, 5 = most)") +
      labs(x = "Share of respondents", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
  } else {
    # ---- Mean bar plot ----
    ggplot(means, aes(x = mean, y = factor)) +
      geom_col(fill = input$bar_colour, width = 0.7) +
      geom_text(aes(label = sprintf("%.2f", mean)), hjust = -0.2, size = 4) +
      scale_x_continuous(limits = c(0, 5.3), breaks = 0:5) +
      labs(x = "Mean importance rating (1 = least, 5 = most)", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
  }
})

# ---- RQ1: Grouped Bar Plot ----
output$importance_grpbr_plot <- renderPlot({
  req(input$factors)
  
  d <- importance_long |> filter(factor %in% input$factors)
  means <- d |> group_by(factor) |> summarise(mean = mean(rating))
  lv <- means$factor[order(means$mean)]          # same factor order as the stacked plot
  d$factor <- factor(d$factor, levels = lv)
  d$rating <- factor(d$rating, levels = 1:5)     # 5 at the top of each group
  
  ggplot(d, aes(y = factor, fill = rating)) +
    geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = importance_colours, drop = FALSE,
                      name = "Importance\n(1 = least, 5 = most)") +
    labs(x = "Number of respondents", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank())
})

# ---- RQ1: Text description (Need to change) ----
output$friedman_text <- renderText({
  sprintf("Friedman chi-squared = %.2f, df = %d, p %s. %s",
          rq1_friedman$statistic, as.integer(rq1_friedman$parameter),
          ifelse(rq1_friedman$p.value < .001, "< .001",
                 paste("=", formatC(rq1_friedman$p.value, format = "f", digits = 3))),
          "A small p-value means the five factors are not all rated equally important.")
})


# ---- Helper: add "(n = ...)" to each housing type label ----
with_n_labels <- function(d) {
  lab <- d |> count(housing_type) |> mutate(lab = paste0(housing_type, "\n(n = ", n, ")"))
  d$grp <- factor(lab$lab[match(d$housing_type, lab$housing_type)], levels = lab$lab)
  d
}

# ---- RQ2: housing type by student status ----
rq2_data <- reactive({
  if (isTRUE(input$rq2_incl_res)) survey else survey_no_residence
})

output$rq2_pct_plot <- renderPlot({
  d <- with_n_labels(rq2_data())
  counts <- d |> count(grp, student_status) |> group_by(grp) |>
    mutate(pct = n / sum(n)) |> ungroup()
  
  ggplot(counts, aes(x = pct, y = grp, fill = student_status)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(pct < 0.15, as.character(n), sprintf("%d (%.0f%%)", n, 100 * pct)),
                  colour = student_status),
              position = position_stack(vjust = 0.5), size = 4, show.legend = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = status_colours, name = "Student status") +
    scale_colour_manual(values = c("Domestic" = "#1B2631", "International" = "#FFFFFF")) +
    labs(x = "Share of students in each housing type", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank())
})

output$rq2_count_plot <- renderPlot({
  d <- with_n_labels(rq2_data())
  counts <- d |> count(grp, student_status, .drop = FALSE)
  
  ggplot(counts, aes(x = n, y = grp, fill = student_status)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.8) +
    geom_text(aes(label = n), position = position_dodge(width = 0.8), hjust = -0.3, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = status_colours, name = "Student status") +
    labs(x = "Number of respondents", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank())
})

output$rq2_text <- renderText({
  d    <- rq2_data()
  tab  <- table(d$housing_type, d$student_status)
  chi  <- suppressWarnings(chisq.test(tab))
  fish <- fisher.test(tab)
  min_expected <- min(chi$expected)
  sprintf("Chi-square = %.2f, df = %d, p %s. Fisher's exact test: p %s. Smallest expected count = %.2f. %s %s",
          chi$statistic, as.integer(chi$parameter), format_p(chi$p.value), format_p(fish$p.value),
          min_expected,
          if (min_expected < 5) "Some expected counts are below 5, so rely on Fisher's exact test rather than the chi-square p-value." else "",
          if (fish$p.value < .05) "Housing type and student status appear to be related."
          else "There is no clear evidence that housing type and student status are related.")
})

# ---- RQ3: housing satisfaction by housing type ----
rq3_data <- reactive({
  if (isTRUE(input$rq3_incl_res)) survey else survey_no_residence
})

output$rq3_box_plot <- renderPlot({
  d <- rq3_data()
  ggplot(d, aes(x = housing_type, y = housing_satisfaction, fill = housing_type)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6) +
    geom_jitter(position = position_jitter(width = 0.12, height = 0.12, seed = 1),
                alpha = 0.5, size = 1.6) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
    scale_fill_manual(values = housing_colours) +
    scale_y_continuous(breaks = 1:10, limits = c(0.5, 10.5)) +
    labs(x = "Housing type", y = "Housing satisfaction (1 to 10)",
         subtitle = "Diamonds mark group means") +
    guides(fill = "none") +
    theme_minimal(base_size = 14)
})

output$rq3_dist_plot <- renderPlot({
  d <- with_n_labels(rq3_data())
  ggplot(d, aes(x = housing_satisfaction, fill = housing_type)) +
    geom_bar(width = 0.8) +
    facet_wrap(~ grp, ncol = 1) +
    scale_x_continuous(breaks = 1:10, limits = c(0.5, 10.5)) +
    scale_fill_manual(values = housing_colours) +
    labs(x = "Housing satisfaction (1 = very dissatisfied, 10 = very satisfied)",
         y = "Number of respondents") +
    guides(fill = "none") +
    theme_minimal(base_size = 14)
})

output$rq3_ci_plot <- renderPlot({
  d  <- rq3_data()
  ci <- d |>
    group_by(housing_type) |>
    summarise(n = n(), mean = mean(housing_satisfaction),
              half = qt(0.975, n - 1) * sd(housing_satisfaction) / sqrt(n), .groups = "drop") |>
    mutate(lo = mean - half, hi = mean + half)
  overall <- mean(d$housing_satisfaction)
  
  lo_min <- suppressWarnings(min(ci$lo, na.rm = TRUE))
  hi_max <- suppressWarnings(max(ci$hi, na.rm = TRUE))
  if (!is.finite(lo_min)) lo_min <- 5
  if (!is.finite(hi_max)) hi_max <- 10
  x_upper <- max(10, ceiling(hi_max))
  x_lower <- if (identical(input$rq3_ci_axis, "Full scale (1-10)")) 1 else max(1, min(5, floor(lo_min)))
  
  ggplot(ci, aes(y = housing_type, colour = housing_type)) +
    geom_vline(xintercept = overall, linetype = "dashed", colour = "#C0392B", linewidth = 0.8) +
    geom_segment(aes(x = lo, xend = hi, yend = housing_type), linewidth = 1.2) +
    geom_point(aes(x = mean), size = 4) +
    annotate("text", x = overall, y = Inf, label = sprintf("Overall mean = %.2f", overall),
             vjust = 1.5, hjust = -0.05, size = 3.5, colour = "#C0392B") +
    scale_colour_manual(values = housing_colours) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_discrete(limits = rev) +
    coord_cartesian(xlim = c(x_lower, x_upper)) +
    labs(x = "Mean housing satisfaction (95% confidence interval)", y = NULL) +
    guides(colour = "none") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank())
})

output$rq3_text <- renderText({
  d   <- rq3_data()
  a   <- summary(aov(housing_satisfaction ~ housing_type, data = d))[[1]]
  kw  <- kruskal.test(housing_satisfaction ~ housing_type, data = d)
  dev <- abs(d$housing_satisfaction - ave(d$housing_satisfaction, d$housing_type, FUN = median))
  lev <- summary(aov(dev ~ d$housing_type))[[1]]
  p_aov <- a[["Pr(>F)"]][1]
  sprintf("ANOVA: F(%d, %d) = %.2f, p %s. Kruskal-Wallis: chi-squared = %.2f, p %s. Levene's test for equal variances: p %s. %s",
          as.integer(a[["Df"]][1]), as.integer(a[["Df"]][2]), a[["F value"]][1], format_p(p_aov),
          unname(kw$statistic), format_p(kw$p.value), format_p(lev[["Pr(>F)"]][1]),
          if (isTRUE(p_aov < .05)) "Average satisfaction differs between at least two housing types."
          else "There is no clear evidence that average satisfaction differs by housing type.")
})

# ---- Regression (MLR) ----
mlr_term_labels <- c("housing_typeLiving with family"   = "Living with family (vs off-campus)",
                     "housing_typeLiving with roommate" = "Living with roommate (vs off-campus)",
                     "cost_bracket"                     = "Monthly housing cost",
                     "commute_time"                     = "Commute time",
                     "distance_importance"              = "Importance of distance",
                     "amenities_importance"             = "Importance of amenities",
                     "roommates_importance"             = "Importance of roommates")

mlr_coefs <- reactive({
  d <- survey_no_residence
  if (!identical(input$mlr_scale, "Original units")) {
    d <- d |> mutate(across(c(cost_bracket, commute_time, distance_importance,
                              amenities_importance, roommates_importance),
                            \(v) as.numeric(scale(v))))
  }
  m   <- lm(mlr_formula, data = d)
  est <- coef(summary(m))
  ci  <- confint(m)
  out <- data.frame(term = rownames(est), estimate = est[, "Estimate"],
                    lo = ci[, 1], hi = ci[, 2], p = est[, "Pr(>|t|)"], row.names = NULL)
  out <- out[out$term != "(Intercept)", ]
  out$label <- factor(unname(mlr_term_labels[out$term]), levels = rev(unname(mlr_term_labels[out$term])))
  out$sig   <- factor(ifelse(out$p < .05, "p < .05", "p >= .05"), levels = c("p < .05", "p >= .05"))
  out
})

output$mlr_coef_plot <- renderPlot({
  cf <- mlr_coefs()
  per_sd <- !identical(input$mlr_scale, "Original units")
  ggplot(cf, aes(x = estimate, y = label, colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_segment(aes(x = lo, xend = hi, yend = label), linewidth = 1.1) +
    geom_point(size = 3.5) +
    geom_text(aes(label = sprintf("%.2f", estimate)), vjust = -1, size = 3.6, show.legend = FALSE) +
    scale_colour_manual(values = c("p < .05" = "#2C6E91", "p >= .05" = "#888780"),
                        name = NULL, drop = FALSE) +
    labs(x = if (per_sd) "Change in satisfaction (points) per 1 SD increase"
         else "Change in satisfaction (points) per one-unit increase",
         y = NULL, subtitle = "Dots are estimates, lines are 95% confidence intervals") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
})

output$mlr_comparison <- renderTable({
  model_comparison
}, digits = 2)

output$mlr_text <- renderText({
  s <- summary(model_mlr_full)
  f <- s$fstatistic
  p <- unname(pf(f[1], f[2], f[3], lower.tail = FALSE))
  sel <- attr(terms(model_mlr_selected), "term.labels")
  sprintf("n = %d. R-squared = %.3f (adjusted %.3f). Overall F(%d, %d) = %.2f, p %s. Backward AIC selection kept: %s.",
          as.integer(nobs(model_mlr_full)), s$r.squared, s$adj.r.squared,
          as.integer(f[2]), as.integer(f[3]), unname(f[1]), format_p(p),
          if (length(sel)) paste(sel, collapse = ", ") else "no predictors (mean only)")
})