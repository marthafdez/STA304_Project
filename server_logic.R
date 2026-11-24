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