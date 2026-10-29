# ---- Summary: Histograms -----

# output$hist <- renderPlot({
#  req(input$hist_var)
#  max_val <- if (input$hist_var == "housing_satisfaction") 10 else 5   # scale ranges
#  
#  ggplot(survey, aes(x = .data[[input$hist_var]])) +
#    geom_bar(fill = "#2C6E91", alpha = 0.85, width = 0.8) +
#    scale_x_continuous(breaks = 1:max_val, limits = c(0.5, max_val + 0.5)) +
#    labs(x = "Rating", y = "Number of respondents") +
#    theme_minimal(base_size = 14)
# })

#| fig-cap: "Distribution of housing satisfaction (n = 58). The dashed line marks the scale midpoint of 5.5, the hypothesised mean in our one-sample t-test, and the solid line marks the sample mean. Almost every student scores above the midpoint, so mean satisfaction appears well above neutral. The distribution is left-skewed and bunched at the top of the scale."
#| fig-height: 3.2
#| fig-width: 6.5
output$hist <- renderPlot({
  ggplot(survey, aes(x = housing_satisfaction)) +
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