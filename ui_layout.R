rq1_panel <- nav_panel(title = "Importance of Factors",
          layout_sidebar(
            sidebar = sidebar(
              radioButtons("plot_type", "Plot type:",
                           choices = c("Rating distribution", "Mean rating")),
              checkboxGroupInput("factors", "Factors to show:",
                                 choices = unname(importance_labels),
                                 selected = unname(importance_labels)),
              conditionalPanel(
                condition = "input.plot_type == 'Mean rating'",
                colourInput("bar_colour", "Bar colour:", value = "#2C6E91")
              )
            ),
            navset_card_underline(title = "How important is each factor when choosing housing?",
              nav_panel("Stacked Bar Chart" , withSpinner(plotOutput("importance_plot"))),
              nav_panel("Grouped Bar Chart", withSpinner(plotOutput("importance_grpbr_plot")))
            ),
            card(
              card_header("Friedman test (all five factors)"),
              textOutput("friedman_text")
            )
          ))