#ORIGINAL!!!!!
#rq1_panel <- nav_panel(title = "Importance of Factors",
#          layout_sidebar(
#            sidebar = sidebar(
#              radioButtons("plot_type", "Plot type:",
#                           choices = c("Rating distribution", "Mean rating")),
#              checkboxGroupInput("factors", "Factors to show:",
#                                 choices = unname(importance_labels),
#                                 selected = unname(importance_labels)),
#              conditionalPanel(
#                condition = "input.plot_type == 'Mean rating'",
#                colourInput("bar_colour", "Bar colour:", value = "#2C6E91")
#              )
#            ),
#            navset_card_underline(title = "How important is each factor when choosing housing?",
#              nav_panel("Stacked Bar Chart" , withSpinner(plotOutput("importance_plot"))),
#              nav_panel("Grouped Bar Chart", withSpinner(plotOutput("importance_grpbr_plot")))
#            ),
#            card(
#              card_header("Friedman test (all five factors)"),
#              textOutput("friedman_text")
#            )
#          ))

create_rq1_panel <- function(importance_labels) {
  nav_panel(
    title = "Importance of Factors",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "plot_type", "Plot type:",
          choices = c("Rating distribution", "Mean rating")
        ),
        checkboxGroupInput(
          "factors", "Factors to show:",
          choices = unname(importance_labels),
          selected = unname(importance_labels)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Mean rating'",
          colourInput("bar_colour", "Bar colour:",
                      value = "#2C6E91")
        )
      ),
      navset_card_underline(
        title = "How important is each factor when choosing housing?",
        nav_panel("Stacked Bar Chart",
                  withSpinner(plotOutput("importance_plot"))),
        nav_panel("Grouped Bar Chart",
                  withSpinner(plotOutput("importance_grpbr_plot")))
      ),
      card(
        card_header("Friedman test (all five factors)"),
        textOutput("friedman_text")
      )
    )
  )
}
create_rq2_panel <- function() {
  nav_panel(
    title = "Housing type by student status",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("rq2_incl_res", "Include the one residence respondent", value = FALSE),
        helpText("The residence group has only one student, so it is left out by default.")
      ),
      navset_card_underline(
        title = "Is housing type related to student status?",
        nav_panel("Percent stacked",
                  withSpinner(plotOutput("rq2_pct_plot"))),
        nav_panel("Counts",
                  withSpinner(plotOutput("rq2_count_plot")))
      ),
      card(
        card_header("Chi-square test and Fisher's exact test"),
        textOutput("rq2_text")
      )
    )
  )
}

create_rq3_panel <- function() {
  nav_panel(
    title = "Housing satisfaction by housing type",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("rq3_incl_res", "Include the one residence respondent", value = FALSE),
        conditionalPanel(
          condition = "input.rq3_tabs == 'Means with 95% CI'",
          radioButtons("rq3_ci_axis", "Axis range:",
                       choices = c("Zoomed in", "Full scale (1-10)"))
        )
      ),
      navset_card_underline(
        id = "rq3_tabs",
        title = "Does satisfaction differ by housing type?",
        nav_panel("Boxplot",
                  withSpinner(plotOutput("rq3_box_plot"))),
        nav_panel("Distribution by group",
                  withSpinner(plotOutput("rq3_dist_plot", height = "480px"))),
        nav_panel("Means with 95% CI",
                  withSpinner(plotOutput("rq3_ci_plot")))
      ),
      card(
        card_header("One-way ANOVA (with Kruskal-Wallis and Levene's test)"),
        textOutput("rq3_text")
      )
    )
  )
}

create_mlr_panel <- function() {
  nav_panel(
    title = "Regression (MLR)",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("mlr_scale", "Coefficient scale:",
                     choices = c("Per 1 SD of predictor", "Original units")),
        helpText("Per 1 SD puts the numeric predictors on a comparable scale. Significance is the same either way.")
      ),
      navset_card_underline(
        title = "What predicts housing satisfaction?",
        nav_panel("Coefficient plot",
                  withSpinner(plotOutput("mlr_coef_plot"))),
        nav_panel("Model comparison",
                  tableOutput("mlr_comparison"))
      ),
      card(
        card_header("Full model fit"),
        textOutput("mlr_text")
      )
    )
  )
}

create_about_panel <- function() {
  nav_panel(
    title = "About",
    h3("About the Factors Affecting Student Housing Survey"),
    
    h4("The survey"),
    p("This app presents the results of our survey on factors affecting student housing. 
       We asked students about their student status, how important different factors 
       are when choosing housing, their current housing type, their commute time, 
       and their satisfaction with each factor."),
    
    h4("The sample"),
    p("We collected 58 responses through Google Forms between October 8 and 
       October 11, 2026. Respondents are students taking STA304 in Fall 2026. 
       As an incentive, our group offered to complete the survey of any student 
       who completed ours."),
    
    h4("Caveats"),
    tags$ul(
      tags$li("The sample is not truly random. We used voluntary response sampling, 
               so students who chose to answer may differ from those who did not."),
      tags$li("Many respondents answered in exchange for our group completing their 
               survey, so they may have been less interested in the topic or answered 
               more quickly."),
      tags$li("Only students in STA304 were surveyed, so they may not represent 
               all students."),
      tags$li("Some groups are small, so results for narrow filters may be unreliable."),
      tags$li("Answers are self-reported and may not be perfectly accurate."),
      tags$li("The results describe our respondents only and should not be 
               generalized to all students.")
    )
  )
}