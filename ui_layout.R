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
