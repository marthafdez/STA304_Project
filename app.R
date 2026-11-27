library(shiny)
library(bslib)
library(shinycssloaders) # loading screens
library(colourpicker)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# thematic::thematic_shiny() was removed: it shrank every axis label to ~6px and the
# plots set their own colours, so it had no other visible effect.

# ---- Load survey data (same cleaning steps as the EDA script) ----
# Keep the CSV in the same folder as app.R
raw_export <- read.csv("a1.csv",
                       check.names = FALSE, fileEncoding = "UTF-8-BOM")
names(raw_export) <- trimws(gsub("[\r\n]", "", names(raw_export)))
raw_export <- raw_export[, names(raw_export) != ""]                  # drop empty trailing columns
survey <- raw_export |> distinct(studentID, .keep_all = TRUE)  # drop the duplicate submission

# ---- Apply the code book labels (same labels as the EDA script) ----
# Only converts columns that are still numeric codes, so it is safe if a1.csv is already labelled
label_codes <- function(x, labels) {
  if (is.numeric(x)) factor(x, levels = seq_along(labels) - 1, labels = labels)
  else factor(x, levels = labels)
}
survey <- survey |>
  mutate(
    gender         = label_codes(gender, c("Female", "Male", "Other")),
    student_status = label_codes(student_status, c("Domestic", "International")),
    housing_type   = label_codes(housing_type, c("Residence", "Off-campus", "Living with family",
                                                 "Living with roommate")),
    cost           = label_codes(cost, c("I do not pay for housing", "Less than $500", "$501 to $1000",
                                         "$1001 to $1500", "$1501 to $2000", "$2001 to $2500",
                                         "More than $2500"))
  )

# Same data without the single residence respondent (RQ2, RQ3 and the regression use this)
survey_no_residence <- survey |>
  filter(housing_type != "Residence") |>
  droplevels() |>
  mutate(cost_bracket = as.integer(cost) - 1)     # 0 = pays nothing, ..., 6 = over $2500

# ---- Helpers and colours shared by the RQ2, RQ3 and regression tabs ----
format_p <- function(p) ifelse(p < .001, "< .001", paste("=", formatC(p, format = "f", digits = 3)))

housing_colours <- c("Residence" = "#8E44AD", "Off-campus" = "#2C6E91",
                     "Living with family" = "#E67E22", "Living with roommate" = "#27AE60")
status_colours  <- c("Domestic" = "#AED6F1", "International" = "#1B4F72")

# ---- RQ1 data: importance ratings in long format ----
importance_labels <- c(distance_importance  = "Distance to campus",
                       rent_importance      = "Rent",
                       commute_importance   = "Commute time",
                       amenities_importance = "Amenities",
                       roommates_importance = "Roommates")

importance_colours <- c("5" = "#1B4F72", "4" = "#5DADE2", "3" = "#D5D8DC",
                        "2" = "#F0B27A", "1" = "#BA4A00")

# ---- Regression (proposal's advanced test): same model as the EDA script; it is fitted in server_logic.R so the filters apply ----
mlr_formula <- housing_satisfaction ~ housing_type + cost_bracket + commute_time +
  distance_importance + amenities_importance + roommates_importance

#print(exists("importance_labels"))
#print(importance_labels)

#source("ui_layout.R") <- ORIGINAL!!!!!
source("ui_layout.R", local = TRUE)
source("interpretations.R", local = TRUE)   # plain-language text for every tab

rq1_panel <- create_rq1_panel(importance_labels)
rq2_panel <- create_rq2_panel()
rq3_panel <- create_rq3_panel()
mlr_panel <- create_mlr_panel()
about_panel <- create_about_panel()   
#source("ui_layout.R", echo = TRUE)  

#print(exists("importance_labels"))

ui <- page_navbar(
  theme = bs_theme(bootswatch = 'cyborg'),
  fillable = FALSE,   # let pages scroll so plots keep their full height
  
  navbar_options = navbar_options(
    bg = "#0A5E54",
    theme = "dark",
    collapsible = FALSE
  ),
  
  title = "Student Housing Study",
  nav_panel(
    title = "Summary",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(inputId = "hist_var",
                    label = "Variable to show:",
                    choices = list("Housing satisfaction" = "housing_satisfaction",
                                   "Importance of rent" = "rent_importance",
                                   "Importance of distance to campus" = "distance_importance",
                                   "Importance of commute time" = "commute_importance",
                                   "Importance of roommates" = "roommates_importance",
                                   "Importance of amenities" = "amenities_importance"),
                    selected = "housing_satisfaction"),
        radioButtons("hist_yscale", "Y-axis shows:",
                     choices = c("Number of students", "Percent of students")),
        sliderInput("hist_xrange", "Rating range on the x-axis:",
                    min = 1, max = 10, value = c(1, 10), step = 1, ticks = FALSE),
        helpText("Zooming only changes what is shown. No responses are removed."),
        demographic_filters("sum")
      ),
      card(withSpinner(plotOutput("hist"))),
      card(card_header("What does this mean?"), uiOutput("summary_interpretation"))
    )
  ),
  
  rq1_panel,
  rq2_panel,
  rq3_panel,
  mlr_panel,
  nav_panel(
    title = "Survey Responses",
    DT::dataTableOutput("responseTable")
  ),
  about_panel
)

server = function(input, output, session) {
  # bs_themer()
  
  # ORIGINAL!!! 
  # source(file.path("server_logic.R"), local = TRUE)$value
  
  source("server_logic.R", local = TRUE)
  
  # Interactive survey response table (follows shown_data(), so it updates with the filter)
  table_names <- c(studentID = "Student ID", gender = "Gender", student_status = "Student status",
                   distance_importance = "Importance: distance", rent_importance = "Importance: rent",
                   commute_importance = "Importance: commute", amenities_importance = "Importance: amenities",
                   roommates_importance = "Importance: roommates", housing_type = "Housing type",
                   housing_satisfaction = "Housing satisfaction (1-10)", cost = "Monthly housing cost",
                   commute_time = "Commute time (minutes)",
                   transportation_satisfacton = "Transportation satisfaction (1-10)")
  
  output$responseTable <- DT::renderDT({
    d <- shown_data()
    d <- d[, intersect(names(table_names), names(d)), drop = FALSE]
    names(d) <- unname(table_names[names(d)])
    DT::datatable(
      d,
      class = "compact stripe nowrap",     # nowrap: long answers no longer stretch every row
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })
  
}

shinyApp(ui, server)