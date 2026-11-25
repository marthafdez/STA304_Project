library(shiny)
library(bslib)
library(shinycssloaders) # loading screens
library(colourpicker)
library(thematic)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

thematic::thematic_shiny(font = "auto")

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

importance_long <- survey |>
  select(all_of(names(importance_labels))) |>
  pivot_longer(everything(), names_to = "factor", values_to = "rating") |>
  mutate(factor = unname(importance_labels[factor]))

importance_colours <- c("5" = "#1B4F72", "4" = "#5DADE2", "3" = "#D5D8DC",
                        "2" = "#F0B27A", "1" = "#BA4A00")

rq1_friedman <- friedman.test(as.matrix(survey[names(importance_labels)]))

# ---- Regression (proposal's advanced test): same model as the EDA script ----
mlr_formula <- housing_satisfaction ~ housing_type + cost_bracket + commute_time +
  distance_importance + amenities_importance + roommates_importance
model_mlr_full     <- lm(mlr_formula, data = survey_no_residence)
model_mlr_selected <- step(model_mlr_full, direction = "backward", trace = 0)   # backward AIC selection
model_mean_only    <- lm(housing_satisfaction ~ 1, data = survey_no_residence)
model_housing_only <- lm(housing_satisfaction ~ housing_type, data = survey_no_residence)
candidate_models <- list("Full model (all six predictors)" = model_mlr_full,
                         "Housing type only"               = model_housing_only,
                         "No predictors (mean only)"       = model_mean_only)
model_comparison <- data.frame(
  Model = names(candidate_models),
  `Number of terms` = vapply(candidate_models, function(m) length(coef(m)) - 1, numeric(1)),
  R2  = vapply(candidate_models, function(m) summary(m)$r.squared, numeric(1)),
  AIC = vapply(candidate_models, AIC, numeric(1)),
  BIC = vapply(candidate_models, BIC, numeric(1)),
  check.names = FALSE, row.names = NULL
)
model_comparison <- model_comparison[order(model_comparison$AIC), ]

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
  
  navbar_options = navbar_options(
    bg = "#0A5E54",
    theme = "dark",
    collapsible = FALSE
  ),
  
  title = "Student Housing Study",
  nav_panel(title = "Summary", 
            selectInput(inputId = "hist_var",
                        label = "What var would you like to see?",
                        choices = list("Housing Satisfaction" = "housing_satisfaction",
                                       "Importance of Rent" = "rent_importance",
                                       "Importance of Distance" = "distance_importance",
                                       "Importance of Commute Time" = "commute_importance",
                                       "Importance of Roommates" = "roommates_importance",
                                       "Importance of Amenities" = "amenities_importance"),
                        selected = "housing_satisfaction"),
            plotOutput("hist"),
            card(card_header("What does this mean?"), uiOutput("summary_interpretation"))
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
  
  # Interactive survey response table
  output$responseTable <- DT::renderDT({
    
    DT::datatable(
      survey,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
    
  })
  
}

shinyApp(ui, server)