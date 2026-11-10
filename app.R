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
survey <- raw_export |> distinct(studentID, .keep_all = TRUE)  # drop the duplicate submission

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

#print(exists("importance_labels"))
#print(importance_labels)

#source("ui_layout.R") <- ORIGINAL!!!!!
source("ui_layout.R", local = TRUE)

rq1_panel <- create_rq1_panel(importance_labels)
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
                        choices = list("Housing Satisfaction" = "housing_sat",
                                       "Importance of Rent" = "rent_imp",
                                       "Importance of Distance" = "dist_imp",
                                       "Importance of Commute Time" = "cmmt_imp",
                                       "Importance of Roommates" = "rmmt_imp",
                                       "Importance of Amenities" = "amt_imp"),
                        selected = "housing_sat"),
            plotOutput("hist")
            ),
  rq1_panel,
  nav_panel(title = "Housing choice by Residence status"),
  nav_panel(title = "Housing satisfaction by Housing type"),
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
