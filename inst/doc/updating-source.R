## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(shinyCohortBuilder)

## ----eval = FALSE-------------------------------------------------------------
#  library(shiny)
#  library(cohortBuilder)
#  library(shinyCohortBuilder)
#  
#  mtcars_list = list(
#    "0" = dplyr::filter(mtcars, am == 0),
#    "1" = dplyr::filter(mtcars, am == 1)
#  )
#  
#  ui <- fluidPage(
#    sidebarLayout(
#      sidebarPanel(
#        radioButtons("version", "Version", choices = c("0", "1")),
#        cb_ui("mtcars")
#      ),
#      mainPanel(
#        verbatimTextOutput("cohort_data")
#      )
#    )
#  )
#  
#  server <- function(input, output, session) {
#    init_source <- set_source(tblist(mtcars = mtcars_list[["0"]]))
#    mt_cohort <- cohort(
#      init_source,
#      filter("range", id = "mpg", dataset = "mtcars", variable = "mpg", active = FALSE),
#      filter("range", id = "qsec", dataset = "mtcars", variable = "qsec", active = FALSE)
#    )
#  
#    cb_server("mtcars", mt_cohort)
#  
#    observeEvent(input$version, {
#      new_source <- set_source(tblist(mtcars = mtcars_list[[input$version]]))
#      update_source(mt_cohort, new_source, keep_steps = TRUE)
#    })
#  }
#  
#  shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
#  library(shiny)
#  library(cohortBuilder)
#  library(shinyCohortBuilder)
#  
#  source_mtcars <- set_source(
#    tblist(mtcars = mtcars),
#    filter("range", id = "mpg", dataset = "mtcars", variable = "mpg", active = FALSE),
#    filter("range", id = "qsec", dataset = "mtcars", variable = "qsec", active = FALSE)
#  )
#  
#  source_iris <- set_source(
#    tblist(iris = iris),
#    filter("discrete", id = "species", dataset = "iris", variable = "Species", active = FALSE),
#    filter("range", id = "petal_length", dataset = "iris", variable = "Petal.length", active = FALSE)
#  )
#  
#  ui <- fluidPage(
#    sidebarLayout(
#      sidebarPanel(
#        radioButtons("dataset", "Dataset", choices = c("mtcars", "iris")),
#        cb_ui("data_panel")
#      ),
#      mainPanel(
#        verbatimTextOutput("cohort_data")
#      )
#    )
#  )
#  
#  server <- function(input, output, session) {
#    cohort_object <- cohort(source_mtcars)
#    cb_server("data_panel", cohort_object)
#  
#    observeEvent(input$dataset, {
#      if (input$dataset = "mtcars") {
#        update_source(cohort_object, source_mtcars, keep_steps = FALSE)
#      }
#      if (input$dataset = "iris") {
#        update_source(cohort_object, source_iris, keep_steps = FALSE)
#      }
#    })
#  }
#  
#  shinyApp(ui, server)

