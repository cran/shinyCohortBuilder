## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)
set.seed(123)
old_opts <- options()
options(tibble.width = Inf)
iris <- tibble::as.tibble(iris)
options("tibble.print_max" = 5)
options("tibble.print_min" = 5)

## ----eval = FALSE-------------------------------------------------------------
#  library(shiny)
#  library(cohortBuilder)
#  library(shinyCohortBuilder)
#  
#  ui <- fluidPage(
#    cb_ui("panel_id")
#  )
#  
#  server <- function(input, output, session) {
#    source_obj <- set_source(tblist(iris = iris))
#    cohort_obj <- cohort(
#      source_obj,
#      filter("discrete", id = "species", dataset = "iris", variable = "Species"),
#      filter("range", id = "petal_length", dataset = "iris", variable = "Petal.Length")
#    )
#    cb_server("panel_id", cohort_obj)
#  }
#  
#  shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
#  library(shiny)
#  library(cohortBuilder)
#  library(shinyCohortBuilder)
#  
#  program_vm <- function(programs, cohort) {
#    c(
#      "standard" = "Standard",
#      "premium" = "Premium",
#      "vip" = "VIP"
#    )[programs]
#  }
#  
#  librarian_source <- set_source(
#    as.tblist(librarian),
#    value_mappings = list(program_vm = program_vm)
#  )
#  librarian_cohort <- cohort(
#    librarian_source,
#    filter(
#      "discrete",
#      id = "program",
#      dataset = "borrowers",
#      variable = "program",
#      value_mapping = "program_vm",
#      gui_input = "vs"
#    )
#  )
#  
#  gui(librarian_cohort)

## -----------------------------------------------------------------------------
iris_source <- set_source(tblist(iris = iris)) %>% 
  autofilter()
iris_cohort <- cohort(iris_source)

sum_up(iris_cohort)

## ----eval = FALSE-------------------------------------------------------------
#  set_source(
#    ...,
#    available_filters = list(
#      filter("discrete", ...),
#      filter("range", ...),
#      ...
#    )
#  )

## ----include = FALSE----------------------------------------------------------
options(old_opts)

