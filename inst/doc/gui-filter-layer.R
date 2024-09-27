## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(shinyCohortBuilder)

## -----------------------------------------------------------------------------
library(cohortBuilder)
iris_source <- set_source(tblist(iris = iris))
species_filter <- filter(
  type = "discrete", 
  id = "species",
  dataset = "iris",
  variable = "Species",
  value = "setosa"
)

## -----------------------------------------------------------------------------
evaled_filter <- species_filter(iris_source)
class(evaled_filter)

## -----------------------------------------------------------------------------
str(evaled_filter, give.attr = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  .gui_filter.discrete <- function(filter, ...) {
#    list(
#      input = function(input_id, cohort) {
#        shiny::tagList(
#          .cb_input(
#            do.call(
#              shiny::checkboxGroupInput,
#              discrete_input_params(filter, input_id, cohort, ...)
#            ),
#            filter$input_param
#          ),
#          .cb_input(
#            keep_na_input(input_id, filter, cohort),
#            "keep_na"
#          )
#        )
#      },
#      # other objects
#    )
#  }

## ----eval = FALSE-------------------------------------------------------------
#  do.call(
#    shiny::checkboxGroupInput,
#    discrete_input_params(filter, input_id, cohort, ...)
#  )

## ----eval = FALSE-------------------------------------------------------------
#  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
#    return(
#      list(inputId = input_id, choices = character(0), selected = character(0), label = NULL)
#    )
#  }

## ----eval = FALSE-------------------------------------------------------------
#  step_id <- filter$step_id
#  filter_id <- filter$id

## ----eval = FALSE-------------------------------------------------------------
#  filter_params <- filter$get_params()

## ----eval = FALSE-------------------------------------------------------------
#  filter_params[[filter$input_param]]

## ----eval = FALSE-------------------------------------------------------------
#  names(
#    cohort$get_cache(step_id, filter_id, state = "pre")$choices
#  )

## ----eval = FALSE-------------------------------------------------------------
#  names(
#    cohort$get_cache(step_id, filter_id, state = "pre")$choices
#  )

## ----eval = FALSE-------------------------------------------------------------
#  !is.null(filter_params$value_mapping)

## ----eval = FALSE-------------------------------------------------------------
#  cohort$get_source()$attributes$value_mappings[[filter_params$value_mapping]]

## ----eval = FALSE-------------------------------------------------------------
#  .pre_post_stats_text(
#    name = <choices labels names vector>,
#    current = <current step statistics vector>,
#    previous = <previous step statistics vector>,
#    stats = <"pre, post", both or NULL> # it's recommended to use `stats = cohort$attributes$stats` to inherit the option from Cohort configuration
#  )

## -----------------------------------------------------------------------------
.pre_post_stats_text(
  name = c("A", "B"),
  current = 1:2,
  previous = 3:4,
  stats = c("pre", "post")
)

## ----eval = FALSE-------------------------------------------------------------
#  .gui_filter.discrete <- function(filter, ...) {
#    list(
#      feedback = function(input_id, cohort, empty = FALSE) {
#        list(
#          plot_id = shiny::NS(input_id, "feedback_plot") ,
#          output_fun = ggiraph::girafeOutput,
#          render_fun = if (!is.null(empty)) {
#            ggiraph::renderGirafe({
#              if(empty) { # when no data in parent step
#                return(
#                  ggiraph::girafe(
#                    ggobj      = ggplot2::ggplot(),
#                    width_svg  = 10,
#                    height_svg = 0.1
#                  )
#                )
#              }
#              step_id <- filter$step_id
#              filter_id <- filter$id
#  
#              filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
#              filter_value <- extract_selected_value(filter$get_params()$value, filter_cache$choices, FALSE)
#              plot_data <- filter_cache$choices[filter_value]
#              n_missing <- filter_cache$n_missing
#              if (identical(filter$get_params()$keep_na, FALSE)) {
#                n_missing <- 0
#              }
#  
#              plot_feedback_bar(plot_data, n_missing)
#            })
#          }
#        )
#      },
#      # other methods
#    )
#  }

## ----eval = FALSE-------------------------------------------------------------
#  step_id <- filter$step_id
#  filter_id <- filter$id

## ----eval = FALSE-------------------------------------------------------------
#  .gui_filter.discrete <- function(filter, ...) {
#    list(
#      server = function(input_id, input, output, session, cohort) {
#        shiny::observeEvent(input[[shiny::NS(input_id, "feedback_plot_selected")]], {
#          value <- input[[shiny::NS(input_id, "feedback_plot_selected")]]
#  
#          if (!is.na(value)) {
#            .trigger_action(session, "update_filter", params = list(
#              step_id = filter$step_id, filter_id = filter$id,
#              input_name = filter$input_param, input_value = value,
#              run_flow = FALSE
#            ))
#          }
#        }, ignoreInit = TRUE) %>% .save_observer(input_id, session)
#      },
#      # other methods
#    )
#  }

## ----eval = FALSE-------------------------------------------------------------
#  .gui_filter.discrete <- function(filter, ...) {
#    list(
#      update = function(session, input_id, cohort, reset = FALSE, ...) {
#        update_params <- discrete_input_params(filter, input_id, cohort, reset = reset, update = TRUE, ...)
#        do.call(
#          shiny::updateCheckboxGroupInput,
#          append(
#            list(session = session),
#            update_params
#          )
#        )
#        .update_keep_na_input(session, input_id, filter, cohort)
#      },
#      # other methods
#    )
#  }

## ----eval = FALSE-------------------------------------------------------------
#  names(
#    cohort$get_cache(step_id, filter_id, state = "pre")$choices
#  )

## ----eval = FALSE-------------------------------------------------------------
#  label = if (update) character(0) else NULL

