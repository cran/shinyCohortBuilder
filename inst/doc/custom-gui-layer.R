## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(shinyCohortBuilder)

## ---- eval = FALSE------------------------------------------------------------
#  .render_filters.default <- function(source, cohort, step_id, ns, ...) {
#    step <- cohort$get_step(step_id)
#    shiny::tagList(
#      shiny::htmlOutput(ns(paste0(step_id, "-stats")), class = "scb_data_stats"),
#      step$filters %>%
#        purrr::map(~ .render_filter(.x, step_id, cohort, ns = ns)) %>%
#        shiny::div(class = "cb_filters", `data-step_id` = step_id)
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .render_filters.tblist <- function(source, cohort, step_id, ns, ...) {
#    step <- cohort$get_step(step_id)
#  
#    group_filters(cohort$get_source(), step$filters) %>%
#      purrr::imap(~ dataset_filters(.x, .y, step_id, cohort, ns = ns)) %>%
#      shiny::div(class = "cb_filters", `data-step_id` = step_id)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  render_filters.db <- function(source, cohort, step_id, ns) {
#    step <- cohort$get_step(step_id)
#  
#    group_filters_db(cohort$get_source(), step$filters) %>%
#      purrr::imap(~ dataset_filters_db(.x, .y, step_id, cohort, ns = ns)) %>%
#      div(class = "cb_filters", `data-step_id` = step_id)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .update_data_stats.default <- function(source, step_id, cohort, session, ...) {
#    ns <- session$ns
#    stats <- cohort$attributes$stats
#  
#    session$output[[paste0(step_id, "-stats")]] <- shiny::renderUI({
#      previous <- cohort$get_cache(step_id, state = "pre")$n_rows
#      if (!previous > 0) {
#        return("No data selected in previous step.")
#      }
#      current <- cohort$get_cache(step_id, state = "post")$n_rows
#      .pre_post_stats(current, previous, percent = TRUE, stats = stats)
#    })
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .update_data_stats.tblist <- function(source, step_id, cohort, session, ...) {
#    stats <- cohort$attributes$stats
#    step <- cohort$get_step(step_id)
#  
#    dataset_names <- names(cohort$get_source()$attributes$datasets)
#    data_filters <- purrr::map_chr(step$filters, get_filter_dataset)
#    dataset_names <- intersect(dataset_names, data_filters)
#  
#    dataset_names %>% purrr::walk(
#      ~ .sendOutput(
#        paste0(step_id, "-stats_", .x),
#        shiny::renderUI({
#          previous <- cohort$get_cache(step_id, state = "pre")[[.x]]$n_rows
#          if (!previous > 0) {
#            return("No data selected in previous step.")
#          }
#          current <- cohort$get_cache(step_id, state = "post")[[.x]]$n_rows
#          .pre_post_stats(current, previous, percent = TRUE, stats = stats)
#        }),
#        session
#      )
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  update_data_stats.db <- function(source, step_id, cohort, session) {
#    stats <- cohort$attributes$stats
#  
#    dataset_names <- source$attributes$tables
#    dataset_names %>% purrr::walk(
#      ~ shinyCohortBuilder::sendOutput(
#        paste0(step_id, "-stats_", .x),
#        shiny::renderUI({
#          previous <- cohort$get_cache(step_id, state = "pre")[[.x]]$n_rows
#          if (!previous > 0) {
#            return("No data selected in previous step.")
#          }
#          current <- cohort$get_cache(step_id, state = "post")[[.x]]$n_rows
#          shinyCohortBuilder::pre_post_stats(current, previous, percent = TRUE, stats = stats)
#        })
#      )
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  autofilter.tblist <- function(source, attach_as = c("step", "meta"), ...) {
#    attach_as <- rlang::arg_match(attach_as)
#    step_rule <- source$dtconn %>%
#      purrr::imap(~filter_rules(.x, .y)) %>%
#      unlist(recursive = FALSE) %>%
#      purrr::map(~do.call(cohortBuilder::filter, .)) %>%
#      unname()
#  
#    if (identical(attach_as, "meta")) {
#      source$attributes$available_filters <- step_rule
#    } else {
#      source %>%
#        cohortBuilder::add_step(do.call(cohortBuilder::step, step_rule))
#    }
#  
#    return(source)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .available_filters_choices.tblist <- function(source, cohort, ...) {
#  
#    available_filters <- cohort$attributes$available_filters
#  
#    choices <- purrr::map(available_filters, function(x) {
#      tibble::tibble(
#        name = as.character(
#          shiny::div(
#            `data-tooltip-z-index` = 9999,
#            `data-tooltip` = x$get_params("description"),
#            `data-tooltip-position` = "top right",
#            `data-tooltip-allow-html` = "true",
#            x$name
#          )
#        ),
#        id = x$id,
#        dataset = x$get_params("dataset")
#      )
#    }) %>% dplyr::bind_rows()
#  
#    shinyWidgets::prepare_choices(choices, name, id, dataset)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .step_attrition.default <- function(source, id, cohort, session, ...) {
#    ns <- session$ns
#  
#    list(
#      render = shiny::renderPlot({
#        cohort$show_attrition()
#      }),
#      output = shiny::plotOutput(id)
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .step_attrition.tblist <- function(source, id, cohort, session, ...) {
#    ns <- session$ns
#    choices <- names(source$attributes$datasets)
#  
#    list(
#      render = shiny::renderPlot({
#        cohort$show_attrition(dataset = session$input$attrition_input)
#      }),
#      output = shiny::tagList(
#        shiny::selectInput(ns("attrition_input"), "Choose dataset", choices),
#        shiny::plotOutput(id)
#      )
#    )
#  }

