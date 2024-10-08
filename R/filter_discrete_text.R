plot_feedback_text_bar <- function(plot_data) {

  feedback_data <- data.frame(
    level = factor(names(plot_data)),
    n = unlist(plot_data)
  )
  n_selected <- feedback_data$n[1]
  n_total <- sum(feedback_data$n)

  if (NROW(feedback_data) == 0) {
    gg_object <- ggplot2::ggplot()
  } else {

    chart_palette <- getOption("scb_chart_palette", scb_chart_palette)
    color_palette <- c(chart_palette$no_data, chart_palette$discrete[1])

    gg_object <- feedback_data %>%
      ggplot2::ggplot(ggplot2::aes(x = "I", y = n, fill = level)) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse = FALSE)) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text  = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(0, "pt"),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background  = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = ggplot2::unit(c(1, 0, 0, 0),"mm"),
        panel.border = ggplot2::element_rect(colour = "grey50", fill = NA, size = 1),
        panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "mm"),
        plot.subtitle = ggplot2::element_text(color = "dimgray", size = 10, face = "plain")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        subtitle = glue::glue(
          "Unique values: {format_number(n_selected)}",
          " / {format_number(n_total)} ",
          "({round(100 * n_selected / n_total, 1)}%)"
        )
      ) +
      ggplot2::scale_fill_manual(name = NULL, values = color_palette)
  }

  return(gg_object)
}

get_matching_vals <- function(selected, original, reset = FALSE) {

  if (reset || identical(selected, NA)) {
    return(original)
  }

  if (identical(selected, "")) {
    return(selected)
  }

  selected_vec <- unique(strsplit(sub(" ", "", selected, fixed = TRUE), ",", fixed = TRUE)[[1]])
  original_vec <- unique(strsplit(sub(" ", "", original, fixed = TRUE), ",", fixed = TRUE)[[1]])

  if (!all(selected_vec %in% original_vec)) {
    return(paste(intersect(selected_vec, original_vec), collapse = ","))
  }

  return(selected)
}

get_n_matching_vals <- function(selected, original) {

  original_vec <- unique(strsplit(sub(" ", "", original, fixed = TRUE), ",", fixed = TRUE)[[1]])
  if (identical(selected, NA)) {
    return(length(original_vec))
  }
  selected_vec <- unique(strsplit(sub(" ", "", selected, fixed = TRUE), ",", fixed = TRUE)[[1]])

  sum(selected_vec %in% original_vec)
}

discrete_text_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {

  step_id <- filter$step_id
  filter_id <- filter$id

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id, value = "", label = NULL)
    )
  }

  parent_choices <- cohort$get_cache(step_id, filter_id, state = "pre")$choices
  selected_value <- get_matching_vals(
    filter$get_params("value"),
    parent_choices,
    reset
  )

  params <- list(
    inputId = input_id,
    value = selected_value,
    label = "Place values separated by commas here:",
    all = parent_choices,
    ...
  )

  if (update) {
    params$label <- NULL
  }

  return(params)
}


#' @rdname gui-filter-layer
#' @export
.gui_filter.discrete_text <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_params <- modify_list(
        list(
          all = NULL, readonly = FALSE, width = "100%",
          inputId = paste0(input_id, "_selected")
        ),
        discrete_text_input_params(filter, input_id, cohort, ...)
      )
      parent <- input_params$all
      modal_dialog_id <- paste0(input_id, "modal_in")
      # fixes overlapping modal dialog by backdrop
      move_dialog_to_body_js <- paste0("$('#", modal_dialog_id, "').appendTo('body');")
      move_dialog_back_js <- paste0("$('#", modal_dialog_id, "').appendTo('#", input_id, " .cb_inputs');")

      shiny::tagList(
        shinyGizmo::modalDialogUI(
          modal_dialog_id,
          do.call(shinyGizmo::textArea, input_params),
          shinyGizmo::textArea(paste0(input_id, "show_all"), parent, "Possible values", readonly = TRUE),
          backdrop = TRUE,
          size = "l",
          footer = shiny::tagList(
            .cb_input(
              shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("[data-id=\"", input_params$inputId, "\""),
                `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
                onclick = move_dialog_back_js, try_binding = FALSE
              ),
              filter$input_param,
              style = "display: inline-block;"
            ),
            shiny::modalButton("Dismiss") %>%
              htmltools::tagAppendAttributes(
                onclick = move_dialog_back_js
              )
          ),
          button = button(
            getOption("scb_icons", scb_labels)$filter_discrete_text_bttn_label,
            icon = getOption("scb_icons", scb_icons)$filter_discrete_text_bttn_icon,
            class = "btn-sm scb-input-button",
            `data-toggle` = "modal", `data-target` = paste0("#", modal_dialog_id),
            `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", modal_dialog_id),
            onclick = move_dialog_to_body_js
          )
        )
      )
    },
    feedback = function(input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot") ,
        output_fun = shiny::plotOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderPlot(height = 40, {
            if(empty) {
              return(ggplot2::ggplot())
            }
            step_id <- filter$step_id
            filter_id <- filter$id

            filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
            n_total <- filter_cache$n_data

            n_selected <- get_n_matching_vals(filter$get_params("value"), filter_cache$choices)
            plot_data <- c("selected" = n_selected, "not_seleced" = n_total - n_selected)

            plot_feedback_text_bar(plot_data)
          })
        }
      )
    },
    server = function(input_id, input, output, session, cohort) {},
    update = function(session, input_id, cohort, reset = FALSE, ...) {
      input_fun <- shinyGizmo::updateTextArea
      update_params <- discrete_text_input_params(filter, input_id, cohort, reset, TRUE, ...)
      parent <- update_params$all
      update_params$all <- NULL
      update_params$inputId <- paste0(input_id, "_selected")

      do.call(
        input_fun,
        append(
          list(session = session),
          update_params
        )
      )
      input_fun(session, paste0(input_id, "show_all"), value = parent)
    },
    post_stats = FALSE,
    multi_input = FALSE
  )
}
