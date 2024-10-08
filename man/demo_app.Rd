% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{demo_app}
\alias{demo_app}
\title{Run demo application}
\usage{
demo_app(
  steps = TRUE,
  stats = c("pre", "post"),
  run_button = "none",
  feedback = TRUE,
  state = TRUE,
  bootstrap = 5,
  enable_bookmarking = TRUE,
  code = TRUE,
  attrition = TRUE,
  show_help = TRUE,
  new_step = c("clone", "configure"),
  ...,
  run_app = TRUE
)
}
\arguments{
\item{steps}{Set to TRUE (default) if multiple steps should be available.}

\item{stats}{Choose which statistics should be displayed for data (and some filters).
Possible options are: "pre" - previous step stat, "post" - current step stats,
`c("pre", "post")` - for both and NULL for no stats.}

\item{run_button}{Should Run button be displayed? If so, the current step computations are run only when clicked.
Three options are available "none" - no button, "local" - button displayed at each step panel,
"global" - button visible in top filtering panel.}

\item{feedback}{Set to TRUE (default) if feedback plots should be displayed at each filter.}

\item{state}{Set to TRUE (default) to enable get/set state panel.}

\item{bootstrap}{Boostrap version to be used for filtering panel.
See \link[bslib]{bs_theme} version argument.}

\item{enable_bookmarking}{Set to TRUE (default) if panel should be compatible with native shiny bookmarking.}

\item{code}{Set to TRUE (default) to enable reproducible code panel.}

\item{attrition}{Set to TRUE (default) to enable attrition plot panel.}

\item{show_help}{Set to TRUE (default) to enable help buttons.}

\item{new_step}{Choose which add step method should be used for creating new step.
Possible options are: "clone" - copy filters from last step,
"configure" - opening modal and allow to chose filters from available filters.}

\item{...}{Extra parameters passed to selected cohort methods.
Currently unused.}

\item{run_app}{If 'TRUE' the application will run using \link[shiny]{runApp},
otherwise \link[shiny]{shinyApp} object is returned.}
}
\value{
In case of `run_app=TRUE` no return value, used for side effect which is running a Shiny application.
  Otherwise \link[shiny]{shinyApp} object.
}
\description{
The demo presents available filters and toolbox features.
}
\examples{
if (interactive()) {
  library(shinyCohortBuilder)
  demo_app(steps = FALSE)
}
if (interactive()) {
  library(shinyCohortBuilder)
  demo_app(run_button = "local", state = FALSE)
}
}
