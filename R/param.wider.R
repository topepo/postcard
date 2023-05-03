#' Transform data to wide format and extracting baseline parameters
#'
#'  Function that take a data set and write it to wide format and take baseline measurement as the latest
#'  measurement at or before baseline.
#'
#' @param data           The data set that should be transformed to wide format
#' @param visit_name     Name of the column with the visit numbers
#' @param visit_n        Visit number for the randomisation visit
#' @param param_name     Name of the column with the parameter names
#' @param parameters     Vector of parameters that should be pivoted to their own column
#' @param analysis_value Name of column with the value of the parameter
#' @param subject_id     Name of the column with subject identification
#' @param analysis_day   Name of column with the relative analysis date
#'
#' @return
#' Data set in wide format with a column for each entrance of the parameters vector.
#'
#'
#' @examples
#' data <- data.frame(subject_id = rep(1:3, rep(4,3)), visit = rep(1:2,
#' rep(2,2)), parameter = rep(c("measure1", "measure2"), 6), analysis_val =
#' rnorm(12, 100, 3))[1:10, ]
#'
#' param.wider(data = data, visit_name = visit, visit_n = 2, param_name =
#' parameter, parameters = c("measure1", "measure2"), analysis_value =
#' analysis_val, subject_id = subject_id, analysis_day = NULL)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select filter group_by arrange desc slice
#' @importFrom tidyr pivot_wider
#' @importFrom rlang enquo
#'
#' @export
#'
param.wider <- function(data,
                        visit_name,
                        visit_n,
                        param_name,
                        parameters,
                        analysis_value,
                        subject_id,
                        analysis_day){

  visit <- rlang::enquo(visit_name)
  param <- rlang::enquo(param_name)
  analysis_value <- rlang::enquo(analysis_value)
  subject <- rlang::enquo(subject_id)
  analysis_day <- rlang::enquo(analysis_day)

  data <- data %>%
    dplyr::filter(!!visit <= visit_n, !!param %in% parameters, !is.na(!!analysis_value)) %>%
    dplyr::group_by(!!subject, !!param) %>%
    dplyr::arrange(dplyr::desc(!!visit), dplyr::desc(!!analysis_day), .by_group = T) %>%
    dplyr::slice(1) %>%
    dplyr::select(!!subject, !!param, !!analysis_value) %>%
    tidyr::pivot_wider(names_from = !!param, values_from = !!analysis_value)

  return(data)
}


