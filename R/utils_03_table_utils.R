#' 03_table_utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Function to add plus sign (+) before positive figures
format_gd <- function(value) {
  if (value != 0)
    sprintf("%+3d", value)
  else
    value
}
