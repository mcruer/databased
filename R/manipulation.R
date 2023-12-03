#' Detect and Retain Rows with Changes in a DataFrame
#'
#' This function identifies and retains rows within a dataframe where changes have occurred in any column, excluding specified columns. It's designed to work within groups defined by a unique identifier and does not consider changes from or to `NA` as changes.
#'
#' @param df A dataframe containing the data to be checked for changes.
#' @param unique_identifier The name of the column that uniquely identifies each row or group for comparison. This parameter is treated as a grouping variable.
#' @param ignore_cols A vector of column names to be ignored during change detection.
#'
#' @return A dataframe containing only the rows where changes have been detected in any of the columns, excluding those specified in `ignore_cols`.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'data' is a dataframe with a unique ID column 'id' and you
#' # want to ignore 'timestamp' column.
#' changes_df <- slice_changes(data, unique_identifier = "id", ignore_cols = "timestamp")
#'
#' # The above will return a dataframe with rows where changes occurred.
#' }
slice_changes <- function (df, unique_identifier, ignore_cols) {

  #NOTE: This does not register a change from a value to NA as a change.
  is_changed <- function (vec) {
    tidyr::replace_na(dplyr::lag(vec) != vec, TRUE)
  }

  run_comparison <- function (df_hits, df_input) {
    purrr::map2(df_hits, df_input, ifelse, NA) %>%
      dplyr::bind_cols()
  }

  column_names <- names(df)

  original <- df
  original_no_project_id <- df %>%
    dplyr::ungroup () %>%
    dplyr::select (-{{unique_identifier}})
  unique_ids <- df %>%
    dplyr::select({{unique_identifier}})

  hits <- original %>%
    dplyr::group_by({{unique_identifier}}) %>%
    gplyr::quickm(dplyr::everything(), is_changed) %>%
    dplyr::ungroup() %>%
    dplyr::select(-{{unique_identifier}})

  output <- run_comparison (hits, original_no_project_id) %>%
    dplyr::bind_cols(unique_ids) %>%
    dplyr::select(dplyr::all_of(column_names)) %>%
    dplyr::filter(dplyr::if_any(.cols = -c({{ignore_cols}}, {{unique_identifier}}), ~!is.na(.x)))

  return (output)
}

#' Take a Snapshot of Data as of a Specific Date
#'
#' This function filters a dataframe to represent a 'snapshot' of the data as of a particular date. It fills in NA values downwards and retains only the most recent entry for each unique identifier up to the specified date.
#'
#' @param df A dataframe to take a snapshot from.
#' @param as_of A date in as_date format or date character string in 'yyyy-mm-dd' format (which is converted with as_date) specifying the snapshot date. If `NULL`, the function will use the latest date available in `date_data`.
#' @param unique_identifier The column name that uniquely identifies each entry or group in `df`. If not provided, defaults to 'project_id'.
#' @param date_data The column name containing date information used to filter the data.
#'
#' @return A dataframe that is a snapshot of `df` as of `as_of`, with NA values filled down and only the most recent data for each unique identifier.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'data_frame' is a dataframe with 'project_id' as the unique
#' # identifier and 'record_date' as the date column:
#' snapshot <- snapshot_data(data_frame, as_of = "2021-12-31",
#' unique_identifier = "project_id",
#' date_data = "record_date")
#'
#' # This will return a snapshot of 'data_frame' as of 2021-12-31, with all
#' # NA values filled down and only the latest record for each project_id.
#' }
snapshot_data <- function (df, as_of = NULL, unique_identifier = project_id, date_data = date_data){

  if (is.character(as_of)) {as_of <- lubridate::ymd(as_of)}

  if(!is.null(as_of)) {
    df <- df %>%
      dplyr::filter({{date_data}} <= as_of)
  }

  df <- df %>%
    dplyr::group_by({{unique_identifier}}) %>%
    tidyr::fill(dplyr::everything(), .direction = "down") %>%
    dplyr::filter({{date_data}} == max({{date_data}})) %>%
    dplyr::ungroup()

  return(df)
}
