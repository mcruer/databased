utils::globalVariables(c("project_id", "date_data"))

#Option Names: databased.path

#' Copy Database Path Setting to Clipboard
#'
#' This function copies a specific R options setting command to the clipboard.
#' The command sets the `databased.path` option to a predefined path (`"S:/data/databased"`).
#' This can be useful for quickly sharing or applying this setting in different R scripts
#' or R sessions.
#'
#' @details
#' The function utilizes the `clipr` package to interact with the system clipboard.
#' It is designed to work in environments where clipboard access is available.
#' The specific text copied to the clipboard is:
#' `options(databased.path = "S:/data/databased")`.
#'
#' @return
#' The function does not return a value but performs an action: copying text to the clipboard.
#'
#' @examples
#' \dontrun{
#'   options_to_clipboard() # Copies the options command to the clipboard
#' }
#'
#' @export
#'
#' @importFrom clipr write_clip
options_to_clipboard <- function() {
  text_to_copy <- 'options(databased.path = "S:/data/databased")'
  clipr::write_clip(text_to_copy)
}


#' Set the Default Data Path for databased
#'
#' This function sets the default data path for the `databased` package by updating the
#' `databased.path` option. This path is used as the default location to read from and write to data files.
#'
#' @param path A character string specifying the file path to be set as the default data path.
#'
#' @details
#' `data_path` is a utility function that modifies the global option `databased.path`.
#' This option is used by the `databased` package to determine the default directory
#' for data operations. Setting this path is especially useful for ensuring
#' consistency in file locations across different sessions and scripts.
#'
#' It's important to note that this function changes a global option, and the set path
#' remains in effect for the duration of the R session or until it's modified again.
#'
#' @examples
#' \dontrun{
#' data_path("path/to/your/data")
#' }
#'
#' @export
data_path <- function (path) {
  options(databased.path = path)
}

#' Set databased.path in .Rprofile
#'
#' Modifies the `.Rprofile` file in the user's home directory to set the `databased.path` option.
#' This function ensures that `databased.path` is correctly set for future R sessions.
#'
#' @param path A character string specifying the desired path for the `databased.path` option.
#'
#' @details
#' - This function alters the `.Rprofile` file located in the user's home directory.
#' - If the `.Rprofile` file does not exist, it will be created with the necessary setting.
#' - If the file exists, the function will append or modify the `databased.path` setting
#'   while preserving other settings.
#' - The `data_path()` function can be used as an alternative to set the `databased.path`
#'   option for the current session without altering the `.Rprofile` file.
#' - Note that a `.Rprofile` file in the project directory will take precedence over the
#'   settings made by this function.
#'
#' IMPORTANT
#' - Modifying the `.Rprofile` can impact the behavior of all R sessions for the user.
#'   Ensure that you understand the implications of these changes.
#' - Consider backing up your `.Rprofile` before making automated modifications.
#'
#' @examples
#' \dontrun{
#' # Set the databased.path in .Rprofile
#' set_databased_path("path/to/databased")
#' }
#'
#' @export
set_databased_path <- function(path){
  rprofile_path <- stringr::str_c(path.expand("~") %>% confirm_slash(),
                                  ".Rprofile")

  text_to_add <- stringr::str_c("options(databased.path = '",
                                path %>% confirm_slash(),
                                "')")


  file_exists <- file.exists(rprofile_path)

  if(!file_exists) {
    readr::write_lines(text_to_add, file = rprofile_path)

  } else {
    c (readr::read_lines(rprofile_path) %>%
         gplyr::str_filter("databased.path",
                           negate = TRUE),
       text_to_add) %>%
      readr::write_lines(file = rprofile_path)
  }

}



#' Ensure Trailing Slash
#'
#' `confirm_slash` ensures that a given file path ends with a forward slash.
#' This is useful for constructing file paths in a consistent manner, especially
#' when working across different operating systems. If the path already ends with
#' a forward slash, it remains unchanged.
#'
#' @param path A character string representing a file path.
#'
#' @return A character string of the file path, guaranteed to end with a forward slash.
#'
#' @examples
#' confirm_slash("path/to/directory")
#' confirm_slash("path/to/directory/")
#'
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_c
confirm_slash <- function(path) {
  if (!stringr::str_detect(path, "/$")) {
    path <- stringr::str_c(path, "/")
  }
  path
}

#path_databased <- "S:/data/databased/rda/"
#path_database <- "S:/data/databased/rda/"
#path_log_databse <- "S:/data/databased/log/"

path_rda <- function(){
  getOption("databased.path") %>%
    confirm_slash() %>%
    stringr::str_c("rda/")
}

path_log <- function() {
  getOption("databased.path") %>%
    confirm_slash() %>%
    stringr::str_c("log/")
}

create_path_log <- function (log_name) {
  stringr::str_c(path_log(), log_name, ".rda")
}

create_path_data <- function (dataset_name) {
  stringr::str_c(path_rda(), dataset_name, ".rda")
}

#' Load a Log File
#'
#' `get_log` loads a log file stored in R data format (`.rda`). It constructs the file path
#' based on the provided log name and a base path set as an option. The function then
#' loads the log data into the R environment. It is assumed that the log file contains
#' an object with the same name as `log_name`.
#'
#' @param log_name The name of the log file to be loaded (without the `.rda` extension).
#'
#' @return The object stored in the log file, typically a data frame or a list.
#'         This object should have the same name as `log_name`.
#'
#' @examples
#' \dontrun{
#'   get_log("example_log") # Loads 'example_log.rda' from the log directory
#' }
#'
#' @export
#'
#' @importFrom stringr str_c
get_log <- function (log_name){
  path_log <- create_path_log(log_name)
  load(path_log)
  get(log_name)
}

#' Append Entry to Log and Save
#'
#' The `log_it` function appends a new entry to an existing log and then saves
#' the updated log back to its file. It relies on the `get_log` function to
#' retrieve the current log and uses tidyverse functions for manipulation.
#'
#' @param log_entry A data frame or similar structure representing the new log entry to be appended.
#' @param log_name The name of the log file (without the `.rda` extension) to which the log entry will be appended.
#'
#' @return The function does not return a value but updates the log file specified by `log_name`.
#'
#' @examples
#' \dontrun{
#'   new_entry <- tibble(timestamp = Sys.time(), message = "New log entry")
#'   log_it(new_entry, "example_log")
#' }
#'
#' @export
#'
log_it <- function (log_entry, log_name) {

  get_log (log_name) %>%
    dplyr::bind_rows(log_entry) %>%
    gplyr::pipe_assign(str_c (log_name))

  # Use the object's name to save it, not the object itself
  save(list=log_name,
       file = create_path_log(log_name))
}


#' Save an R object to a specified database path
#'
#' This function saves an R object to a predefined path in an '.rda' format. The object
#' is saved using its name in the environment specified, allowing for easy retrieval using
#' the same name. The default save path is set by the global 'path_database' variable.
#'
#' @param obj The object to be saved.
#' @param envir The environment from which the object should be saved (default is the global environment).
#' @return Invisible NULL; the function is called for its side effect of saving a file.
#' @export
#' @examples
#' \dontrun{
#'   my_data <- mtcars
#'   database_it(my_data)
#'   # This will save the 'my_data' object to 'S:/data/databased/rda/my_data.rda'
#' }
database_it <- function (obj, envir = .GlobalEnv) {
  object_name <- deparse(substitute(obj))
  # Use the object's name to save it, not the object itself
  save(list=object_name, file = create_path_data(object_name), envir = envir)
}



#' Load Data from a Specified Path
#'
#' This function loads an R data file (.rda) from a predefined path into the global environment.
#' The path is combined with the data name and extension to locate the file.
#' If the file or the specified data object within the file does not exist, the function stops with an error.
#'
#' @param data_name The name of the dataset to load.
#' @param extension The file extension of the dataset, defaulting to '.rda'.
#' @return The specified data object.
#' @export
#' @examples
#' \dontrun{
#' load_data("approvals")
#' }
load_data <- function(data_name, extension = ".rda") {
  file_path <- create_path_data (data_name)

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  loaded_names <- load(file_path)
  if (!data_name %in% loaded_names) {
    stop("Data object '", data_name, "' not found in the loaded file.")
  }

  return(get(data_name))
}

#' Retrieve 'projects' Data as a Tibble
#'
#' This function is a wrapper for `load_data()` that specifically retrieves the 'projects' data.
#' It loads the 'projects' data object and coerces it into a tibble before returning.
#'
#' @return A tibble containing the 'projects' data.
#' @export
#' @examples
#' \dontrun{
#' projects_data <- projects()
#' }
projects <- function (){
  load_data("projects") |>
    tibble::tibble()
}

#' Retrieve 'approvals' Data as a Tibble
#'
#' This function is a wrapper for `load_data()` that specifically retrieves the 'approvals' data.
#' It loads the 'approvals' data object and coerces it into a tibble before returning.
#'
#' @return A tibble containing the 'approvals' data.
#' @export
#' @examples
#' \dontrun{
#' approvals_data <- approvals()
#' }
approvals <- function(){
  load_data("approvals")|>
    tibble::tibble()
}
