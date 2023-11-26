utils::globalVariables(c("path_database", "project_id", "date_data"))

#NOTE: THIS NEEDS REVIEW AND DOCUMENTATION!!!!



path_databased <- "S:/data/databased/rda/"
#path_database <- "S:/data/databased/rda/"
#path_log_databse <- "S:/data/databased/log/"

create_path_log <- function (log_name) {
  stringr::str_c(path_databased, "log/", log_name, ".rda")
}

create_path_data <- function (dataset_name) {
  stringr::str_c(path_databased, "rda/", dataset_name, ".rda")
}

get_log <- function (log_name){
  path_log <- create_path_log(log_name)
  load(path_log)
  get(log_name)
}

log_it <- function (log_entry, log_name) {

  get_log (log_name) %>%
    bind_rows(log_entry) %>%
    pipe_assign(str_c (log_name))

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
  save(list=object_name, file = create_path_data(objct_name), envir = envir)
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
#' load_data("approvals")
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
#' projects_data <- projects()
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
#' approvals_data <- approvals()
approvals <- function(){
  load_data("approvals")|>
    tibble::tibble()
}
