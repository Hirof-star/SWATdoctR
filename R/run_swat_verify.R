#' Run simulations for model verification
#'
#' This function allows to run a SWAT2012 project in R.
#' Basic settings for the SWAT run such as the simulation period or the time
#' interval for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path Character string, path to the SWAT+ project folder
#'   (i.e. TxtInOut).
#' @param outputs Define the outputs that should be read after the simulation
#'   run. The outputs that are defined here depend on the verification steps
#'   that should be performed on the outputs.
#'
#' @return Returns the simulation results for the defined output variables as a
#'   list tibble.
#'
#' @importFrom dplyr %>%
#' @importFrom processx run
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export
#'
run_swat_verification <- function(project_path, outputs = c('wb', 'mgt', 'plt')) {

  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(all(outputs %in% c('wb', 'mgt', 'plt')))
  stopifnot(is.logical(keep_folder))

  ## Get operating system and find SWAT executable file
  os <- get_os()
  swat_exe <- find_swat_exe(project_path, os)

  msg <- run(run_os(swat_exe, os), wd = project_path,
             error_on_status = FALSE)

  if(msg$timeout) {
    out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
      .[max(1, length(.) - 10):length(.)]
    err_msg <- c(paste0('Simulation timed out after ', time_out, ' sec'),
                 'Simulation run:', out_msg)
    model_output <- err_msg
  } else if(nchar(msg$stderr) == 0) {
    model_output <- list()

    if ('plt' %in% outputs) {
      model_output$hru_pw_day <- read_tbl('hru_pw_day', sim_path)
    }
    if ('wb' %in% outputs) {
      model_output$basin_wb_day <- read_tbl('basin_wb_day', sim_path)
    }
    if ('mgt' %in% outputs) {
      model_output$mgt_out <- read_mgt(sim_path)
    }
  }

}

#' Identify the operating system.
#' The function was written by Will Lowe and was copied from here:
#' https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#'
#' @keywords internal
#'
get_os <- function() {
  sysinf <- Sys.info()

  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }

    # If rare case occurs that Sys.info() is NULL
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}

#' Find the SWAT+ executable file and trigger error if 0 or more than 1
#' file are found.
#'
#' @keywords internal
#'
find_swat_exe <- function(project_path, os) {
  if(os == "windows") {
    swat_exe <- list.files(project_path) %>%
      .[grepl(".exe$",.)]

  } else if(os == "linux") {
    swat_exe <- system("find"%&&%project_path%&&%"-executable -type f",
                       intern = T) %>%
      basename(.)
  } else if (os == 'osx') {
    stop('Functionality not tested for Mac. Therefore run aborted')
  }

  # Make sure that there is exactly one executable in the SWAT project folder
  if(length(swat_exe) == 0) stop("No SWAT executable found in the project folder!")
  if(length(swat_exe) > 1) stop("Project folder contains more than one executable!")

  return(swat_exe)
}

#' Read SWAT+ output that is arranged in a tabular format (most outputs)
#' and return the read output table in a tibble format
#'
#' @importFrom data.table fread
#' @importFrom purrr set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
read_tbl <- function(out_file, sim_path) {
  file_path <- paste0(sim_path, '/', out_file, '.txt')

  col_names <- read_lines(file = file_path, skip = 1, n_max = 1) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()

  name_duplicate <- table(col_names) %>%
    .[. > 1]

  for (i in 1:length(name_duplicate)) {
    col_names[col_names == names(name_duplicate[i])] <-
      paste0(names(name_duplicate[i]), 1:name_duplicate[i])
  }

  fread(file_path, skip = 3) %>%
    set_names(., col_names) %>%
    tibble(.)
}

#' Read SWAT+ management output file and return the read output in a tibble
#'
#' @importFrom purrr map set_names
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom_lines
#'
#' @keywords internal
#'
read_mgt <- function(sim_path) {
  file_path <- paste0(sim_path, '/mgt_out1.txt')

  vroom_lines(file_path, skip = 3) %>%
    str_trim(.) %>%
    str_split(., '\t[:space:]+|[:space:]+') %>%
    map(., ~ .x[1:21]) %>%
    unlist() %>%
    matrix(., nrow = 21) %>%
    t() %>%
    as_tibble(., .name_repair = 'minimal') %>%
    set_names(., c('hru', 'year', 'mon', 'day', 'op_typ', 'operation',
                   'phubase', 'phuplant', 'soil_water', 'plant_bioms',
                   'surf_rsd', 'soil_no3', 'soil_solp', 'op_var',
                   paste0('var', 1:7)))
}
