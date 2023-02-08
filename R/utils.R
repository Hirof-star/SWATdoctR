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

#' Add './' to run the exe on unix systems
#' @param exe Text string that defines the name of the executable file
#' @param os Text string that defines the operating system
#' @keywords internal
#'
run_os <- function(exe, os) {
  if(os == 'unix') exe <- '.'%//%exe
  return(exe)
}

#' Find the SWAT+ executable file and trigger error if 0 or more than 1
#' file are found.
#'
#' @param project_path Path to the SWAT+ project folder (i.e. TxtInOut).
#' @param os Text string that defines the operating system
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
