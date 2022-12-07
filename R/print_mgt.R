#' Print the triggered management operations from the mgt outputs for a certain HRU
#'
#' print_triggered_mgt extracts the triggered management operation which are written
#' into mgt_out.txt and reformats them to resemble the scheduled operations in
#' managment.sch to ease a comparison between them.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To print the management at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param hru_id id of the HRU for which the triggered management operations should
#'   be printed
#' @param start_year Integer value to define the first year of printing.
#' @param end_year Integer value to define the last year of printing.
#'
#' @return Prints a tibble with triggered operations to the R console.
#'
#' @importFrom dplyr filter mutate rename select %>%
#' @export
#'
print_triggered_mgt <- function(sim_verify, hru_id, start_year = 1900, end_year = 2100) {
  cat('Triggered managament for\n', ' hru:       ', hru_id, '\n',
      ' management:', sim_verify$lum_mgt$mgt[sim_verify$lum_mgt$id == hru_id], '\n\n')
  sim_verify$mgt_out %>%
    filter(hru == hru_id) %>%
    filter(year %in% start_year:end_year) %>%
    rename(op_data1 = op_typ,
           op_data3 = var1) %>%
    mutate(op_data3 = ifelse(operation != 'FERT', 0, op_data3)) %>%
    # mutate()
    # mutate(date = ymd(paste(year, mon, day, sep = '-'))) %>%
    select(., year, mon, day, phuplant, operation, op_data1, op_data3) %>%
    print(., n = Inf)
}

#' Boxplot for relevant variables at harvest-kill
#'
#' plot_variable_at_harvkill plots boxplots of one of the variables crop heat unit
#' fractions ('phu'), crop yields ('yield'), or plant biomass ('bioms') for crops
#' at harvest-kill of a crop separated for all identified crops.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param variable Selected variable to be plotted. Must be one of: 'phu', 'yield', 'bioms'
#' @param years Simulated years which are aggregated in the boxplot
#'
#' @return ggplot boxplot the selected variable at harvest-kill.
#'
#' @importFrom dplyr filter group_by mutate rename select ungroup %>%
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_hline labs theme_bw
#' @importFrom purrr set_names
#' @export
#'
plot_variable_at_harvkill <- function(sim_verify, variable, years = 1900:2100) {
  tbl_harv <- sim_verify$mgt_out %>%
    filter(year %in% years) %>%
    filter(operation %in% c('HARVEST', 'KILL')) %>%
    group_by(hru,year, mon, day) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2, operation == 'HARVEST')

  if(variable == 'phu') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, phuplant) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Crop HUs at harvest/kill'
  }else if(variable == 'yield') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, op_var) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Crop yield at harvest/kill'
  } else if(variable == 'bioms') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, plant_bioms) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Biomass at harvest/kill'
  } else {
    stop("Variable must be one of: 'phu', 'yield', 'bioms'")
  }

  gg <- ggplot(data = tbl_harv) +
    geom_boxplot(aes(x = crop, y = var)) +
    labs(x = 'Crops', y = 'Crop HUs at harvest/kill') +
    theme_bw()

  if(variable == 'phu') {
    gg + geom_hline(yintercept = 1, lty = 2)
  }

  return(gg)
}
