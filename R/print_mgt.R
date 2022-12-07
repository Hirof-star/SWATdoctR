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
