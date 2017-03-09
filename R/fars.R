#'  Fast csv data Reading
#'
#'  This is a simple function for reading csv
#'
#'@param filename (character) correspond to the full name.
#'@details In case the file does not exist the file a message warning appear
#'@return A object of the tbl_df class.
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'@export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Define filename with specific year
#'
#' This function allows define the year to analyze.
#' @param year An integer or character string that represent the year of study.
#' @return This function returns a specific character string.
#'
#' @examples
#' library(fars)
#' make_filename("2013")
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read data of specific years
#'
#' This is a simple function that filter data in function of the years.
#'
#' @param years An integer o chracter string.
#' @details In case in the year does not exist data a error message apprear.
#' @return  A object of the tbl_df class with two variable: the months and the years.
#' @importFrom dplyr mutate select %>%
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarize  for specific years.
#'
#' This function create summary table with counts of number of records for each month in a specified years.
#'
#' @param years An integer o chracter string.
#' @return This function returns a  object of the tbl_df class that the table with counts.
#' @importFrom dplyr bind_rows group_by summarize %>% n
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_("year", "n")
}

#' Draw Maps with accidents and state
#'
#' This is function for plotting a map of the count of accidents in a specific state in a specific year.
#' @details  If state does not exist a error message apprear. Also there is not accidents in particular state
#' within specified year you will be notified.
#' @param state.num An integer or character string that represent the state to be analyzed.
#' @param year  An integer or character string that that represent the year.
#' @importFrom graphics points
#' @importFrom maps map
#' @importFrom dplyr filter
#' @return This function return a map where the dots represent the accidents
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, "STATE" == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
