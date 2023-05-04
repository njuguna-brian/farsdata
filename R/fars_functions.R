#' Read FARS data from a CSV file and return a tibble
#' This function reads FARS data from CSV files using the function \code{read_csv} from
#' the readr package and then stores it as a tibble using the function \code{tbl_df}
#' from the dplyr package
#' @note fars_read depends on read_csv and tbl_df from the readr and dplyr packages respectively
#' @param filename This is the file name of the FARS data to be read if the data is in the current
#' working directory otherwise it should be the file path of the data
#' @return A tibble containing the data from the CSV file
#'
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv")
#' fars_read("path/to/your/csv/file.csv")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate a file name for FARS data
#'
#' This function generates a file name for FARS data based on a given year.
#'
#' @param year An integer specifying the year for which to generate a file name
#' @return A character string containing the file name for the FARS data for the given year
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for a list of years and return a tibble
#'
#' This function reads FARS data from CSV files for a list of years using the fars_read function and
#' returns a tibble.
#' @param years A list of integers specifying the years for which to read FARS data
#' @return A tibble containing the number of accidents by month and year for all the specified years
#'
#' @importFrom dplyr select mutate %>%
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS data for a list of years and return a tibble
#'
#' This function reads FARS data from CSV files for a list of years using the fars_read_years function and
#' returns a tibble containing the number of accidents by month and year, spread across multiple columns.
#'
#' @param years A list of integers specifying the years for which to summarize FARS data
#' @return A tibble containing the number of accidents by month and year for all the specified years,
#'         spread across multiple columns
#' @note This functions depends on the bind_rows, group_by, summarize functions from dplyr and spread from tidyr
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(`year`, `MONTH`) %>%
    dplyr::summarize(`n` = n()) %>%
    tidyr::spread(`year`, `n`)
}

#' Plot accidents on a map for a given state and year
#'
#' This function plots the location of accidents on a map for a given state and year, using the Federal
#' Highway Administration's Fatality Analysis Reporting System (FARS) data.
#'
#' @param state.num integer code for the state of interest
#' @param year integer year of interest
#' @return A map of the state with the accidents plotted as points
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom  graphics points
#' @examples
#' \dontrun{
#' # plot accidents for state number 10 in year 2014
#' fars_map_state(10, 2014)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
