#' Read a file
#'
#' This is a simple function that reads a file from a path. If the file doesn't exist,
#' then print a error information. Otherwise, read this file into a dataframe.
#'
#' @param filename character string giving a pathname of a file
#' @importFrom dplyr tbl_df
#' @return This function returns a dataframe to store data in the file
#'
#' @examples
#' \dontrun{
#' fname <- system.file("extdata","accident_2013.csv.bz2", package="FARS")
#' fars_read(fname)
#' }
#'
#' @export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Create a filename
#'
#' This function create a corresponding filename according to input year.
#'
#' @param year character string indicating year
#' @return This function returns a comparessed .csv file
#'
#' @examples
#' make_filename(2016)
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read files with specific years
#'
#' This function read csv file with specific years into a list of dataframe.
#' If the input year doesn't exist, then a warning information "invalid year" will be printed
#' and return NULL
#'
#' @param years A vector containing multiple character strings
#' @importFrom dplyr mutate select
#' @import readr
#'
#' @return This function returns a list of dataframes with specific years.
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
#'
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

#' Summarize statistical results of FARS
#'
#' This function gives a summary of fata injuries given years
#'
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr spread
#' @import readr
#' @inheritParams fars_read_years
#' @return A dataframe of fatal injuries in given years
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map state-wide injuries in given year
#'
#' This function shows a map that indicates injuries distribution in a state of given year.
#' If misinput the state number, error message will be printed.
#' If no accidents is reported in this state of given yerar, "no accidents to plot" will be
#' printed.
#'
#' @param state.num An integer number representing a state
#' @param year An integer number for a given year
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(17, 2015)
#' }
#'
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


