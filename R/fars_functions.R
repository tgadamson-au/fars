#' @title Read csv file and return it as a tibble dataframe
#'
#' @description Reads data from bzipped csv files from the US National Highway Traffic Safety Administrations's
#' Fatality Analysis Reporting System
#'
#' @note Will create an error if the file does not exist at the path
#'
#' @param filename A filepath character string of a csv file to be read to a tibble
#'
#' @return Returns a tibble of the csv
#'
#' @examples \dontrun{
#' path <- "accident_2013.csv.bz"
#' fars_read(path)
#'
#' fars_read("accident_2013.csv.bz")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title Create "accident_<year>.csv.bz2" filename string
#'
#' @description Coerces input to integer(s) and inserts into "accident_<year>.csv.bz2" at '<year>' returning the result.
#'
#' @note If input cannot be coerced to integer(s) warning will result and NA(s) will be returned.
#'
#' @param year A numeric, integer or numeric string that will be coerced to an integer and inserted into the
#' "accident_<year>.csv.bz2" string at <year>.
#'
#' @return Returns string(a) "accident_<year>.csv.bz2" with intergerised input at '<year>'
#'
#' @examples \dontrun{
#' make_filename(2018)
#' make_filename('2018')
#' }
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title Read multiple years of fars files
#'
#' @description Takes a list or vector of years and returns a list of tibbles containing the
#' MONTH column for corresponding fars files in the working directory with an appended year column.
#'
#' @note If file does not exist for corresponding year a warning will be generated and NULL returned for that year
#'
#' @param years A list or vector of numerics, integers or numeric/integer strings to read "accident_<year>.csv.bz2"
#'  files from the working directory.
#'
#' @return Returns a list of tibbles containing the MONTH Column of the read file and an appended year column.
#'
#' @examples \dontrun{
#' fars_read_years(2018)
#' fars_read_years('2018')
#' fars_read_years(c(2017, 2018))
#' fars_read_years(list(2017, 2018))
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
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


#' @title Summarise number of records per month contained in fars file(s)
#'
#' @description Takes a list or vector of years and returns a tibble with number of records month by year for
#' corresponding fars csv files in the working directory.
#'
#' @note If file does not exist for corresponding year a warning will be generated.
#'
#' @param years A list or vector of numerics, integers or numeric/integer strings to read "accident_<year>.csv.bz2"
#'  files from the working directory.
#'
#' @return A tibble contain number of records month by year
#'
#' @examples \dontrun{
#' fars_summarise_years(2018)
#' fars_summarise_years(2018L)
#' fars_summarise_years('2018')
#' fars_summarise_years(c(2017, 2018))
#' fars_summarise_years(list(2017, 2018))
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' @title Map Years Fatal Accidents onto State Map
#'
#' @description Draws statemap and plots fatal accidents from fars file for specified year and state.
#'
#' @note If state number does not exist in file will generate an error
#' If no records for state exist will write message and return an a invisible(NULL)
#'
#' @param state.num Numeric, integer or numeric/integer string corresponding with a US state code.
#' @param year Numeric, integer or numeric/integer string corresponding to "accident_<year>.csv.bz2" file in the
#' working directory
#'
#' @examples \dontrun{
#' fars_map_state(1 , 2018)
#' fars_map_state('1' , '2018')
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
