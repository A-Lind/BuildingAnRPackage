#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#' @param year Numeric. A number to use for the custom filename
#'
#' @return A string with the custom filename
#'
#' @examples
#'  make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' \dontrun{fars_read_years(2013:2015)}
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARSÂ´years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   \dontrun{fars_summarize_years(2013:2015)}
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#'  @param year Numeric. A number to use for the custom filename
#'
#'  @return A string with the custom filename
#'
#'  @examples
#'  make_filename(2015)
#'
#'  @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' fars_read_years(2013:2015)
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARS' years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   fars_summarize_years(2013:2015)
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#'  @param year Numeric. A number to use for the custom filename
#'
#'  @return A string with the custom filename
#'
#'  @examples
#'  make_filename(2015)
#'
#'  @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' \dontrun{fars_read_years(2013:2015)}
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARSÂ´years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   \dontrun{fars_summarize_years(2013:2015)}
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#'  @param year Numeric. A number to use for the custom filename
#'
#'  @return A string with the custom filename
#'
#'  @examples
#'  make_filename(2015)
#'
#'  @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' fars_read_years(2013:2015)
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARS' years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   fars_summarize_years(2013:2015)
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#' @param year Numeric. A number to use for the custom filename
#'
#' @return A string with the custom filename
#'
#' @examples
#'  make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' \dontrun{fars_read_years(2013:2015)}
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARSÂ´years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   \dontrun{fars_summarize_years(2013:2015)}
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#'  @param year Numeric. A number to use for the custom filename
#'
#'  @return A string with the custom filename
#'
#'  @examples
#'  make_filename(2015)
#'
#'  @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' fars_read_years(2013:2015)
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARS' years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   fars_summarize_years(2013:2015)
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#'  @param year Numeric. A number to use for the custom filename
#'
#'  @return A string with the custom filename
#'
#'  @examples
#'  make_filename(2015)
#'
#'  @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#'   @param years Numeric vector of years to read
#'
#'   @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#'   @importFrom dplyr mutate
#'   @importFrom dplyr select
#'   @importFrom magrittr "%>%"
#'
#'   @examples
#' \dontrun{fars_read_years(2013:2015)}
#'   @export
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#'   @param years Numeric vector of FARSÂ´years to read
#'
#'   @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#'   @importFrom dplyr bind_rows
#'   @importFrom dplyr group_by
#'   @importFrom dplyr summarize
#'   @importFrom tidyr spread
#'
#'   @examples
#'   \dontrun{fars_summarize_years(2013:2015)}
#'
#'   @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
#' Read a CSV-file into R silently
#'
#' This is a function that checks if a file exists (the (file specified in the \code{filename} argument)) -
#' throws an error if it does not - and then silently uses read_csv from the readr-package to read the csv-file
#'  into the current environment.
#'
#'  @param filename A character string giving the file the function will read into R
#'
#'  @return This function returns a data frame (a tibble) representing the data in the csv-file
#'
#'  @importFrom readr read_csv
#'  @importFrom dplyr tbl_df
#'
#'  @examples
#' write.csv("testdata.csv", data.frame(x = seq(1,10, 1), y = seq(1,10, 1), stringsAsFactors = F))
#' fars_read("testdata.csv")
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

#' Make a filename for the FARS files
#'
#' This function creates custom filenames based on a year for the FARS files.
#'
#' @param year Numeric. A number to use for the custom filename
#'
#' @return A string with the custom filename
#'
#' @examples
#'  make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import multiple years of data
#'
#' This function takes a vector of years and batch reads them into R. If there is no matching data for one or more of
#' the years specified in the argument \code{years}, the function throws a warning.
#'
#' @param years Numeric vector of years to read
#'
#' @return A list of data frames (tibbles). Each data frame consists of two columns: MONTH and year
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_read_years(2013:2015)
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

#' Summarize FARS' years
#'
#' This function counts the number of observations by month for the years specified by the \code{years} argument
#'
#' @param years Numeric vector of FARS' years to read
#'
#' @return A wide format data frame (tibble). One column for year and MONTH. Each value is the count of observations
#' for the given year-month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#'   fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map for a given year and state
#'
#' This function takes two arguments State Number \code{state.num} and Year \code{year} and draws a map with a point
#' pr. observation. It throws an error if state.num and/or year is not the dataset.
#' It returns NULL if no observations to plot.
#' The function uses the previously defined functions make_filename and fars_read.
#'
#' @param state.num Integer. The state identifier
#' @param year Numeric. Year of the FARS data
#'
#' @return A map
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
