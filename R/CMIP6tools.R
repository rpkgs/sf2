get_model <- function(file, prefix = "day_|mon_|year_", postfix = "_his|_ssp|_piControl") {
  pattern <- sprintf("(?<=%s).*(?=%s)", prefix, postfix)
  stringr::str_extract(basename(file), pattern)
}

get_scenario <- function(file) {
  stringr::str_extract(basename(file), "[a-z,A-Z,\\-,0-9]*(?=_r\\d)")
}

#' get_RegionalMean
#'
#' @export
get_RegionalMean <- function(f, shp = sf2::bou1_4p) {
  date <- nctools::nc_date(f)
  print(basename(f))
  r <- exactextractr::exact_extract(terra::rast(f), shp,
    "weighted_mean",
    weights = "area"
  )
  value <- as.numeric(r)
  data.table(date, year = lubridate::year(date), value)
}

#' @rdname get_RegionalMean
#' @export
get_RegionalMeans <- function(fs, shp = sf2::bou1_4p) {
  lst <- lapply(fs, get_RegionalMean, shp = shp)
  model <- get_model(fs)
  scenario <- get_scenario(fs)
  Ipaper::melt_list(lst, model = model, scenario = scenario)
}
