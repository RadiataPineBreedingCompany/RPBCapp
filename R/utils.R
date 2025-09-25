find_tree_zero = function(plan)
{
  tree_zero = plan[plan[[BLOCKNAME]] == 1 & plan[[TPOSNAME]] == 1, ]
  as.numeric(sf::st_coordinates(sf::st_geometry(tree_zero)))
}

xls_find_sheet = function(file, valid_sheet_names, mustWork = TRUE)
{
  sheet_names <- readxl::excel_sheets(file)
  lc_sheet_names = tolower(sheet_names)
  lc_valid_sheet_names = tolower(valid_sheet_names)

  res = match(lc_valid_sheet_names, lc_sheet_names)
  res = na.omit(res)

  if (length(res) == 0 & mustWork)
  {
    msg = paste(valid_sheet_names, collapse = "' or '")
    stop(paste0("Excel sheet '", msg, "' not found in the Excel file", collapse = " "))
  }

  if (length(res) == 0 & !mustWork)
  {
    return(NULL)
  }

  res = res[1]
  return(sheet_names[res])
}

is.NullOb <- function(x)
{
  if(!(is.function(x))) is.null(x) | all(sapply(x, is.null)) else FALSE
}

rmNullObs <- function(x)
{
  if(!(is.function(x)))
  {
    x = x[!(sapply(x, is.NullOb))]
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
}

df_find_column = function(df, valid_col_names, mustWork = TRUE)
{
  col_names <- colnames(df)
  lc_col_names <- tolower(col_names)
  lc_valid_col_names <- tolower(valid_col_names)

  res <- match(lc_valid_col_names, lc_col_names)
  res <- na.omit(res)

  if (length(res) == 0 && mustWork)
  {
    msg <- paste(valid_col_names, collapse = "' or '")
    stop(paste0("Column '", msg, "' not found in the data frame"))
  }

  if (length(res) == 0 && !mustWork) {
    return(NULL)
  }

  res <- res[1]
  return(col_names[res])
}

required <- function(x, msg)
{
  if (is.null(x)) stop(msg, call. = FALSE)
}

assert_file_exists = function(file)
{
  if (!file.exists(file))
    stop(paste0("File not found: ", file))
}

assert_sf_polygon = function(sf)
{
  if (any(sf::st_geometry_type(sf) != "POLYGON"))
    stop("Entities are expected to be POLYGON ")
}

assert_sf_point = function(sf)
{
  if (any(sf::st_geometry_type(sf) != "POINT"))
    stop("Entities are expected to be POINT")
}

assert_point_cloud_loaded = function(las)
{
  if (is.null(las))
    stop("No point cloud loaded yet")
}

assert_file_ext <- function(file, expected_ext)
{
  ext <- tools::file_ext(file)
  if (tolower(ext) != tolower(expected_ext)) {
    stop(sprintf(
      "Invalid file extension: '%s'. Expected '.%s'.",
      ext, expected_ext
    ), call. = FALSE)
  }
  invisible(TRUE)
}
