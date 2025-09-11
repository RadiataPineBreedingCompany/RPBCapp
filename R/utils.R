remove_virtual_trees = function(data)
{
  str = BLOCKNAME
  if ("BlockID" %in% names(data)) str = "BlockID"
  if (!str %in% names(data))
    stop(paste0("Unexpected error in remove_virtual_trees: invalid column names. There is no '", str, "' attribute.  Please report to info@r-lidar.com"))
  data[data[[str]] >= 0,]
}

xls_find_sheet = function(file, valid_sheet_names)
{
  sheet_names <- readxl::excel_sheets(file)
  lc_sheet_names = tolower(sheet_names)
  lc_valid_sheet_names = tolower(valid_sheet_names)

  res = match(lc_valid_sheet_names, lc_sheet_names)
  res = na.omit(res)

  if (length(res) == 0)
  {
    msg = paste(valid_sheet_names, collapse = "' or '")
    stop(paste0("Excel sheet '", msg, "' not found in the Excel file", collapse = " "))
  }

  res = res[1]
  return(sheet_names[res])
}
