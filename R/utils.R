remove_virtual_trees = function(data)
{
  str = "Block"
  if ("BlockID" %in% names(data)) str = "BlockID"
  if (!str %in% names(data)) stop("Unexpected error in remove_virtual_trees: invalid column names. Please report to info@r-lidar.com")
  data[data[[str]] >= 0,]
}
