.datatable.aware = TRUE

.onAttach <- function(libname, pkgname)
{
  check_update()
}

# Check if the package has more recent version
check_update = function()
{
  msg <- NULL

  last <- get_latest_version()
  curr <- utils::packageVersion("RPBCapp")
  new_version = !is.null(last) && last[1] > curr
  dev_version = is_dev_version(curr)

  # nocov start
  if (new_version)
  {
    if (dev_version)
      msg = paste("RPBCapp", last, "is now available. You are using", curr, "(unstable) \nremotes::install_github('RadiataPineBreedingCompany/RBPCapp')")
    else
      msg = paste("RPBCapp", last, "is now available. You are using", curr, "\nremotes::install_github('RadiataPineBreedingCompany/RBPCapp')")
  }
  else if (dev_version)
  {
    msg = paste("RPBCapp", curr, "is an unstable development version")
  }

  if (!is.null(msg) & interactive()) packageStartupMessage(msg)
  # nocov end

  return(NULL)
}

get_latest_version <- function(repo = "r-lidar/lasR", branch = "main")
{
  url <- "https://raw.githubusercontent.com/RadiataPineBreedingCompany/RPBCapp/refs/heads/main/DESCRIPTION"

  # Try fetching the DESCRIPTION file
  content <- tryCatch({
    readLines(url, warn = FALSE)
  }, error = function(e) return(NULL))

  if (is.null(content)) return(NULL)

  # Parse the DESCRIPTION file
  desc <- tryCatch({
    read.dcf(textConnection(content))
  }, error = function(e) return(NULL))

  if (is.null(desc) || !"Version" %in% colnames(desc))
    return(NULL)

  version <- package_version(desc[1, "Version"])
  return(version)
}

is_dev_version = function(version)
{
  class(version) <- "list"
  version = version[[1]]
  return(length(version) == 4)
}
