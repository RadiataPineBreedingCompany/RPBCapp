# console and shiny progress bar interface
#' @export
make_progress <- function(progress, iterations)
{
  if (is.null(progress)) {
    # Console mode
    pb <- txtProgressBar(min = 0, max = iterations, style = 3)
    force(pb)
    return(list(
      tick = function(i, detail = "") setTxtProgressBar(pb, i),
      finalize = function() close(pb)
    ))
  } else {
    # Shiny mode
    done <- 0
    return(list(
      tick = function(i, detail = "") {
        delta <- i - done
        done <<- i
        progress(delta/iterations, detail = detail)
      },
      finalize = function() {}
    ))
  }
}
