make_progress <- function(progress, iterations)
{
  if (is.null(progress)) {
    # Console mode
    pb <- utils::txtProgressBar(min = 0, max = iterations, style = 3)
    force(pb)
    return(list(
      tick = function(i, detail = "") utils::setTxtProgressBar(pb, i),
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
