as.latex <- function(x, label = NULL,
                     inline = ifelse(is.null(label), TRUE, FALSE),
                     count = ifelse(is.null(label), FALSE, TRUE))
{
  out <- list(alt = x, inline = inline, count = count, label = label)
  class(out) <- "latex"
  return(out)
}


