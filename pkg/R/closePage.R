closePage <- function(page, indent = "  ", splash = FALSE) {
  hwriterlink <- hwrite('hwriter5',
                        link = 'http://www.ebi.ac.uk/~gpau/hwriter/index.html')
  if (splash) {
    hwrite(paste0('\n<br/><br/><font size = \"-2\">(Page generated on ',
                  date(), ' by ', hwriterlink, ' ',
                  (sessionInfo()$otherPkgs)[['hwriter5']]$Version,
                  ')</font>'), page, br = TRUE)
  } else {
    hwrite('\n<br/><br/>', page, br = TRUE)
  }
  hwrite(paste0(indent, '</body>\n', indent, '</html>'), page, br = FALSE)
  close(page)
}

