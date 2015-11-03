newPage <- function(filename, dirname = NULL, title = filename,
                    doctype = "<!DOCTYPE html>\n",
                    html.attributes = "lang='en'",
                    link.javascript = NULL,
                    mathjaxconfig = NULL, mathjax = "default",
                    meta.attributes = "charset='utf-8'",
                    link.css = NULL, css = NULL,
                    head = NULL, head.attributes = NULL,
                    body.attributes = NULL, indent = "  ",
                    onlyContents = FALSE)
{
  ## today <- format(strptime(date(), "%a %b %d %H:%M:%S %Y"), "%B %d, %Y")

  ## Create equation numbers and list for labels
  ## Set up new environment for equation list and equation number counter
  hwriterEquation <- numeric(0)
  hwriterEquationList <- character(0)
  hwriteLatex <- function(ltx, page = NULL, equationStyle = NULL, ...)
  {
    ## cat is used to write to files,
    ## deal with output to standard output
    if (is.null(page)) page = ""
    ## count: add a (#)
    ## label: add before: Equation (#): label
    if (! is(ltx, "latex")) ltx <- as.latex(ltx)
    if (ltx$inline){
      ## inline: directly there in the text
      ## (can't be counted or labeled)
      cat(paste0("\\(", ltx$alt,"\\)"),
          file = page, append = TRUE, sep = " ")
    } else {
      if (is.null(equationStyle)){
        equationStyle <- defaultEquationStyle
        cat(equationStyle, file = page, append = TRUE)
      }
      ## not inline: will be within a table to center it
      if (ltx$count) {
        ## update counter hwriterEquation
        hwriterEquation <- hwriterEquation + 1
        ## deal with label
        if (is.null(ltx$label)){
          hwriterEquationList[hwriterEquation] <- paste0("eq:", hwriterEquation)
          eqnLabel <- paste("eq:", eqnNum, sep = "")
        } else {
          hwriterEquationList[hwriterEquation] <- paste0("eq:", ltx$label)
          eqnLabel <- paste0("eq:", ltx$label)
        }
        ## write out equation as table with equation number
        cat(paste0("\\n<center><table class='equationdisplay'>\\n",
                  "<tr>\\n",
                  "<td='equation'>\\n ",
                  ">\\[", ltx$alt, "\\]</td>\\n",
                  "<td class='equation' id = '",
                    eqnLabel,"'>(", eqnNum,")</td>\\n",
                  "</tr>\\n",
                  "</table>\\n",
                  "</center>\\n",
                  ),
            file = page, append = TRUE)
      } else {
        cat(paste("<center>\\n",
                  "<table class='equationdisplay'>\\n",
                  "<tr>\\n",
                  "<td class='equation'>\\n",
                  "\\[", ltx$alt, "\\]\\n",
                  "</td>\\n",
                  "</tr>\\n",
                  "</table>\\n",
                  "</center>\\n",
                  sep = ""),
            file = page, append = TRUE)
      }
    }
  }

  if (!is.null(dirname)) {
    if (!file.exists(dirname)){
      dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
    }
    filename <- file.path(dirname, filename)
  }
  page <- file(filename, "wt")
  if (!is.null(link.javascript)){
    link.javascript <- paste(hmakeTag("script", type = "text/javascript",
                                      src = link.javascript),
                             collapse = "\n")
  }
  if (!is.null(mathjaxconfig)){
    if (mathjaxconfig == "default") {
      mathJaxConfig  <- defaultMathJaxConfig
    } else {
      mathJaxConfig <- mathjaxconfig
    }
    mathjaxconfig <- paste(hmakeTag("script",
                                    type = "text/x-mathjax-config",
                                    src = mathJaxConfig),
                           collapse = "\n")
    mathjaxconfig <- paste0(indent, mathkjaxconfig)
  }
  if (!is.null(mathjax)){
    if (mathjax == "default") {
      mathJaxSource  <- defaultMathJax
    } else {
      mathJaxSource <- mathjax
    }
    mathjax <- paste(hmakeTag("script", type = "text/javascript",
                              src = mathJaxSource, indent = indent,
                              newline = TRUE),
                     collapse = "\n")
    mathjax <- paste0(indent, mathjax)
  }
  if (!is.null(link.css)){
    link.css <- paste(hmakeTag("link", rel = "stylesheet",
                               type = "text/css", href = link.css,
                               indent = indent, newline = TRUE),
                      collapse = "\n")
  }
  if (!is.null(css)){
    css <- paste(hmakeTag("style", css, indent = indent, newline = TRUE),
                 collapse = "\n")
    ## required by HTML5 validator
    css <- sub("<style", "<style scoped", css, fixed = TRUE)
  }
  head <- paste(hmakeTag("title", title, indent = indent, newline = FALSE),
                head, link.javascript,
                mathjaxconfig, mathjax, link.css, css, sep = "\n")
  head <- do.call(hmakeTag,
                  c(list("head", head, indent = indent, newline = TRUE),
                  head.attributes))
  head <- gsub("(\n)+", "\n", head)
  head <- sub("</head>", paste0(indent, "</head>\n", indent), head,
              fixed = TRUE)
  bodyStart <- do.call(hmakeTag, c(list("body", NULL, indent = indent,
                                        newline = TRUE),
                                   body.attributes))
  bodyStart <- substr(bodyStart, 1, regexpr("</body>", bodyStart) - 1)
  hwrite(paste0(doctype, "<html ", html.attributes, ">\n  ",
                head, bodyStart), page)

  list(p = page, hwriteLatex = hwriteLatex)
}
