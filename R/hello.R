# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title The Template `hello` Function.
#' @description Write hello to the console.
#' @param name the name of the person/thing to say hello to. Default is `NULL`
#' meaning no-one/-thing.
#' @param invisible should the return be invisible? Default is `FALSE` (no).
#' @return A `character` vector saying hello.
#' @export
hello <- function(name = NULL, invisible = FALSE) {
  if (is.null(name)) {
    ret <- "Hello!"
  } else {
    ret <- paste0("Hello ", name, "!")
  }
  if (invisible) {
    return(invisible(ret))
  } else {
    return(ret)
  }
}
