tagReplaceAttribute <- function(x, name, value) {
  x$attribs[[name]] <- value
  x
}

restyleButton <- function(x, variant="primary") {
  tagReplaceAttribute(x, "class", paste("btn btn-", variant, " action-button", sep=""))
}

