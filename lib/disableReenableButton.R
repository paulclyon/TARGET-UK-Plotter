disableReenable <- function(name, func, ...) {
    shinyjs::disable(name)
    withr::defer(shinyjs::enable(name))
    func(...)
}
