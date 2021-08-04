#' Add methods to print function
#'
#' @param x A consort.list, consort.plot or consort object.
#' @param ... Not used.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}, \code{\link{consort_plot}},\code{\link{build_consort}}

#'
#' @export
#' @importFrom grid grid.draw grid.newpage viewport vpTree pushViewport seekViewport upViewport vpList gpar rectGrob roundrectGrob
print <- function (x, ...) {
  UseMethod("print", x)
}

print.consort.plot <- function(x, ...) {

  requireNamespace("grid", quietly = TRUE)

  label_list <- x$label_list
  consort_list <- x$consort_list

  widths = c(0.1, 0.9)

  grid.newpage()
  if(is.null(label_list)){
    for(i in consort_list){
      print(i)
    }
  }else{
    widths <- c(0.05, widths, 0.05)
    top.vp <- viewport(layout=grid.layout(1, 4,
                                          widths  = widths,
                                          heights = c(1, 1)))
    lab_vp <- viewport(layout.pos.row = 1, layout.pos.col = 2, name = "label")
    con_vp <- viewport(layout.pos.row = 1, layout.pos.col = 3, name = "consort")

    splot <- vpTree(top.vp, vpList(lab_vp, con_vp))
    pushViewport(splot)
    seekViewport("label")
    for(i in label_list){
      print(i, ...)
    }
    seekViewport("consort")
    for(i in consort_list){
      print(i, ...)
    }
    upViewport(0)
  }
}

#' @export
print.consort.list <- function(x, ...) {
  for(box in x)
    print(box, ...)
}

#' @export
print.consort <- function(x, ...) {
  grid::grid.draw(x, ...)
  if(!is.null(attr(x, "connect")))
    grid::grid.draw(attr(x, "connect"), ...)
}


#' grid.draw method
#'
#' This method is to support saving plots with `ggplot2::ggsave`.
#'
#' @param x A consort.plot object.
#' @export
grid.draw.consort.plot <- function(x){
  print.consort.plot(x)
}



