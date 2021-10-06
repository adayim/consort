#' Add methods to print function
#' 
#' Method for create viewports for the \code{consort.list}, \code{consort.plot} 
#' or \code{consort} objects and display the output in on a grid device.
#'
#' @param x A \code{consort.list}, \code{consort.plot} or \code{consort}.
#' @param ... Not used.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}, \code{\link{consort_plot}},\code{\link{build_consort}}
#' 
#' @return None.
#'
#' @export
print.consort.plot <- function(x, ...) {

  label_list <- x$label_list
  consort_list <- x$consort_list

  widths = c(0.1, 0.9)

  grid::grid.newpage()
  if(is.null(label_list)){
    for(i in consort_list){
      print(i)
    }
  }else{
    widths <- c(0.05, widths, 0.05)
    top.vp <- grid::viewport(layout=grid.layout(1, 4,
                                          widths  = widths,
                                          heights = c(1, 1)))
    lab_vp <- grid::viewport(layout.pos.row = 1, layout.pos.col = 2, name = "label")
    con_vp <- grid::viewport(layout.pos.row = 1, layout.pos.col = 3, name = "consort")

    splot <- grid::vpTree(top.vp, grid::vpList(lab_vp, con_vp))
    grid::pushViewport(splot)
    grid::seekViewport("label")
    for(i in label_list){
      print(i, ...)
    }
    grid::seekViewport("consort")
    for(i in consort_list){
      print(i, ...)
    }
    grid::upViewport(0)
  }
}

#' Print Consort Plots
#' @param x an object of \code{consort.list}.
#' @param ... further arguments passed to \code{print} or \code{grid::grid.draw}.
#' 
#' @return None
#' @name print.consort
#' @export
print.consort.list <- function(x, ...) {
  x <- x[lengths(x) != 0]
  for(box in x)
    print(box, ...)
}

#' @rdname print.consort
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
#' @rdname print.consort
#' @export
grid.draw.consort.plot <- function(x){
  print.consort.plot(x)
}



