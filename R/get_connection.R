
# Generate connections
#' @keywords internal
get_connect <- function(consort_plot){
  
  connection <- sapply(names(consort_plot), function(x){
    prev_node <- consort_plot[[x]]$prev_node
    
    if(is.null(prev_node))
      return(NULL)
    else
      list(node = c(x, prev_node),
           connect = ifelse(is.null(consort_plot[[x]]$side), "bt",
                            ifelse(consort_plot[[x]]$side== "right", "bl", "br")) )
    
  })
  Filter(Negate(is.null), connection)
  
}
