calc_points <- function(dims=NULL,rectangle_scores=NULL,labels=NULL) {
  
  # Four possible quadrants, so four possible ways to calculate points
  out_all <- apply(dims,2,function(x) {
    m <- -1/(x[2]/x[1])
    b <- x[2] - m*x[1]
    if(abs(m)>1) {
      x_int <- -b/m
      out_x <- seq(from=x_int - 1/abs(m),to=x_int + 1/abs(m),
                   length.out=100) 
    } else if(abs(m)<=1) {
      if(abs(m)>0) {
      out_x <- seq(from=rectangle_scores['max_x']/(2+abs(m)),to=rectangle_scores['min_x']/(2+abs(m)),
                   length.out=100) 
      } else {
        out_x <- seq(from=rectangle_scores['max_x'],to=rectangle_scores['min_x'],
                     length.out=100) 
      }
    }
      points <- out_x*m + b
    
    return(list(xpoints=out_x,ypoints=points))
  })
  names(out_all) <- labels
  out_all <- lapply(out_all,as_data_frame)
  out_all <- bind_rows(out_all)
  out_all$categories <- rep(labels,each=100)
  return(out_all)
}