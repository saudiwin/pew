calc_points <- function(dims=NULL,rectangle_scores=NULL,labels=NULL) {
  # Four possible quadrants, so four possible ways to calculate points
  out_all <- apply(dims,2,function(x) {
    if(x[1]>0 & x[2]>0) {
      m <- -1/(x[2]/x[1])
      b <- x[2] - m*x[1]
      if(abs(m)>1) 
      {
        out_x <- seq(from=rectangle_scores['max_x']/abs(m),to=rectangle_scores['min_x']/abs(m),
                     length.out=100) 
      } else {
        out_x <- seq(from=rectangle_scores['max_x'],to=rectangle_scores['min_x'],
                     length.out=100) 
      }
      points <- out_x*m + b
    } else if(x[1]>0 & x[2]<0) {
    out_x <- seq(from=0,to=rectangle_scores['min_x'],
                 length.out=100) 
      m <- -1/(x[2]/x[1])
      b <- x[2] - m*x[1]
      points <- out_x*m + b
    } else if(x[1]<0 & x[2]>0) {
      m <- -1/(x[2]/x[1])
      b <- x[2] - m*x[1]
    out_x <- seq(from=rectangle_scores['max_x'],to=0,
                 length.out=100) 

      points <- out_x*m + b
    } else if(x[1]<0 & x[2]<0) {
      m <- -1/(x[2]/x[1])
      b <- x[2] - m*x[1]
      if(abs(m)>1) 
      {
    out_x <- seq(from=rectangle_scores['max_x']/abs(m),to=rectangle_scores['min_x']/abs(m),
                 length.out=100) 
      } else {
        out_x <- seq(from=rectangle_scores['max_x'],to=rectangle_scores['min_x'],
                     length.out=100) 
      }
      
      points <- out_x*m + b
    }
    return(list(xpoints=out_x,ypoints=points))
  })
  names(out_all) <- labels
  out_all <- lapply(out_all,as_data_frame)
  out_all <- bind_rows(out_all)
  out_all$categories <- rep(labels,each=100)
  return(out_all)
}