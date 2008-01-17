Rsymphony_solve <-
function(obj, mat, dir, rhs, int = NULL, max = FALSE, bounds = NULL)
{
  ## direction of optimization
  if(!is.logical(max))
    stop("'max' can be either TRUE or FALSE")
  nr <- nrow(mat)
  nc <- ncol(mat)

  ## Handle directions of constraints: add some error checking
  ## eventually ...
  TABLE <- c("L", "L", "E", "E", "G", "G")
  names(TABLE) <- c('<', '<=', "==", "=", ">", ">=")
  row_sense <- TABLE[dir]
  if(any(is.na(row_sense)))
    stop("'dir' must be either '<', '<=', '>', '>=', '==' or '='!")
  
  ## bounding support with using Rglpk bounds for the time being ...
  if(is.null(bounds))
    bounds <- list()
  bounds <- glp_bounds(bounds, nc)

  ## use machine's max double values for infinities for the time being ...
  col_lb <- replace(bounds[,2], bounds[,2]==-Inf, -.Machine$double.xmax)
  col_ub <- replace(bounds[,3], bounds[,3]==Inf, .Machine$double.xmax)
  
  ## Note that the integer spec passed on is a vector of integer
  ## indicators.
  if(is.null(int))
    int <- logical(nc)
  else if(is.integer(int))
    int <- ifelse(seq_len(nc) %in% int, TRUE, FALSE)
  else stop("'int' must be a vector of integer indicators") 

  mat <- make_csc_matrix(mat)
  
  ## Call the C interface.
  out <- .C("R_symphony_solve",
            as.integer(nc),
            as.integer(nr),
            as.integer(mat$matbeg),
            as.integer(mat$matind),
            as.double(mat$values),
            as.double(col_lb),
            as.double(col_ub),
            as.integer(int),
            if(max) as.double(-obj) else as.double(obj),
            obj2 = double(nc),              
            as.character(paste(row_sense, collapse = "")),
            as.double(rhs),
            double(),
            objval = double(1L),
            solution = double(nc),
            status = integer(1L))
  
  status_db <- 
    c("TM_NO_PROBLEM" = 225L,
      "TM_NO_SOLUTION" = 226L,
      "TM_OPTIMAL_SOLUTION_FOUND" = 227L,
      "TM_TIME_LIMIT_EXCEEDED" = 228L,
      "TM_NODE_LIMIT_EXCEEDED" = 229L,
      "TM_TARGET_GAP_ACHIEVED" = 230L,
      "TM_FOUND_FIRST_FEASIBLE" = 231L,
      "TM_FINISHED" = 232L,
      "TM_UNFINISHED" = 233L,
      "TM_FEASIBLE_SOLUTION_FOUND" = 240L,
      "TM_SIGNAL_CAUGHT" = 235L,
      "TM_ERROR__NO_BRANCHING_CANDIDATE" = -251L,
      "TM_ERROR__ILLEGAL_RETURN_CODE" = -252L,
      "TM_ERROR__NUMERICAL_INSTABILITY" = -253L,
      "TM_ERROR__COMM_ERROR" = -254L,
      "TM_ERROR__USER" = -275L) 
  status <- if(out$status == 227L)
    c("TM_OPTIMAL_SOLUTION_FOUND" = 0L)
  else
    status_db[match(out$status, status_db)]
  
  list(solution = out$solution,
       objval = sum(obj * out$solution),
       ## Equivalently,
       ##   if(max) - out$objval else out$objval
       status = status)
}        
