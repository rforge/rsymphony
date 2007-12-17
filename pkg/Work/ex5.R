ex5 <-
function()
{
    obj <- c(3,5)
    mat <- matrix(c(1,0,0,2,3,2),ncol=2,nrow=3,byrow=TRUE)
    dir <- rep("<=", 3)
    rhs <- c(4, 12, 18)    
    int <- rep(FALSE, 2)
    max <- TRUE

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}

