ex4 <-
function()
{
    obj <- c(15, 3, 9, 12)    
    mat <- matrix(c(1,-2,4,3,-4,6,5,-4,2,-3,3,8),ncol=3,nrow=4,byrow=TRUE)
    dir <- rep("<=", 3)
    rhs <- c(10, 20, 25)
    int <- rep(FALSE, 3)
    max <- TRUE

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}

