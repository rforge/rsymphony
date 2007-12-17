ex2 <-
function()
{
    obj <- c(2, 4, 3)
    mat <- matrix(c(3,4,2,2,1,2,1,3,2),ncol=3,nrow=3,byrow=TRUE)
    dir <- rep("<=", 3)
    rhs <- c(60, 40, 80)
    max <- TRUE
    int <- rep(FALSE, 3)

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)    
}

