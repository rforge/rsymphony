ex3 <-
function()
{
    mat <- matrix(c(3,1,1,1,-1,2,1,1,-1),ncol=3,nrow=3,byrow=TRUE)
    obj <- c(2, -1, 1)
    dir <- rep("<=", 3)
    max <- TRUE
    int <- rep(FALSE, 3)
    rhs <- c(6, 1, 2)

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}

