ex6 <-
function()
{
    obj <- c(3,1,3)
    mat <- matrix(c(-1,0,1,2,4,-3,1,-3,2), nrow=3)
    dir <- c("<=","<=","<=")
    rhs <- c(4,2,3)
    int <- c(TRUE, FALSE, TRUE)
    max <- TRUE

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}
