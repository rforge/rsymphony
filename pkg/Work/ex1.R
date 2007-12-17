ex1 <-
function()
{
    obj <- c(-1, -1)
    mat <- matrix(c(1,2,2,1),ncol=2,nrow=2)
    dir <- c("<","<")
    rhs <- c(3,3)
    max	<- FALSE
    int <- c(TRUE, TRUE)

    Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)    
}

