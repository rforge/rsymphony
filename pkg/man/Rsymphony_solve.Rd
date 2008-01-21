\name{Rsymphony_solve_LP}
\alias{Rsymphony_solve_LP}
\title{COIN-OR SYMPHONY Linear and Mixed Integer Programming Solver}
\description{
  High level R interface to the COIN-OR SYMPHONY solver for linear as
  well as mixed integer linear programming problems (MILPs).
}
\usage{
Rsymphony_solve_LP(obj, mat, dir, rhs, int = NULL,
                max = FALSE, bounds = NULL)
}
\arguments{
  \item{obj}{a vector with the objective coefficients}
  \item{mat}{a vector or a matrix of the constraint coefficients}
  \item{dir}{a character vector with the directions of the constraints.
    Each element must be one of \code{"<"}, \code{"<="}, \code{">"},
    \code{">="}, \code{"=="} or \code{"!="}.}
  \item{rhs}{the right hand side of the constraints}
  \item{int}{a vector with indices representing which objective
    coefficients should be of type integer}
  \item{max}{a logical giving the direction of the optimization.
    \code{TRUE} means that the objective is to maximize the objective
    function, \code{FALSE} (default) means to minimize it.}
  \item{bounds}{\code{NULL} (default) or a list with elements
    \code{upper} and \code{lower} containing the indices and
    corresponding bounds of the objective variables.  The default for
    each variable is a bound between 0 and \code{Inf}.}
}
\details{
  SYMPHONY is an open source solver for solving mixed integer linear
  programs (MILPs).  The current version can be found at
  \url{https://projects.coin-or.org/SYMPHONY}.  Package \pkg{Rsymphony}
  uses the C interface of the callable library provided by SYMPHONY, and
  supplies a high level solver function in R using the low level C
  interface.
}
\value{
  A list containing the optimal solution, with the following components.
  \item{solution}{the vector of optimal coefficients}
  \item{objval}{the value of the objective function at the optimum}
  \item{status}{an integer with status information about the solution
    returned: 0 if the optimal solution was found, a non-zero value
    otherwise.}
}
\references{
  SYMPHONY development home page
  (\url{https://projects.coin-or.org/SYMPHONY/wiki}).
}
\author{
  Reinhard Harter, Kurt Hornik and Stefan Theussl
}
\seealso{
  \code{\link[lpSolve]{lp}} in package \pkg{lpSolve};
  \code{\link[Rglpk]{Rglpk_solve_LP}} in package \pkg{Rglpk}.
}
\examples{
## simple linear program (Ex. 17 of Hanke/Nettekoven 2003: "Linear 
## Programming")

obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rsymphony_solve_LP(obj, mat, dir, rhs, max = max)

## simple mixed integer program (Ex. 54 of Hanke/Nettekoven 2003: 
## "Linear Programming")

obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(4, 2, 3)
int <- c(1L, 3L)
max <- TRUE

Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}
\keyword{optimize}
