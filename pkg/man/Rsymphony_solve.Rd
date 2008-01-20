\name{Rsymphony_solve_LP}
\alias{Rsymphony_solve_LP}

\title{COIN-OR Symphony Linear and Mixed Integer Programming Solver}
\description{
  High level R interface to COIN-OR Symphony for solving
  linear as well as mixed integer linear programming problems (MILP).
}
\usage{
Rsymphony_solve_LP(obj, mat, dir, rhs, int = NULL,
                max = FALSE, bounds = NULL)
}
\arguments{
  \item{obj}{a vector with the objective coefficients}
  \item{mat}{a vector or a matrix of the constraint coefficients}
  \item{dir}{a character vector with the directions of the constraints
        (\code{"<","<=", ...})} 
  \item{rhs}{the right hand side of the linear problem}
  \item{int}{a vector with indices representing which objective
    coefficients should be of type integer}
  \item{max}{the direction of the optimization. \code{TRUE} means that the
    objective is to maximize the linear problem, \code{FALSE} (default) means
    to minimize it}
  \item{bounds}{a list containing an element \sQuote{upper} as well as an
  element \sQuote{lower} containing the indices and corresponding bounds of the
  objective variable. The default for each variable is a bound between 0
  and \code{Inf} (\code{NULL} or not listed in \sQuote{bounds}).}
}
\details{
  Symphony is an open source solver for solving
  mixed integer linear programs (MILPs). The current version can
  be found at
  \url{https://projects.coin-or.org/SYMPHONY}. This package uses the C
  interface of the provided callable library. A high level function in R
  to this low level C interface is supplied.
}
\value{
  A list containing the optimal solution,  the corresponding
  values of the objectives and the status information of the solution (0
  -> optimal solution or some status code otherwise).
}
\references{
  \item{SYMPHONY development home page}{
  \url{https://projects.coin-or.org/SYMPHONY/wiki}}
}
\author{Reinhard Harter and Kurt Hornik and Stefan Theussl}
%\note{}
\seealso{
  \item{\code{\link[lpSolve]{lp}}}{{in \pkg{lpSolve}}}
  \item{\code{\link[Rglpk]{Rglpk_solve}}}{{in package \pkg{Rglpk}}}
}
\examples{
## simple linear program (Ex. 17 of Hanke/Nettekoven 2003: "Linear 
## Programming")

obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow=3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rsymphony_solve_LP(obj, mat, dir, rhs, max=max)

## simple mixed integer program (Ex. 54 of Hanke/Nettekoven 2003: 
## "Linear Programming")

obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow=3)
dir <- c("<=", "<=", "<=")
rhs <- c(4, 2, 3)
int <- c(1L, 3L)
max <- TRUE

Rsymphony_solve_LP(obj, mat, dir, rhs, int, max)
}
\keyword{optimize}

