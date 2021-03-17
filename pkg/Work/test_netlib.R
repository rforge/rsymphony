Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library("slam")
library("ROI")
library("ROI.models.netlib")
library("Rsymphony")


is_milp <- function(op) !all(types(op) == "C")


roi_to_symphony <- function(x) {
    m <- list(obj = as.matrix(terms(objective(x))[["L"]]),
              mat = constraints(x)$L,
              dir = constraints(x)$dir,
              rhs = constraints(x)$rhs,
              bounds = bounds(x),
              types = if (is_milp(x)) types(x) else NULL,
              max = x$maximum)
    m
}


#
# Solve some LPs
#
for (name in netlib()) {
    op <- netlib(name)
    sol <- do.call(Rsymphony_solve_LP, roi_to_symphony(op))
}


#
# Solve MILPs
#
for (name in c("beaconfd", "cycle")) {
    set.seed(0)
    op <- netlib(name)
    op$types <- sample(c("C", "I", "B"), length(objective(op)), TRUE)
    sol <- do.call(Rsymphony_solve_LP, roi_to_symphony(op))
    print(name)
}


## MILP - Example - 1
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
obj <- c(3, 1, 3)
A <- rbind(c(-1,  2,  1),
           c( 0,  4, -3),
           c( 1, -3,  2))
b <- c(4, 2, 3)
bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                  lb = c(-Inf, 2), ub = c(4, 100))

op <- OP(objective = obj,
        constraints = L_constraint(L = A,
                                   dir = c("<=", "<=", "<="),
                                   rhs = b),
        types = c("I", "C", "I"),
        bounds = bounds,
        maximum = TRUE)

sol <- do.call(Rsymphony_solve_LP, roi_to_symphony(op))
sol # c(4, 2.5, 3)


## MILP - Example - 2
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
obj <- c(3, 1, 3)
A <- rbind(c(-1,  2,  1),
           c( 0,  4, -3),
           c( 1, -3,  2))
b <- c(4, 2, 3)

op <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         maximum = TRUE)

sol <- do.call(Rsymphony_solve_LP, roi_to_symphony(op))
sol # c(5, 2.75, 3)


## -----------------------------------------------------------------------------
##
## Pruning Moves [Matteo Fischetti, Domenico Salvagnin] (2009)
##
## -----------------------------------------------------------------------------
## min   - x_1 - x_2 - x_3 - x_4 - 99 x_5
## s.t.  x_1 + x_2 <= 1
##       x_3 + x_4 <= 1
##       x_4 + x_5 <= 1
##       x_i in {0, 1}
op <- OP()
objective(op) <- L_objective(c(-1, -1, -1, -1, -99))
mat <- simple_triplet_matrix(rep(1:3, 2),
                             c(c(1, 3, 4), c(2, 4, 5)),
                             rep(1, 6))
constraints(op) <- L_constraint(mat,
                               dir = leq(3),
                               rhs = rep.int(1, 3))
types(op) <- rep("B", length(objective(op)))
sol <- do.call(Rsymphony_solve_LP, roi_to_symphony(op))
sol # rbind(c(0, 1, 1, 0, 1), c(1, 0, 1, 0, 1))
