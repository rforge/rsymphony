source("../R/sparse.R")

requireNamespace("slam")
requireNamespace("Matrix")
requireNamespace("SparseM")

create_test_matrix <- function() {
    nr <- as.integer(runif(1, 4, 500))
    nc <- as.integer(runif(1, 4, 500))
    sparsity <- runif(1)
    n <- as.integer((1 - sparsity) * nr * nc)
    v <- runif(n, -100, 100)
    ij <- expand.grid(seq_len(nr), seq_len(nc))
    i <- sample(seq_len(nrow(ij)), n)
    slam::simple_triplet_matrix(ij[i, 1], ij[i, 2], v,
                                nrow = nr, ncol = nc)
}

test_make_csc_matrix <- function() {
    Ms <- create_test_matrix()          # simple_triplet_matrix
    Md <- as.matrix(Ms)
    Mt <- make_csc_matrix(Ms)
    test <- function(M) {
        stopifnot(all.equal(Mt, make_csc_matrix(M)))
    }
    test(Md)
    test(as(Md, "dgTMatrix"))
    test(as(Md, "dgCMatrix"))
    test(as(Md, "dgRMatrix"))
    test(SparseM::as.matrix.coo(Md))
    test(SparseM::as.matrix.csc(Md))
    test(SparseM::as.matrix.csr(Md))
    invisible(NULL)
}

## test_size <- 1000L
test_size <- 10L
for (i in seq_len(test_size)) {
    cat("Run:", i, "\n")
    test_make_csc_matrix()
}
