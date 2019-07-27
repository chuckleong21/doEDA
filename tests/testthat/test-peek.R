context('Peek workflow')
library(doEDA)


test_that("peek applies to data.frame objects", {
        mat <- matrix(1:10, nrow = 2)
        expect_error(peek(mat))
})

test_that("peek returns a list object called peekture", {
        dat <- data.frame(1:10)
        peek(dat)
        expect_equal(class(peekture), "list")
})

test_that("peek outputs a ggplot object", {
        mat <- matrix(1:10, nrow = 2)
        dat <- as.data.frame(mat)
        expect_true(ggplot2::is.ggplot(peek(dat)))
})
