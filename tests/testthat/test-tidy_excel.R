context("import tidy excel data")
library(doEDA)
library(readxl)

xcel <- readxl_example("datasets.xlsx")
sheet_alias <- excel_sheets(xcel)
out <- tidy_excel(xcel)


test_that("file is xls format or xlsx format", {
        csv <- "a.csv"
        expect_error(tidy_excel(csv))
})

test_that("output type is default to a list", {
        expect_true(is.list(out))
})

test_that("output length and the number of sheet names are equal", {
        expect_equal(length(sheet_alias), length(out))
})

test_that("a flattened output is a data.frame", {
        expect_true(is.data.frame(suppressWarnings(tidy_excel(xcel, flatten = TRUE))))
})

test_that("Print warnings when it is necessary", {
        expect_warning(tidy_excel(xcel, flatten = TRUE))
})
