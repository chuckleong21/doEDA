context("import tidy excel data")
library(doEDA)

test_that("file is xls format or xlsx format", {
        csv <- "a.csv"
        expect_error(tidy_excel(csv))
        xcel <- readxl::readxl_example("datasets.xlsx")
        expect_equal(length(readxl::excel_sheets(xcel)), 4)
        sheet_alias <- readxl::excel_sheets(xcel)
        out <- tidy_excel(xcel)
        expect_true(all(colnames(out) != sheet_alias))
        expect_true(is.data.frame(out))
})
