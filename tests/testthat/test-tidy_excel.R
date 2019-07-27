context("import tidy excel data")
library(doEDA)

test_that("file is xls format or xlsx format", {
        csv <- "a.csv"
        expect_error(tidy_excel(csv))
        xcel <- "~/Desktop/R/数据汇总.xlsx"
        expect_equal(length(readxl::excel_sheets(xcel)), 1)
        sheet_alias <- readxl::excel_sheets(xcel)
        out <- tidy_excel(xcel)
        expect_true(all(colnames(out) != sheet_alias))
        expect_true(is.data.frame(out))
})
