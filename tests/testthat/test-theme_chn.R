context("ggplot2 theme wrapped for Chinese characters")

test_that("theme object", {
        expect_is(theme_chn(), "gg")
        expect_is(theme_chn(), "theme")
        expect_equal(typeof(theme_chn()), "list")
})
