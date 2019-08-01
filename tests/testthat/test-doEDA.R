context("Generate graph with one line of code")

test_that("pieChart", {
        expect_error(pieChart(as.matrix(iris), Species))
        expect_warning(pieChart(iris, Sepal.Length))
        expect_is(pieChart(iris, Species), "ggplot")
        nolabel <- pieChart(invest, age, labelon = F)
        labelled <- pieChart(invest, age, labelon = T)
        expect_error(expect_identical(nolabel, labelled))
        clockwise <- pieChart(iris, Species, direction = 1)
        anticlockwise <- pieChart(iris, Species, direction = -1)
        expect_error(expect_identical(anticlockwise, clockwise))
        expect_equal(pieChart(iris, Species), pieChart(iris, "Species"))
})

test_that("waffleChart", {
        palette_name <- waffleChart(iris, Species, divisor = 10, palette = "Set1")
        color_names <- waffleChart(iris, Species, divisor = 10,
                                    palette = c("#E41A1C", "#377EB8", "#4DAF4A"))
        expect_error(waffleChart(unlist(iris), Species))
        expect_is(palette_name, "ggplot")
        expect_is(palette_name, "ggplot")
        expect_equal(palette_name, color_names)
        expect_equal(waffleChart(iris, "Species"), waffleChart(iris, Species))
        expect_equal(waffleChart(iris, Species, divisor = nrow(iris) / 100),
                     waffleChart(iris, Species))
})

test_that("horizontal bar chart", {
        matinvest <- as.matrix(invest)
        gg <- hbarChart(invest, vars(gender, age),
                        vars(age), .xVar = gender, age)
        err <- quote({hbarChart(matinvest, vars(gender, age),
                                vars(age), gender, age)})
        expect_error(eval(err))
        expect_is(gg, "ggplot")
        expect_error(hbarChart(invest, vars(gender, age),
                               vars(investment, age), gender, age))
})

