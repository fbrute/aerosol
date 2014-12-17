#context("Checking power calculation")

testthat::describe("main_power()",{
        it("check data names", {
                df = main_power()
                testthat::expect_true(is.data.frame(df))
                testthat::expect_true("date" %in% names(df))
                testthat::expect_true("power" %in% names(df))
        })

})
