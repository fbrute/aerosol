#context("Checking mixr mean calculation")
# library(testthat)


testthat::describe("meanForAMonth()",{
        it("month is between 1 and 12", {
                mean <- meanForAMonth(month=13)
                testthat::expect_true(mean == -1)
        })

        it("mean value should be greater than 0", {
                mean <- meanForAMonth(1)
                testthat::expect_true(mean > 0)
        })
})


testthat::describe("getData()",{

        it("data names should contains date", {
                df = getData()
                testthat::expect_true(is.data.frame(df))
                testthat::expect_true("date" %in% names(df.meanmixr))
        })


        it("data should contains expected dates and means1", {
                df.mean = getMeanMixrData()
                expect_true(is.data.frame(df.meanmixr))
                testthat::expect_equal(names(df.meanmixr),c("date","mean_mixr"))
        })

        it("mean value should be greater than 0", {
                mean <- meanForAMonth(1)
                testthat::expect_true(mean > 0)
        })
})

# test.getMeanMixrData <- function() {
#         filename <- "./modis/data/MOD08_D3.051.Optical_Depth_Land_And_Ocean_Mean.01Apr2012.G3.input.txt"
#         df.meanmixr = getData(filename)
#         checkTrue(is.data.frame(df.modis))
#         checkEquals(names(df.modis),c("lat","long","aot"))
#         print(df.modis$aot[1])
#         expect_true(df.modis$aot[1] == 0.134)
}
# testthat::test_that("month to get the mean from is numeric",
# {
#         mean = meanForAMonth(1)
#         testthat::expect_that(mean > 0, is_true())
# }
# )
#
# test.meanForAMonth <- function(month = 1) {
#         checkTrue( month %in% 1:12)
#
#
#
# }
