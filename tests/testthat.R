## -- Two useful resources -- ##
#https://www.rdocumentation.org/packages/testthat/versions/2.0.1
#http://testthat.r-lib.org/reference/index.html

library(testthat)
library(FishSET)

test_check("FishSET")

#test_that("str_length is number of characters", {
#  expect_equal(str_length("a"), 1)
#  expect_equal(str_length("ab"), 2)
#  expect_equal(str_length("abc"), 3)
#})

test_that('date_parser returns a date', {
  expect_true(all.equal(date_parser('2011/10/5'), as.Date('2011/10/5')))
})

test_that('all calc.method outputs are the same for create_expectations', {
 x<- matrix(nrow=1, c(.5,.1,.4,.3,.2,.65,1.6,2.6,2.1,6.5,2.6,2.265,7))  
 expect_equal(polyval(stats::coef(stats::lm(x[, 1:12] ~x[1, 2:13])), x), x, tolerance=.2)
 expect_equal(x * c(x[, 2:13] / x[, 1:12],mean(x[,2:13] / x[, 1:12])), x, tolerance=.6)
})


 