library(runr)
context("Input validation")

test_that("incorrect inputs fail", {
            expect_error(run(NA, NA, NA))
            expect_error(run(data.frame(), NA, NA))
            expect_error(run(NA, function(){}, NA))
            expect_error(run(na, na, environment()))
})

test_that("function signature", {
            expect_error(run(
                             data.frame(a=1),
                             function(..., environment, pig) {},
                             environment()),
                         "\\.\\.\\.", fixed = FALSE)
            expect_error(run(data.frame(a=1),
                             function(environment, ...) {},
                             environment()),
                         "ncol\\(data\\) \\+ 2", fixed = FALSE)
            expect_error(run(data.frame(a=1),
                              function(...) {},
                              environment()),
                         "ncol\\(data\\) \\+ 1", fixed = FALSE)
})

test_that("no data", {
            err_val <- "ncol\\(data\\)"
            expect_error(run(data.frame(),
                              function(env, ...) {},
                              environment()),
                         err_val, fixed = FALSE)
            expect_error( run(data.frame(),
                              function(env) {},
                              environment()),
                         err_val, fixed = FALSE)
})

test_that("correct inputs succeed", {
            ## keep this one at the end of this file; update with any change in
            ## input validation
            expect_silent(run(data.frame(a=1, b=2),
                              function(a, b, environment){
                              data.frame()} ,
                              environment()))
            expect_silent(run(data.frame(a=1),
                              function(a, environment = environment()){
                              data.frame()},
                              environment()))
            expect_silent(run(data.frame(a=1),
                              function(a, environment = environment(), ...){
                              data.frame()},
                              environment()))
})
