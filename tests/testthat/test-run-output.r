library(runr)
context("Output validation")
context("-- without manipulation")

test_that("output non-df fails", {
            expect_error(run(data.frame(a=1, b=2),
                              function(a, b , env, ...) {c(a=a, b=b)},
                              environment()),
                         "not data frames", fixed = FALSE)
})
test_that("output identical to input", {
            expect_identical(run(data.frame(a=1, b=2),
                              function(a, b , env, ...) {data.frame(a=a, b=b)},
                              environment()),
                         data.frame(a=1, b=2))
})

context("-- with simple manipulation")
test_that("random, two-column data.frames with no extra data", {
            reps <- ceiling(runif(min=0, max=1, n=10) * 100)
            for(r in reps) {
              aval <- rnorm(1, 0, 65)
              bval <- rgamma(1, 3, 9)
              expect_identical(run(data.frame(a=aval, b=bval),
                                   function(a, b , env, ...) {data.frame(a=rep(a, r),
                                                                         b=rep(b, r))},
                                   environment()),
                               data.frame(a=rep(aval, r),
                                          b=rep(bval, r)))
            }
})

test_that("random, two-column data.frames with extra NA data", {
            reps <- ceiling(runif(min=0, max=1, n=10) * 100)
            for(r in reps) {
              aval <- rnorm(1, 0, 65)
              bval <- rgamma(1, 3, 9)
              extra_val <- NA
              extra <- list(c=rep(extra_val, r))
              expect_identical(run(data.frame(a=aval, b=bval),
                                   function(a, b, env, ...) {data.frame(a=rep(a, r),
                                                                         b=rep(b, r), 
                                                                         c=env$c)},
                                   extra),
                               data.frame(a=rep(aval, r),
                                          b=rep(bval, r), 
                                          c=rep(extra_val, r)))
            }
})
test_that("random, two-column data.frames with slightly weird data", {
            reps <- ceiling(runif(min=0, max=1, n=10) * 100)
            for(r in reps) {
              aval <- rnorm(1, 0, 65)
              bval <- rgamma(1, 3, 9)
              extra_val <- sample(size=3, list(NA, NaN, list(a=5)))
              extra <- list(c=rep(extra_val, r))
              expect_identical(run(data.frame(a=aval, b=bval),
                                   function(a, b, env, ...) {data.frame(a=rep(a, r),
                                                                         b=rep(b, r), 
                                                                         c=env$c)},
                                   extra),
                               data.frame(a=rep(aval, r),
                                          b=rep(bval, r), 
                                          c=rep(extra_val, r)))
            }
})

test_that("random, two-column data.frames with extra data", {
            reps <- ceiling(runif(min=0, max=1, n=10) * 100)
            for(r in reps) {
              aval <- rnorm(1, 0, 65)
              bval <- rgamma(1, 3, 9)
              extra_val <- sample(size=3, list(NA, NaN, list(a=5)))
              extra <- list(c=rep(extra_val, r))
              expect_identical(run(data.frame(a=aval, b=bval),
                                   function(a, b, env, ...) {data.frame(a=rep(a, r),
                                                                         b=rep(b, r), 
                                                                         c=env$c)},
                                   extra),
                               data.frame(a=rep(aval, r),
                                          b=rep(bval, r), 
                                          c=rep(extra_val, r)))
            }
})

context("-- when manipulation depends on extra data")
test_that("random, two-column data.frames with functions depending on extra data", {
            reps <- ceiling(runif(min=0, max=1, n=10) * 100)
            for(r in reps) {
              aval <- rnorm(1, 0, 65)
              bval <- rgamma(1, 3, 9)
              extra_val <- sample(size=1, c(NA, NaN, 100000))
              extra <- list(c=extra_val)
              expect_identical(run(data.frame(a=aval, b=bval),
                                   function(a, b , env, ...) {data.frame(new=rep(a, r),
                                                                         old=rep(b, r))},
                                   environment()),
                               data.frame(new=rep(aval, r),
                                          old=rep(bval, r)))
            }
            expect_identical(run(data.frame(a=aval, b=bval),
                                 function(a, b , env, ...) {data.frame(new=max(a, env$c),
                                                                       old=min(b, env$c))},
                                 extra),
                             data.frame(new=max(aval, extra$c),
                                        old=min(bval, extra$c)))
})
test_that("random, two-column data.frames with functions depending on extra data", {
            avals <- 1:10
            bvals <- ceiling(runif(min=0, max=1, n=10) * 100)
            dat <- expand.grid(a=avals, b=bvals)
            extra <- list(c=rnorm(10))
            out <- run(dat,
                       function(a, b , env, ...) {
                         ## complex processing ;-)
                         data.frame(new=a, old=b, res=env$c)},
                         extra)

            ## make reference
            dat['obs'] <- 1:nrow(dat)
            dat_ref <- lapply(split(dat, dat['obs']), 
                              function(chunk) {
                                data.frame(new=chunk$a, 
                                           old=chunk$b,
                                           res=extra$c)
                              })
            dat_ref <- do.call(rbind, dat_ref)
            row.names(dat_ref) <- NULL
            expect_identical(out, dat_ref)
})
context("-- ordering of rows")
test_that("example output ordering", {
            REP <- 1
            growth <- function(n, r, K, b) {
            }
            data <- expand.grid(
                                b = seq(0.01, 0.5, length.out=10),
                                K = exp(seq(0.1, 5, length.out=10)),
                                r = seq(0.5, 3.5, length.out=10)
                                )
            initial_data = list(N0=0.9, T=1, reps=REP)
            growth_runner <- function(r, K, b, ic) {
              n0 = ic$N0
              T = ic$T
              reps = ic$reps
              out <- lapply(1:reps, function(i) {
                              return(1)
                                })
              data.frame(b = b, 
                         K = K, 
                         r = r, 
                         n_final = do.call(rbind, out))
            }
            output <- run(data, growth_runner, initial_data)

            expect_identical(output['r'], data['r'])
            expect_identical(output['K'], data['K'])
            expect_identical(output['b'], data['b'])
})
