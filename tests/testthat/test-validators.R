context('parameter validation')

describe('numeric validation', {
  lapply(paste0('validate_', NUMERIC_VARS), function(fn) {
    test_that('It allows positive numeric numbers to go through', {
      value <- 4.2
      expect_equal(do.call(fn, list(value)), value)
    })
  })
})
