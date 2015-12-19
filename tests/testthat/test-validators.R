context('parameter validation')

describe('numeric validation', {
  lapply(paste0('validate_', NUMERIC_VARS), function(fn) {
    test_that(paste0('It allows positive numeric numbers to go through ', fn), {
      value <- 4.2
      expect_equal(do.call(fn, list(value)), value)
    })
  })

  lapply(paste0('validate_', NUMERIC_VARS), function(fn) {
    test_that(paste0('It does not allow negative numbers to go through ', fn), {
      BAD_VALUES <- list(-42, 0, c(1, 2, 3), 'hello', function(x) {x}, TRUE, NA)
      lapply(BAD_VALUES, function(value) {
        expect_error(do.call(fn, list(value)), 'must be a positive numeric')
      })
    })
  })
})

describe('max iterations', {
  test_that('Max iterations is a positive integer', {
    expect_equal(validate_max_iterations(5), 5)
    expect_error(validate_max_iterations(0), 'must be a positive numeric')
    expect_error(validate_max_iterations(4.2), 'must be a positive integer')
  })
})

describe('num hidden', {
  test_that('Positive cases are validated', {
    GOOD <- list(1, c(1, 2, 3))
    lapply(GOOD, function(value) {
      expect_equal(validate_num_hidden(value), value)
    })
  })

  test_that('Negative cases throw an error', {
    GOOD <- list('hello', c(1.5, 2, 3), function(x) {x}, TRUE, 0, NA)
    lapply(GOOD, function(value) {
      expect_error(validate_num_hidden(value), 'must be an integer vector, specifying number of neurons in each hidden layer')
    })
  })
})

describe('rescaling', {
  test_that('Rescale defaults to FALSE but can be TRUE', {
    expect_equal(validate_rescale(TRUE), TRUE)
    lapply(c(1, NA, FALSE, c(1,2), function(x) {x}, 'hello'), function(value) {
      expect_equal(validate_rescale(value), FALSE)
    })
  })

  describe('rescaling offset must be a positive numeric', {
    BAD_VALUES <- list(-1, c(1, 2), FALSE, 'hello', function(x) {x})
    test_that('when rescale is false return value is NULL', {
      lapply(BAD_VALUES, function(value) {
        expect_null(validate_rescaling_offset(value, FALSE))
      })
      expect_null(validate_rescaling_offset(0.5, FALSE))
    })

    test_that('when rescale is false return value is NULL', {
      lapply(BAD_VALUES, function(value) {
        expect_error(validate_rescaling_offset(value, TRUE), 'must be a positive numeric')
      })
      expect_equal(validate_rescaling_offset(0.5, TRUE), 0.5)
    })
  })
})
