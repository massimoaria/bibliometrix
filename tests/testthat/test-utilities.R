# Test per funzioni utility: trim, trim.leading, trimES

test_that("trim rimuove spazi iniziali e finali", {
  expect_equal(trim("  hello  "), "hello")
  expect_equal(trim("hello"), "hello")
  expect_equal(trim("  "), "")
  expect_equal(trim(c("  a ", " b")), c("a", "b"))
})

test_that("trim.leading rimuove solo spazi iniziali", {
  expect_equal(trim.leading("  hello"), "hello")
  expect_equal(trim.leading("  hello  "), "hello  ")
  expect_equal(trim.leading("hello"), "hello")
  expect_equal(trim.leading(c("  a", " b")), c("a", "b"))
})

test_that("trimES rimuove spazi extra multipli", {
  expect_equal(trimES("hello  world"), "hello world")
  expect_equal(trimES("a   b   c"), "a b c")
  expect_equal(trimES("hello world"), "hello world")
  expect_equal(trimES(c("a  b", "c   d")), c("a b", "c d"))
})
