test_that("upgrade", {
  expect_true(parse_arguments("install", "-u", "cli")$upgrade)
})
