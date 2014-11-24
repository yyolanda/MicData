## For gMicData()
test_that("case is ignored", {
	expect_identical(gMicData("TLM49"), gMicData("tlm49"))
})

test_that("always returns data.frame", {
	expect_is(gMicData("u89"), "data.frame")
})

test_that("number of rows equal", {
	expect_equal(nrow(gMicData("tlm49")), 19)
})

test_that("whitespace is ignored", {
	expect_equal(gMicData("tlm49"), gMicData("tlm 49"))
})

test_that("Invalid input", {
	expect_error(gMicData(49))
})

## For gDescription()
test_that("case is ignored", {
	expect_identical(gDescription("TLM103"), gDescription("tlm103"))
})

test_that("always returns character vector", {
	expect_is(gDescription("u89"), "character")
})

test_that("whitespace is ignored", {
	expect_equal(gDescription("tlm49"), gDescription("tlm 49"))
})

test_that("Invalid input", {
	expect_error(gDescription(49))
})

## For gModel()
test_that("always returns character vector", {
	expect_is(gModel(), "character")
})

test_that("output length is correct",{
	expect_equal(length(gModel()), 50)
})

test_that("last item matches",{
	expect_equal(gModel()[length(gModel())], "usm69")
})
