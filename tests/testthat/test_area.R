library(UFO)

test_that('case is ignored', {
	expect_equal(latest.sightings(area = 'bc'), latest.sightings(area = 'BC'))
	expect_equal(latest.sightings(area = 'Bc'), latest.sightings(area = 'bC'))
	expect_equal(sightings.by.shape(area = 'bc'), sightings.by.shape(area = 'BC'))
	expect_equal(sightings.by.shape(area = 'Bc'), sightings.by.shape(area = 'bC'))
	expect_equal(latest.sightings(area = 'wa'), latest.sightings(area = 'WA'))
	expect_equal(latest.sightings(area = 'Wa'), latest.sightings(area = 'wA'))
	expect_equal(sightings.by.shape(area = 'wa'), sightings.by.shape(area = 'WA'))
	expect_equal(sightings.by.shape(area = 'Wa'), sightings.by.shape(area = 'wA'))
})

test_that('only two letters accepted', {
	expect_error(latest.sightings(area = 'a'))
	expect_error(sightings.by.shape(area = 'a'))
	expect_error(latest.sightings(area = 'aaa'))
	expect_error(sightings.by.shape(area = 'aaa'))
	expect_error(latest.sightings(area = 'ABC'))
	expect_error(sightings.by.shape(area = 'ABC'))	
})

