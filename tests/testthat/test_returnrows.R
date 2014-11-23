library(UFO)

test_that('return.rows returns specified number', {
	expect_equivalent(nrow(latest.sightings()), 5)
	expect_equivalent(nrow(latest.sightings(return.rows = 5)), 5)
	expect_equivalent(nrow(latest.sightings(area = 'WA', return.rows = 10)), 10) 
	expect_equivalent(nrow(latest.sightings(area = 'NY', return.rows = 500)), 500)
	
	expect_error(latest.sightings(return.rows = A))
	expect_error(latest.sightings(return.rows = 'plot'))
	
})