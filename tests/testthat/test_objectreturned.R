library(UFO)

test_that('print returns correct object', {
	expect_is(sightings.by.shape(), 'ggplot')
	expect_is(sightings.by.shape(area = 'TX', print = 'plot'), 'ggplot')
	expect_is(sightings.by.shape(print = 'table'), 'data.frame')
	
	expect_is(sightings.by.shape(print = 'ufo'), 'character')	
	expect_is(sightings.by.shape(print = 'shape'), 'character')	
	
})
