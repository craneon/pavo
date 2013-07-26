## Dichromatism Measurement: Scaled Volume Overlap

## Measures how much of the species' colorspace is shared by the male and female. Volume overlap can be used as a means of quantifying mimesis (Stoddard & Stevens 2011) or dichromatism, as it quantifies whether the different sexes occupy similar (overlapping) or different (non-overlapping) sensory niches.

## Uses pavo's exact solution (instead of Monte Carlo option).

## Volume overlap is scaled by male volume to give a measurement of shared color space relative to the male volume, which is useful for hantlint he rante of possible volumes for male color space (e.g. if volume overlap is small but so is male volume, then the species is likely monochromatic; similarly, if female volume is contained within male volume (100% overlap) but male volume is much larger than female volume, then the sexes are likely dichromatic.).

## MUST be in the "Specs-by-Species" working directory, produced by the species.spec() function. 

## SCRIPT FOR PATCH DICHROMATISM MEASURE

di.volume <- function(specs) {

  volume.comp <- data.frame()

	##Loop for volume overlap across all files in folder

	for(x in names(table(species))){
		filename <- paste(x, ".txt", sep="")
		curr.species <- read.table(filename, header = TRUE, row.names = 1)
		curr.species <- as.rspec(curr.species)
	
		## Make male and female data sets
	
		curr.M <- curr.species[c(1,grep('[0-9]M', names(curr.species)))]
		curr.F <- curr.species[c(1,grep('[0-9]F', names(curr.species)))]
		
		## Loop for each species to get volume overlap
	
		d <- data.frame()
		
		overlap <- voloverlap(tcs(vismodel(curr.M, visual="avg.v")), tcs(vismodel(curr.F, visual="avg.v")))
		d <- rbind(d, overlap)
		
		curr.overlap <- as.data.frame(d[,"overlapvol"] / d[,"vol1"])
		row.names(curr.overlap) <- x
		volume.comp <- rbind(volume.comp, curr.overlap)
		
	}

	## Print out the result
	
	setwd('..')
	write.table(volume.comp, "volume-overlap-scaled.txt", sep="\t")
	setwd("Specs_by_Species")
	
	## Return result in R Console (can suppress)
	
	volume.comp
}
