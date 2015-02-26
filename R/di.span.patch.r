## Dichromatism Measurement: Patch-by-patch volume centroid distance (span)

## Measures distance between the centers of the convex hulls created by the set of spectra for each male and female patch.

## Adds together the patch-by-patch distances for total color distance and divides by number of patches. 

## If you are using additional patches beyond a standard set, make sure your results are correlated strongly with those procured with only your standard set (e.g. if you have 9 patches for some birds and a standard set of 5, you want to make sure that the additional 4 patches are not skewing the final measurement after averaging). 

## MUST be in the "Specs-by-Species" working directory, produced by the species.spec() function. 

## SCRIPT FOR PATCH DICHROMATISM MEASURE

di.span.patch <- function(specs, avg = TRUE) {
	
	species <- gsub('([A-Z]+)([0-9]+)([A-Za-z]+)([0-9])$', '', names(specs))[-1]
	
	patch.span <- data.frame()
	
	## (Eliminate problem species that don't have M and F, e.g. Cnmc))
	## species <- species[-grep("Cnmc", species)]
	## specs <- specs[-grep("Cnmc", names(specs))]
	
	## Loop for centroid distances across all files in folder
	
	for(x in names(table(species))){
		filename <- paste(x, ".txt", sep="")
		curr.species <- read.table(filename, header = TRUE, row.names = 1)
		curr.species <- as.rspec(curr.species)
	
	## Make male and female data sets
	
		curr.M <- curr.species[c(1,grep('[0-9]M', names(curr.species)))]
		patches.M <- gsub('([A-Za-z]+)([0-9]+)[A-Z]', '', names(curr.M))[-1]
		patches.M <- gsub('[0-9]$', '', patches.M)
	
		curr.F <- curr.species[c(1,grep('[0-9]F', names(curr.species)))]
		patches.F <- gsub('([A-Za-z]+)([0-9]+)[A-Z]', '', names(curr.F))[-1]
		patches.F <- gsub('[0-9]$', '', patches.F)
	
	## Check that patches are the same
	
		if (sum(names(table(patches.M)) != names(table(patches.F)))) {
			warning(paste("Error at ", x))
			next
		}
	
	## Loop for each species to get sum of centroid distances between patches
	
		d <- data.frame()
		for(ii in names(table(patches.M))){
			curr.patch <- curr.species[c(1, grep(ii, names(curr.species)))]
			overlap <- voloverlap(tcs(vismodel(curr.patch[c(1,grep('[0-9]M', names(curr.patch)))], visual="avg.v")), tcs(vismodel(curr.patch[c(1,grep('[0-9]F', names(curr.patch)))], visual="avg.v")))
			row.names(overlap) <- ii
			d <- rbind(d, overlap)
		}
	
		if (avg == TRUE) {
		curr.span <- as.data.frame(sum(d[,"cenDistance"]) / nrow(table(patches.M))) ##should we divide here?
		}
		
		if (avg == FALSE) {
		curr.span <- as.data.frame(sum(d[,"cenDistance"])) ##left in for comparison
		}
		
		row.names(curr.span) <- x
		patch.span <- rbind(patch.span, curr.span)
	}
	
	## If the loop produces errors and stops, type x to see the name of the species at which the error occurred. 
	## The most likely cause of error is insufficient data: the centroid distance calculation requires at least 4 measurements per patch per sex (e.g. four female throats), which can come from multiple individuals. 
	## If you only have one individual of a species, measure at least 5 spectra per patch.
	
	
	## Print out the result
	
	setwd('..')
	write.table(patch.span, "patch-centroid-span.txt", sep="\t")
	setwd("Specs_by_Species")
	
	## Return result in R Console (can suppress)
	
	patch.span
}