## Dichromatism Measurement: Brilliance Comparison

## Measures the difference in overall brilliance between male and female birds of a species. When log-avg option is FALSE (default), the measurement compares the average of the sum of relative reflectance over the entire spectral range (the integral under the curve) for all reflectance curves in the male or female individuals. When log-avg option is TRUE (must set), the function follows Maia et al. 2013 to compare the log-scaled average reflectance value across all spectra.

## MUST be in the "Specs-by-Species" working directory, produced by the species.spec() function. 

## SCRIPT FOR BRILLIANCE COMPARISON MEASURE

di.brilliance <- function(specs, log-avg = FALSE) {
  
	##Set up data structures
	
	r = nrow(table(species))
	q = 1
	brightness.M <- matrix(nrow=r)
	brightness.F <- matrix(nrow=r)
	
	##Loop for brightness integral across all files in folder
	
	for(x in names(table(species))){
		filename <- paste(x, ".txt", sep="")
		curr.species <- read.table(filename, header = TRUE, row.names = 1)
		curr.species <- as.rspec(curr.species)
	
		## Make male and female data sets
	
		curr.M <- curr.species[c(1,grep('[0-9]M', names(curr.species)))]
		curr.F <- curr.species[c(1,grep('[0-9]F', names(curr.species)))]
	
		if (log-avg == FALSE) {
		
			## Get mean brightness integral over all spectra by adding together each individual spectral integral and dividing by the numer of spectra
			
			## Compile male data
			avg.integral.M <- sum(summary(curr.M)$B1)/nrow(summary(curr.M))
		    	brightness.M[q,]=c(avg.integral.M)
		
			## Complie female data
			avg.integral.F <- sum(summary(curr.F)$B1)/nrow(summary(curr.F))
			brightness.F[q,] <- c(avg.integral.F)
			
		}
		
		if (log-avg == TRUE) {
			
			## Get log-transformed average reflectance value of spectral curves, as in Maia et al. 2013. Uses $B2, the mean brightness (Mean relative reflectance over the entire spectral range) of the spectral range, instead of $B1, the total brightness (sum of relative reflectance over the entire spectral range, area under the curve).
			
			mean.M <- mean(summary(curr.M)$B2)
			mean.M <- log(mean.M)  ## natural logarithm of male average brightness
			brightness.M[q,]=c(mean.M)
			
			mean.F <- mean(summary(curr.F)$B2)
			mean.F <- log(mean.F)
			brightness.F[q,] <- c(mean.F)
				
		}
	
		## Step up the counter
			q = q+1
	
	}


	## Subtract female from male brightness

	mf.difference <- brightness.M - brightness.F

	## New data frame for all values

	brightness <- data.frame(brightness.M, brightness.F, mf.difference)
	row.names(brightness) <-  names(table(species))

	## Print result

	setwd('..')
	write.table(brightness, "brightness_integral.txt", sep="\t")
	setwd("Specs_by_Species")
	
	## Return result in R Console (can suppress)
	
	brightness

}
