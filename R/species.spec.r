## Species.spec organizes data. After using getspec, use species.spec to divide your specs up by species and place everything as tab-delimited text documents in one folder. Necessary precursor for the dimorphism measurement scripts.

species.spec <- function(specs) {

  ## Create folder for the spec files
	
	dir.create('Specs_by_Species')
	setwd("Specs_by_Species")
	
	
	##Get species vector
	
	species <- gsub('([A-Z]+)([0-9]+)([A-Za-z]+)([0-9])$', '', names(specs))[-1]
	
	## Create separate files for each species
	
	for(x in names(table(species))){
		curr <- specs[c(1,grep(x, names(specs)))]
		filename <- paste(x, ".txt", sep="")
		write.table(curr, filename, col.names=TRUE, row.names=TRUE, sep="\t")
	}
  
}
