#!/usr/bin/Rscript

library(stringr)
library(ggplot2)

require(gridExtra)

prep_df <- function(b,c,e) {
# -- b is filename
# -- c is the column number, e.g 1 for V1
# -- d is the final dataframe name, e.g dV1
# -- e is a string and the tag name, e.g 'V1 eigenvector, in-cluster'
	a <- read.table(b)[,c]
	d <- data.frame(matrix(unlist(a), nrow=length(a), byrow=TRUE))
	d$set = e
	colnames(d) <- c('A','set')
	return(d)
}

set_prefix <- function(dirname) {
	if ( grepl('FAD.-', dirname, fixed = TRUE) ) {
		return('F_')
	} else if ( grepl('FADH.', dirname, fixed = TRUE) ) {
		return('FH_')
	}
}


filelist = scan("inlist.in", what="", sep = "\n")

for (filename_i in filelist) {

	dirname    <- str_match(filename_i, "dir/\\s*(.*?)\\s*/")[2]
	atomname   <- str_match(filename_i,"cluster/\\s*(.*?)\\s*_projected.dat")[2]
	filename_f <- gsub('1-in','2-from',filename_i)
	prefix     <- set_prefix(dirname)

	pdf(file = paste0('more_histos/',prefix,atomname,'_3plots.pdf'))

	dVi     <- prep_df(filename_i, 1, 'V1 eigenvector, in-cluster')
	dVf     <- prep_df(filename_f, 1, 'V1 eigenvector, from-cluster')
	distrib <- rbind(dVi,dVf)
	plot_V1 <- ggplot(distrib, aes(A, fill = set)) + guides(fill = FALSE) +
				geom_density(alpha = .2, adjust = 2) + 
				theme(axis.ticks.y = element_blank(), 
              axis.text.y = element_blank(), 
              text = element_text(size = 15)) +
				xlab(expression(paste(A^(V[1]),' (MHz)'))) +
				ylab('Probability density (a.u)')
#	print(plot_V1)

	dVi     <- prep_df(filename_i, 2, 'V2 eigenvector, in-cluster')
	dVf     <- prep_df(filename_f, 2, 'V2 eigenvector, from-cluster')
	distrib <- rbind(dVi,dVf)
	plot_V2 <- ggplot(distrib, aes(A, fill = set)) + guides(fill = FALSE) +
				geom_density(alpha = .2, adjust = 2) + 
				theme(axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(), 
              axis.text.y = element_blank(),
              text = element_text(size = 15)) +
				xlab(expression(paste(A^(V[2]),' (MHz)')))
#	print(plot_V2)

	dVi     <- prep_df(filename_i, 3, 'V3 eigenvector, in-cluster')
	dVf     <- prep_df(filename_f, 3, 'V3 eigenvector, from-cluster')
	distrib <- rbind(dVi,dVf)
	plot_V3 <- ggplot(distrib, aes(A, fill = set)) + guides(fill = FALSE) +
				geom_density(alpha = .2, adjust = 2) + 
				theme(axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(), 
              axis.text.y = element_blank(),
              text = element_text(size = 15)) +
				xlab(expression(paste(A^(V[3]),' (MHz)')))
#	print(plot_V3)
	grid.arrange(plot_V1, plot_V2, plot_V3, ncol=3)

	dev.off()

}

q()
