# HFC_eigenvalsProjector

This program reads hyperfine coupling (HFC) tensors computed on multiple flavin adenine dinucleotide (FAD) molecules structures using the quantum chemistry software deMon2k. Tensors are then projected onto an average eigenbasis for each magnetically active atom. The distributions values taken by HFC tensors across all geometries, projected onto the average eigenbasis for each atom, are finally plotted using the "groupsof3_graphs" R script.

We investigate the flavin in 2 protonation states: the radical anion (FAD.-) and the radical neutral semiquinone (FADH.). For each oxidation states, multiple flavin geometries were generated. These are extracted at regular intervals from a Molecular Dynamics (MD) simulation of the flavin inside a solvated pigeon cryptochrome protein (clCry4). HFC tensors were subsequently computed on the aforementioned flavin geometries. To gauge the effect of electronic polarisation by the protein environment of flavin-borne HFC tensors, the single-point calculation of HFC tensors is done with ("in_cluster") and without ("from_cluster") explicit inclusion of a "cluster" of aminoacid residues. 

Method:
 * Read in whole tensors, diagonalise them, save the eigenvectors.
 * Check eigenvectors are reasonably consistent. Maybe use clustering to identify 3 (internal and not necessarily ortho) basis vectors.
 * Geom sum and renormalise these 3 internal axes to obtain the (internal) x,y,z average principal axes.
 * Project the whole tensor onto each of these axes.
 * Plot histograms of the projections on axis 1, 2, and 3.
