#!/bin/bash

REFERENCE_FILE=Christiansen_Woodcroft_combined_16S.fasta
QUERY_FILE=rep_set.fasta

#Thanks for the walkthrough! https://www.polarmicrobes.org/phylogenetic-placement-re-re-visited/

## Degap
seqmagick mogrify --ungap ${REFERENCE_FILE}
## Align
cmalign --dna -o ref.sto --outformat Pfam 16S_bacteria.cm ${REFERENCE_FILE}
## Convert to fasta format
seqmagick convert ref.sto ${REFERENCE_FILE}
seqmagick mogrify --deduplicate-sequences ${REFERENCE_FILE}
raxmlHPC-SSE3 -T 8 -m GTRGAMMA -s ${REFERENCE_FILE} -n ref.tre -f d -p 12345
raxmlHPC-SSE3 -T 2 -m GTRGAMMA -f I -t RAxML_bestTree.ref.tre -n root.ref.tre
raxmlHPC-SSE3 -T 8 -m GTRGAMMA -f J -p 12345 -t RAxML_rootedTree.root.ref.tre -n conf.root.ref.tre -s ${REFERENCE_FILE}
taxit create -l 16S_rRNA -P ref.refpkg --aln-fasta ${REFERENCE_FILE} --tree-stats RAxML_info.ref.tre --tree-file RAxML_fastTreeSH_Support.conf.root.ref.tre

seqmagick mogrify --ungap ${QUERY_FILE}
cmalign --dna -o query.sto --outformat Pfam 16S_bacteria.cm ${QUERY_FILE}
seqmagick convert query.ref.sto ${QUERY_FILE}
pplacer -o query.ref.jplace -p --keep-at-most 20 -c ref.refpkg ${QUERY_FILE}
guppy to_csv --point-mass --pp -o query.ref.csv query.ref.jplace
guppy tog --node-numbers --xml --pp -o query.ref.tog.phyloxml query.ref.jplace
