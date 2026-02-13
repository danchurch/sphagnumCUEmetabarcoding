#!/usr/bin/env bash

outDir="/home/daniel/Documents/projects/wernerSphagnum/sphagnumCUEmetabarcoding/sequenceAnalysis/findANME/"
alignment="/home/daniel/Documents/projects/wernerSphagnum/sphagnumCUEmetabarcoding/sequenceAnalysis/findANME/refSeqsInSilva.afa"

FastTree -gtr -nt < ${alignment} > ${alignment/afa/nwk}


