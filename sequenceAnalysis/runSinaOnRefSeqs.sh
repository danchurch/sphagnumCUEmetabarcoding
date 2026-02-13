#!/usr/bin/env bash
arbSilvaDB=/home/daniel/Documents/databases/silva/silva.arb
outFolder=/home/daniel/Documents/projects/wernerSphagnum/sphagnumCUEmetabarcoding/sequenceAnalysis/findANME/
sina  -i ps007_cleaned_sqrt_prop_RefSeqs.fa -r $arbSilvaDB -o $outFolder/refSeqsInSilva

