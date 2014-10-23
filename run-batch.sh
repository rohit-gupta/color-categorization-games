#!/bin/bash

for t in 3 4 5 6
do
	results_file="results/new/spectrum-McGill-smoothing-10-$t.csv"
	measurements_file="results/new/spectrum-McGill-smoothing-10-$t-measurements.csv"
	> $results_file
	> $measurements_file
    echo $t
	for n in {1..20}
	do
        echo "  $n"
    	python sim_max_signaling.py 'data/spectrum-6500K-5-20-20.csv' 8 $t 99999999999 $results_file $measurements_file
	done
done
