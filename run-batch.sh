#!/bin/bash

for t in 3 4 5 6
do
	> results/new-simulations-$t.csv
    echo $t
	for n in {1..10}
	do
        echo "  $n"
    	python sim_max_signaling.py 'data/spectrum-6500K-5-20-20.csv' $t 99999999999 results/new-simulations-$t.csv
	done
done
