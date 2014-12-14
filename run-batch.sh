#!/bin/bash

for spectrum in 'spectrum-10-CIELAB-Daylight-6500K' 'spectrum-15-CIELAB-Daylight-6500K'
do
    for t in 3 4 5 6
    do
        results_file="results/$spectrum-$t.csv"
        measurements_file="results/$spectrum-$t-measurements.csv"
        > $results_file
        > $measurements_file
        echo $t
        for n in {1..20}
        do
            echo "  $n"
            python sim_max_signaling.py "data/$spectrum.csv" "data/$spectrum-priors-10.csv" $t 99999999999 $results_file $measurements_file
            exit 1
        done
    done
done