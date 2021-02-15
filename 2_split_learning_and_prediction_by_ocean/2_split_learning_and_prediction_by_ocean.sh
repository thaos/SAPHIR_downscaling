#!/bin/sh
#PBS -l mem=16gb,vmem=32gb
#PBS -N split_learning_and_prediction_par_ocean

cd /homedata/vmichot/documents/SAPHIR-CALIPSO/trunk/Calibration
/usr/bin/Rscript  --vanilla 2_split_learning_and_prediction_par_ocean.R $1 $2 $3 > 2_split_learning_and_prediction_par_ocean$1_$2_$3.log 2>&1

# for aa in {North_Atlantic,Indian_Ocean,South_Atlantic,North_Pacific,South_Pacific};do for ay in {2012,2013,2014,2015,2016}; do for m in {1,2,3,4,5,6,7,8,9,10,11,12}; do qsub 2_split_learning_and_prediction_par_ocean.sh -q day -F"$aa $ay $m" ; done; done; done

# for aa in {North_Atlantic,Indian_Ocean};do for ay in {2012,2013}; do for m in {1,2}; do qsub split_learning_and_prediction_files.sh -q day -F"$aa $ay $m" ; done; done; done