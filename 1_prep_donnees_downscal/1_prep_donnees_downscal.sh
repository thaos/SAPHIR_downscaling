#!/bin/sh
#PBS -l mem=16gb,vmem=32gb
#PBS -N prep_donnees_downscal

cd /homedata/vmichot/documents/SAPHIR-CALIPSO/trunk/Calibration
/usr/bin/Rscript  --vanilla 1_prep_donnees_downscal.R $1 $2 $3 > 1_prep_donnees_downscal$1_$2_$3.log 2>&1

# for aa in {North_Atlantic,Indian_Ocean,South_Atlantic,North_Pacific,South_Pacific};do for ay in {2012,2013,2014,2015,2016}; do for m in {1,2,3,4,5,6,7,8,9,10,11,12}; do qsub 1_prep_donnees_downscal.sh -q day -F"$aa $ay $m" ; done; done; done
# aa=North_Pacific; ay=2013; m=7; qsub 1_prep_donnees_downscal.sh -q day -F"$aa $ay $m" ;