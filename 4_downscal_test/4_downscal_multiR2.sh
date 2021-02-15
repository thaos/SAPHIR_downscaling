#!/bin/sh
#PBS -l mem=16gb,vmem=32gb
#PBS -N downscal_TYPE_0

cd /homedata/vmichot/documents/SAPHIR-CALIPSO/trunk/Calibration
/usr/bin/Rscript  --vanilla 4_downscal_multiR2.R $1 $2 $3 > 4_downscal_multiR2_$1_$2_$3.log 2>&1



# aa="Indian_Ocean"; ay=2013; m=7; qsub 4_downscal_TYPE_0_avecR2_4_downscal_multiR2.sh -q day -F"$aa $ay $m";
# for aa in {"North_Atlantic","Indian_Ocean"}; do ay=2013; m=7; qsub 4_downscal_multiR2.sh -q day -F"$aa $ay $m"; done