#!/bin/sh
#PBS -l mem=32gb,vmem=32gb
#PBS -N MODEL_Indian_Ocean_ALLclouds

cd /bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/3_downscal_MODEL
/usr/bin/Rscript  --vanilla 3_downscal_MODEL_couche_SANS_Iteration.R $1 $2 $3 $4> 3_downscal_MODEL_couche_SANS_Iteration_$1_$2_$3_$4.log 2>&1


# aa=Indian_Ocean; ay=2013; m=7; for l in {1,6}; do qsub 3_downscal_MODEL_couche.sh -q week -F"$aa $ay $m $l"; done
# aa=Indian_Ocean; ay=2013; m=7;l=1;qsub 3_downscal_MODEL_couche.sh -q infini -F"$aa $ay $m $l"
# aa=Indian_Ocean; ay=2013; m=7; for l in {1,2,3,4,5,6}; do qsub 3_downscal_MODEL_couche.sh -q infini -F"$aa $ay $m $l"; done
# for aa in {"Indian_Ocean","North_Pacific","South_Pacific","South_Atlantic","North_Atlantic", "South_Atlantic"}; do ay=2013; m=7; for l in {1,2,3,4,5,6}; do qsub 3_downscal_MODEL_couche.sh -q infini -F"$aa $ay $m $l"; done; done

