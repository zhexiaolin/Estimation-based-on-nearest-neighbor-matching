#!/bin/bash
#SBATCH --cpus-per-task 10
#SBATCH -p high
#SBATCH --job-name=zxlin
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=zhexiaolin@berkeley.edu
pip install -r requirements.txt
Rscript --save check_dependency.R
Rscript --save shadish.R
python3 gan_estimation/gan_shadish_baseline.py
Rscript --save comparison.R
Rscript --save comparison_shadish.R
Rscript --save se.R
Rscript --save se_shadish.R
Rscript --save produce_tables.R
Rscript --save produce_tables_shadish.R