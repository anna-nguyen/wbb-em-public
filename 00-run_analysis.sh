#!/bin/bash

#SBATCH --job-name=run_diarrhea_analysis
#SBATCH --begin=now
#SBATCH --dependency=singleton
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --mem=64G
#SBATCH --output=01-wbb-em.out
#SBATCH --time=2-00:00:00

#module purge

#module load R/4.0.2

cd $HOME/Documents/Research/WBB_Climate_EM/wbb-em

#############################################
# 1-data-processing
#############################################
R CMD BATCH --no-save 1-data-processing/1-clean_diarr_data.R 1-data-processing/1-clean_diarr_data.out

R CMD BATCH --no-save 1-data-processing/2a-extract_daily_temp_data.R 1-data-processing/2a-extract_daily_temp_data.out
R CMD BATCH --no-save 1-data-processing/2b-calculate_temp_cutoffs.R 1-data-processing/2b-calculate_temp_cutoffs.out
R CMD BATCH --no-save 1-data-processing/2c-construct_temp_variables.R 1-data-processing/2c-construct_temp_variables.out

python 1-data-processing/3a-subset_raw_ppt_data.py >> 1-data-processing/3a-subset_raw_ppt_data.txt
R CMD BATCH --no-save 1-data-processing/3b-extract_daily_ppt_data..R 1-data-processing/3b-extract_daily_ppt_data..out
R CMD BATCH --no-save 1-data-processing/3c-calculate_ppt_cutoffs.R 1-data-processing/3c-calculate_ppt_cutoffs.out
R CMD BATCH --no-save 1-data-processing/3d-construct_ppt_variables.R 1-data-processing/3d-construct_ppt_variables.out

python 1-data-processing/4a-calculate_ppt_daily_averages.py >> 1-data-processing/4a-calculate_ppt_daily_averages.txt
R CMD BATCH --no-save 1-data-processing/4b-define_rainy_season.R 1-data-processing/4b-define_rainy_season.out

R CMD BATCH --no-save 1-data-processing/5-merge_wbb_data.R 1-data-processing/5-merge_wbb_data.out

#############################################
# 2-fit-models
#############################################
R CMD BATCH --no-save 2-fit-models/1-tx_effect_mod_diarrhea.R 2-fit-models/1-tx_effect_mod_diarrhea.out
R CMD BATCH --no-save 2-fit-models/2-tx_effect_mod_diarrhea_negcontrol.R 2-fit-models/2-tx_effect_mod_diarrhea_negcontrol.out

#############################################
# 3-analysis
#############################################
R CMD BATCH --no-save 3-analysis/1-generate_categorical_results.R 3-analysis/1-generate_categorical_results.out
R CMD BATCH --no-save 3-analysis/2-generate_continuous_results.R 3-analysis/2-generate_continuous_results.out
R CMD BATCH --no-save 3-analysis/3-distance_from_station.R 3-analysis/3-distance_from_station.out
R CMD BATCH --no-save 3-analysis/4-generate_results_manuscript_text.R 3-analysis/4-generate_results_manuscript_text.out

#############################################
# 4-fig-tab
#############################################
R CMD BATCH --no-save 4-fig-tab/1a-generate_table1.R 4-fig-tab/1-generate_table1.out
R CMD BATCH --no-save 4-fig-tab/2-Figure1_plots_over_time.R 4-fig-tab/2-Figure1_plots_over_time.out
R CMD BATCH --no-save 4-fig-tab/3-Figure2_plot_continuous_ppt_results.R 4-fig-tab/3-Figure2_plot_continuous_ppt_results.out
R CMD BATCH --no-save 4-fig-tab/4-Figure3_plot_categorical_ppt_results.R 4-fig-tab/4-Figure3_plot_categorical_ppt_results.out
R CMD BATCH --no-save 4-fig-tab/5-Figure4_plot_temperature_results.R 4-fig-tab/5-Figure4_plot_temperature_results.out

#############################################
# 4-fig-tab/Appendix
#############################################
R CMD BATCH --no-save 4-fig-tab/Appendix/1-TableS1_extended_table1.R 4-fig-tab/Appendix/1-TableS1_extended_table1.out
R CMD BATCH --no-save 4-fig-tab/Appendix/2-TableS2S3_all_tmle_results.R 4-fig-tab/Appendix/2-TableS2S3_all_tmle_results.out
R CMD BATCH --no-save 4-fig-tab/Appendix/3-FigureS1_plot_temp_ppt.R 4-fig-tab/Appendix/3-FigureS1_plot_temp_ppt.out
R CMD BATCH --no-save 4-fig-tab/Appendix/4-FigureS2_plot_negcontrol_results.R 4-fig-tab/Appendix/4-FigureS2_plot_negcontrol_results.out
R CMD BATCH --no-save 4-fig-tab/Appendix/5-FigureS3_plot_ppt_all_lags 4-fig-tab/Appendix/5-FigureS3_plot_ppt_all_lags.out
R CMD BATCH --no-save 4-fig-tab/Appendix/6-FigureS4_plot_temp_all_lags.R 4-fig-tab/Appendix/6-FigureS4_plot_temp_all_lags.out
R CMD BATCH --no-save 4-fig-tab/Appendix/7-FigureS5_plot_80v90_heavy_rain_cutoffs.R 4-fig-tab/Appendix/7-FigureS5_plot_80v90_heavy_rain_cutoffs.out
