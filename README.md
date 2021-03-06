# Code repository for Quinn et al., 2022 (https://doi.org/10.1016/j.ecolind.2022.108831)
This code repository contains R and python scripts that process data and produce results from provided intermediate products for soundscape classification and ecological analyses. This work was funded under the Soundscapes 2 Landscapes, NASA’s Citizen Science for Earth Systems Program 16-CSESP 2016-0009 and is citable using the Zenodo DOI: <https://doi.org/10.5281/zenodo.6038459> and supports the published manuscript <https://doi.org/10.1016/j.ecolind.2022.108831> 
	
Note: Please be aware that underlying software, specifically for the CNN implementation, may not continue stability as python libraries are updated.
  
## This repository can: 
1) generate all code-based figures, tables, and numerical results in the published manuscript.
2) be used to re-train the ABGQI-CNN
3) be applied to novel acoustic data (5_ABGQI-CNN_deployment)

## Installation  
- Two Anaconda python environments are included in .yml files under envs. These provide information related to libraries used in spectrogram generation, CNN training, and inference. 
- R required libraries can be found in the envs R_requirements.txt file. The R_lib_install.R file can be run to install all packages in the requirements text file or you can manually download every package. Options to run the R_lib_installer:
	* RStudio: open R_lib_install.R, uncomment line 3 and specify path to envs/R_requirements.txt in Git repo.
	* command line: R_lib_installer.R <envs/R_requirements.txt>
	* Note, warnings are suppressed. If this method results in errors or libs are not installed use "install.packages("<package name>") to manually install.

	
## Rapidly deploy to novel recordings
Use 5_ABGQI-CNN_deployment scripts to quickly generate Mel spectrograms, generate CNN predictions, and classify these predictions based on S2L thresholds (note: these should be adjusted with dataset-specific testing data for ideal results - i.e., generalizability is limited).
- These scripts require changing the local directories to the cloned Git repo:
	* 0_melspec_generation_csv.py line 30
	* 1_inference_ABGQI.py line 34
	* 2_S2L_ABGQI_classification.R line 4
- In Git repo are examples from 3 S2L wav recordings. 
- If scripts are run outside of Git repo create the following directories:
	- data/wavs (where recordings are located)
	- data/melspecs
	- results/predctions
	- results/classifications (note: data/threshold/*.csvs are needed from Git repo)

## Manuscript data
#### List of scripts used to derive manuscript figures, tables, and results sections (when applicable):
- Fig 1: NA - derived using non-public GIS data in ArcGIS
- Fig 2: NA - conceptual figure, no code used
- Fig 3: NA - conceptual figure, no code used
- Fig 4: NA - conceptual figure, no code used
- Fig 5: 3_cnn_threshold_optimization_and_accuracy-R/code/0_CNN_threshold_optimization.R
- Fig 6: 4_ABGQI_environ_analyses-R/code/code/0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- Fig 7: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- Fig 8: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- Table 1: Not related to code, can be derived from Mel spectrogram training, validation, testing data set
- Table 2: 3_cnn_threshold_optimization_and_accuracy-R/code/1_CNN_accuracy_metrics.R
- Table 3: 4_ABGQI_environ_analyses-R/code/3_multivariate_regression.R

#### Results sections:
- 3.1 Model performance: most metrics in 3_cnn_threshold_optimization_and_accuracy-R/code/1_CNN_accuracy_metrics.R; cross validation numbers in data repository; rate of unidentified sound from 4_ABGQI_environ_analyses-R/code/0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2 Statistical anlayses of soundscape components: 4_ABGQI_environ_analyses-R/code/0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2.1 Diurnal LULC patterns: 4_ABGQI_environ_analyses-R/code/2_MannWhitney_tests-day_night.R  and 0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2.2 Annual and date of deployment differences: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.3: Daytime LULC stratification:  4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.4: Distance to roads: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.5: Effect of wind speed on soundscapes: not included as contains non-public GIS data
- 3.3 Factors affecting amount of soundcape components: 4_ABGQI_environ_analyses-R/code/3_multivariate_regression.R

If you have any questions or difficulty implementing the above methods contact author email: cq73@nau.edu
