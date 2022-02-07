# Code repository for Quinn et al., 2022 <doi> 
This repository can: 
  1) generate all code-based figures, tables, and numerical results in the published manuscript.
  2) be used to re-train the ABGQI-CNN
  3) be applied to novel acoustic data by generating 2-s Mel spectrograms, ABGQI-CNN inference probabilities, f-score threshold optimization, and classification based on CNN findings related to Soundscapes 2 Landscapes data


### List of scripts used to derive manuscript figures, tables, and results sections (when applicable):
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

### Results sections:
- 3.1 Model performance: most metrics in 3_cnn_threshold_optimization_and_accuracy-R/code/1_CNN_accuracy_metrics.R; cross validation numbers in data repository; rate of unidentified sound from 4_ABGQI_environ_analyses-R/code/0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2 Statistical anlayses of soundscape components: 4_ABGQI_environ_analyses-R/code/0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2.1 Diurnal LULC patterns: 4_ABGQI_environ_analyses-R/code/2_KWDunn_tests-day_night.R  and 0_ABGQIU_temporal_plotting_faceted_LCLU_figure6.R
- 3.2.2 Annual and date of deployment differences: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.3: Daytime LULC stratification:  4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.4: Distance to roads: 4_ABGQI_environ_analyses-R/code/1_KWDunn_tests-LULC_Roads_Annual.R
- 3.2.5: Effect of wind speed on soundscapes: not included as contains non-public GIS data
- 3.3 Factors affecting amount of soundcape components: 4_ABGQI_environ_analyses-R/code/3_multivariate_regression.R
