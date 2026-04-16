# amlr-gls

The repo contains simple functions and raw data to support analysis of US AMLR data sets from the deployment of light-based geolocators (GLS) on seabird and pinnipeds during the austral winters of 2011, 2012, and 2014. These data are permanently archived at [NCEI at a DOI TBD].

The `plot_raw_data` and `plot_kde_fig` functions create basic data displays from available data. Their output is illustrated in the `plots` folder.

Old functions is the `old scripts` folder, including `process_TFgls`, `bias_estimation` and their associated helper functions, were used to process and estimate usable location estimates from raw tag downloads. These old functions are 1) neither intended nor required for use with the current data set, 2) have not been tested fur full functionality, and 3) are provided as reference material only. They may have dependency on deprecated R packages.

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
