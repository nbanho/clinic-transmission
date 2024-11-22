## Spatiotemporal modeling of indoor tuberculosis transmission

This repository contains code and results for the paper "Estimating Mycobacterium tuberculosis transmission in a South African clinic: Spatiotemporal model based on person movements". 

### analysis

R Markdown files generating tables and figures of descriptive and simulation results:

1. Clinical data (clinical-data.Rmd)
2. Environmental data (environmental-data.Rmd)
3. Patient-tracking data (patient-tracking.Rmd)
4. Simulation results (simulation-results.Rmd)

### data

Restrictions on the availability of personal data apply, but are necessary to maintain the confiden-
tiality of participants. The data is available upon reasonable request, contact: University of Bern,
info.ispm@unibe.ch. All other data are included in the main text of the paper and accompanying supporting
information.

### doc

Paper LaTeX files and schematic illustrations of the clinic and model flow.

### illustrations

R Markdown file to generate prior distributions, and a small R file to illustrate the idea of the spatiotemporal modeling approach in a simple plot.

### models

1. **stm-v2.R**: The spatiotemporal model as a documented R function
2. tm-v1.R: A temporal only version of the model.

### preprocessing

Multiple files to preprocess all data:

1. Several files to process clinical, environmental and patient tracking data.
2. Function to match clinical and tracking data.
3. Function to compute close contact number and time.
4. Shiny app to link interrupted tracks.

### results

1. data: figures of clinical, environmental, and patient-tracking data.
2. inputs: figures of prior distributions
3. modeling: figures of simulation results

File **modeling/STM - Supplementary Video.gif** is an animated plot showing the quanta concentration over time over a short time window as an infectious person enters the clinic's waiting room. 
It may be helpful to understand how we use our data and what the spatiotemporal model does.

### simulations 

Repository to store input data, parameters, and scenario-specific simulation results. 
Most important is the R file **simulations/simulate_risk.R** which creates the setup and can be run in parallel locally or on a computing cluster. 

### tests

Toy example to test and illustrate the spatiotemporal modelling of quanta concentration in a small rectangular room.

### utils

1. plotting.R and tex.R: Generic helper functions for plotting and formatting.
2. spatial.r and distr.r: Helper functions for spatial data and probability distributions.
3. **trans_risk.R**: Documented functions incorporating prior information from the literature regarding important modelling parameters.





