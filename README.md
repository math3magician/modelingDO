# modelingDO

This project focuses on modeling Dansgaard-Oeschger (DO) events, which are rapid temperature changes observed during the last glacial period. The goal is to understand these events' underlying dynamics and driving mechanisms. Data used in this project is from [1]

## File Descriptions

The project includes the following source scripts:

*functions_torch.R:* This script contains all necessary functions needed for maximum likelihood estimation (MLE) using automatic differentiation (AD). This is done using the R Torch package.
*functions_base.R:* This script includes some of the same functions from the previous script, but written in base R code.
*estimator.R:* This script implements the main estimator using R Torch.
*residuals.R:* This script calculates and analyzes the residuals of the DO event model to assess model performance.
*data_loading.R:* This script handles the loading of the DO event data.
*data_preprocessing.R:* This script includes functions for preprocessing the DO event data before modeling.
Please refer to the individual scripts for detailed comments and explanations of the code.

## Getting Started

To use the scripts in this project for modeling DO events, follow these steps:

1. Clone the repository to your local machine.
2. Make sure you have the required dependencies installed, including R, the Torch library, and any additional packages specified in the scripts.
3. Prepare your DO event data by following the instructions in the data_loading.R and data_preprocessing.R scripts.
4. Adjust the parameters and settings in the estimator.R script as needed for your specific modeling requirements.
5. Execute the scripts in the following order to model DO events:

  data_loading.R
  
  data_preprocessing.R

  functions_torch.R
  
  functions_base.R
  
  estimator.R
  
  residuals.R
  
6. Analyze the results and assess the performance of the DO event model using the generated residuals.

## References

[1] Rasmussen, S. O., et al. (2014). A stratigraphic framework for abrupt climatic changes during the last glacial period based on three synchronized Greenland ice-core records: refining and extending the intimate event stratigraphy. Quaternary Science Reviews, 106, 14-28.

This project is part of the course Data Science in International Economics Research (DSIER23) offered by the University of Bielefeld

