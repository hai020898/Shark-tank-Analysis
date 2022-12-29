# Shark-tank-Analysis
Final Project for MGSC 661 as part of MMA program at McGill University

For the final project I have chosen the Shark Tank Dataset from Kaggle.com to predict the outcome of a business pitch on the TV show Shark Tank. A complete description of the columns can be found [here](https://www.kaggle.com/datasets/rahulsathyajit/shark-tank-pitches)

First, exploratory data analysis was conducted to understand the distribution of variables and missing values. I then performed data cleaning to remove irrelevant values and combine several columns. Afterwards, I conducted feature engineering using Random Forest to select predictors that are of higher importance. Subsequently, I implemented a boosted model with parameters tuning using the cleaned data to predict the likely outcome of a pitch on the show. 

Another recommendation using the dataset is to run a Linear Discriminant Analysis model to classify the region that an entrepreneur is likely to come from, given different criteria in order to facilitate the casting process.

More detail on the methodology could be found in **final_report.pdf**

Below are a list of the files in the repository:

  * **model.R**: Contains the code to perform data cleaning, feature engineering, implementing the model and generating plots.
  
  * **final_report.pdf**: The final report outlining the significance of the problem, the methodology for the analysis and insights gained from the results.
  
  * **shark_tank.csv**: Contains the first the data of the first 6 seasons of Shark Tank.
