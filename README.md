# Shark-tank-Analysis
Final Project for MGSC 661 as part of MMA program at McGill University

For the final project I have chosen the Shark Tank Dataset from Kaggle.com to predict the outcome of a business pitch on the TV show Shark Tank. A complete description of the columns can be found [here](https://www.kaggle.com/datasets/rahulsathyajit/shark-tank-pitches)

First, exploratory data analysis was conducted to understand the distribution of variables and missing values. I then performed data cleaning to remove irrelevant values and combine several columns. Afterwards, I conducted feature engineering using Random Forest to select predictors that are of higher importance. Subsequently, I implemented a boosted model with parameters tuning using the cleaned data to predict the likely outcome of a pitch on the show. 

Another recommendation using the dataset is to run a Linear Discriminant Analysis model to classify the region that an entrepreneur is likely to come from, given different criteria in order to facilitate the casting process.
