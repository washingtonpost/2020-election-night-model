# Election-Night Model - 2020 General Election

This is The Washington Post's general election night model. The model was created in conjunction with Decision Desk HQ/0ptimus.

## Model

The model uses quantile regression for point prediction and quantile regression + conformal prediction to generate prediction intervals. You can read about the methods employed here: 

## Data

The model in production uses data collected by Decision Desk HQ/0ptimus and The Washington Post. 

To see how the model works we have included county-level 2012 and 2016 election results for Georgia, Kansas, Kentucky, Missouri and Texas. The data was taken from the [MIT Elections Lab](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ).

We were unable to include the county level data we use in the actual model. Instead this model uses a state level fixed effect and a national fixed effect *only*. You will see, however, that it still performs well.