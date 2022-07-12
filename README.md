# Statistical Election Fraud Detection: Supervised Learning

## :star: Summary
I train a range of supervised machine learning methods (regularized regression, random forest, gradient boosting) on synthetic training data for the probabilistic detection of systematic manipulation in fine-graded election results. When constructing a fraud detection prototype in the context of elections, using empirical data for training comes with several challenges: 

- *Uncertainty of class membership.* Although for some elections in autocratic regimes, there are anecdotal observations (e.g. video footage, official observer reports) of manipulation at few localities, we do not know the exact degree and type of election fraud for any empirical case.  
- *Unbalanced class sizes.* The vast majority of conducted elections are clean. Supervised algorithms would need to learn from a small number of empirical cases. 
- *Few observations.* The number of empirical elections that we can use for training is not sufficient for learning high-order interactions and non-linearities across the feature space.  

In order to circumvent these challenges, I **train supervised models on synthetic election data** for which manipulation is incorporated in the data generating simulations and the type and degree of fraud is known. 

## :mortar_board: Which problem am I solving?
There is a range of statistical approaches available to separate anomalies from fraud-free processes in fine-graded election data. These focus on different numerical characteristics such as Benford patterns in digit distributions of observed vote counts, spikes in the density mass of turnout and vote share distributions around integer percentages, or skewness and kurtosis of turnout and support rates. These suffer from a **central problem:** 

- Each test is developed around one individual numerical characteristic of voting returns, while being agnostic to other features that have been sucessfull in identifying fraud
- It is unclear how inconclusive results across different numerical attributes weigh into substantive conclusions

:heavy_check_mark: I train supervised machine learning models on a multivariate feature space. This weights heterogeneous numerical approaches for fraud detection against each other

:heavy_check_mark: Unified statistical framework for probabilistic election fraud detection synthesizing multiple standalone tests with each other

## :microscope: Feature Engineering: How does fraudulent election data look like?

### 1. Spikes in the density mass of vote share distributions
- We can construct the distribution of vote shares for one candidate/party across a large number of units (e.g. polling stations) 
- In clean elections, distributions are relatively smooth with no systematic spikes
- Systematically *rounding up* vote shares for one candidate/party during vote counting leads to spikes in the density mass around *exactly integer values* that are multiples of 5 (e.g. 55%, 60%, 65%, 70%, ...)

<p float="left">
  <img src="spikes_clean.png" width="300" />
  <img src="spikes_fraud.png" width="300" /> 
  <img src="spikes_fraud_zoom.png" width="150" /> 
</p>

### 2. Skewness and kurtosis in bivariate distribution p(turnout, vote share)
- In clean elections, the bivariate distribution between the level of turnout and a candidate's vote share across all polling stations is approximated reasonably well by two orthogonal Gaussian distributions 
- When *stuffing the ballot box* with pre-prepared ballots, systematic clusters, skewness and kurtosis that can be quantified appears in the distribution between the number of registered ballots (turnout) and the winner's vote share 

<p float="left">
  <img src="comet_emp.png" width="650" />
</p>

### 3. Frequencies of digits (0,1,2,3...) in vote totals


