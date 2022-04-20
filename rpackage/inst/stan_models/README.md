# Stan Models used in adapop package

## Semantic versioning of models

From model 8i1 the following semantic versioning of models is used.
Major (number): No model back-compability, i.e. (some) previous model cannot be used.
Minor (letter): New models/feature/inputs are added, but previous models can be run and should have the sam lpd.
Patch (number): No changes in input or ourput or new flexibilities. Only code performance.


## Model developments

### model0
A simple state space model with cauchy priors

### model2
The state-space expanded with handling also multiple time points in polls and model that explicitly using time_weights of each poll.

### model3
The state-space model is expanded with scaling with time_scale_length.

### model4
Changed model 3 to use student t and nu to model the movement over time.

### model5
State-space with partly known information on the underlying state.

### model6
Extend the model to multiple parties, i.e. a multivariate model.

### model6b
Handle missing values in data (lex Spain) and different length of parties (lex Germany).

### model8a
As model 6b, but with the following additions:
In this model we add industry bias by introducing a linear effect through kappa, a (T_known + 1) by P parameters.
There exist one kappa parameter per party and election and the prior is centered
around 0 (no industry bias). We learn a hiearchical prior parameter for all parties called
sigma_kappa that has a truncated (positive) normal prior with sd = sigma_kappa_hyper.
Hence sigma_kappa_hyper is our global parameter for how large we believe sigma_kappa can be for all parties and all elections.

### model8a1
As model8a, but with an intercept g and increasing b instead of an increasing g starting from zero.

### model8a2
As model8a, but with the last kappa assumed to be 0. This is done to test the model performance.

### model8a3
As model8a, but g is an real valued parameter instead of a real parameter. The interpretation of g is now the number of years since the last election.
This is the best working model including industry bias.

### model8a4
As model8a3, but there is one sigma_kappa per party instead of one for all parties. 

### model8b
As model 6b, but with house effects on the mean, by house, party and slower moving time period s. The house biases has a dynamic changing over the time points s.

### model8b1
As model 8b, but where there is a sum-to-zero constraint over all houses.

### model8c
As model 6b, but with house/design effects on the poll variances, by house, party and slower moving time period s. The design effects is dynamic changing over the time points s.

### model8c2
As model 6b, but with house/design effects on the poll variances, by house and slower moving time period s. The designe effect is slowly changing over s, but there is only one parameter per house.

