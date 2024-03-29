# Estimating Rejection Rates by Ethnic Group

Although absentee voting/vote-by-mail (VBM) may be a safe alternative to voting in person, it might disenfranchise underserved communities. A missing witness signature or a non-official envelope might disqualify a ballot. And rejection rates can vary widely across socio-demographic groups. A [study](https://electionscience.clas.ufl.edu/files/2020/04/Baringer_Herron_Smith_VBM_FL.pdf) of voting lists in Florida found that the absentee ballot rejection rates for minorities are twice as large as for white Americans (approximately 2% vs 1%). Other studies from elections in [LA](https://www.sciencedirect.com/science/article/pii/S0261379408000796) and [Georgia](https://electionscience.clas.ufl.edu/files/2020/05/GA_Venmo.pdf)  found higher rejection rates from first-time and non-white voters.

## Updates 

* *Monday, September 28th, 2020:* I took the number of absentee ballots requested so far from the NYT website and am using them instead of assuming a 70% absentee request rate across all states.


## North Carolina

North Carolina for some reason continuously publishes data on absentee ballots and their status at the individual level with a surprising amount of personal information. So we can actually do a lot with this.

### Raw shares

In an ideal world rejection rates would be constant and plotting shares among submitted and rejected by ethnic/racial group should be on the diagonal. Yes, the groups at the bottom are squashed together but their population share doesn't really matter here. The picture is the same as when estimating these rates at with EAVS data. Minorities generally above the line, white Americans below.

![](plots/States/NC/Shares_rejected_submitted.jpg)

### Over age

An interesting thing is that the data has a lot of personal characteristics one of which being age. So what about age over race. The values are now estimated with a binomial model and a random walk prior going from 18 to 95 for regularization for whites and non-whites. Sadly, no partial pooling but there is so much data and information is still being shared. A second version could use a Gaussian process maybe. Anyway, here estimates over age for white against non-white, female against male, low income vs high income, and their interactions. Data from [here][https://www.ncsbe.gov/results-data/absentee-data].

![](plots/States/NC/Rejected_rates_by_age_ethnicity_NC.jpeg)



![](plots/States/NC/Rejected_rates_by_age_gender_race_NC.jpeg)

![Rejected_rates_by_age_gender_race_income_NC](plots/States/NC/Rejected_rates_by_age_gender_race_income_NC.jpeg)

## Maine

Maine offers less interesting data but we can disaggregate by party id to get rejection rates. Here, just with partial pooling (essentially irrelevant given the amount of data) and a binomial logit model. Data from [here][https://www.maine.gov/sos/cec/elec/data/index.html].

![Rejected_rates_by_party_id_primaries](plots/States/ME/Rejected_rates_by_party_id_primaries.jpeg)







## Nationally

### Approach

To estimate a rejection rate of an ethnic group we would look at the share of the submitted absentee ballots of that group that had been rejected. This data is not available partially due to confidentiality concerns. The same problem exists for example if we want to estimate the share of a particular ethnic group that voted for a specific candidate (without survey data).

Instead, we can use ecological regression, a method that under some assumptions [^1] can estimate individual level behavior from aggregate data. This requires predicting for example the share of requested ballots at the county level with the local ethnic composition. As a mock-up, consider a hypothetical county that has a request rate of 75% and is 100% Latinx. The prediction then looks like 
$$
0.75 = \beta_{1}x_{1} + \beta_2x_2
$$
If $x_2 = 1$ (share Latinx), $x_1 = 0$ (share non-Latinx), and $x_1 + x_2 = 1$, then $\beta_2$ has to be $0.75$ and we would estimate the probability that a Latinx individual requests an absentee ballot as $75\%$. If the county is not homogenous, there is more uncertainty regarding the probability to vote as the 75% could be composed of some mixture of Latinx and non-Latinx individuals. But with a large number of data points, this can be relatively precisely estimated.

### Rejection rates

Estimating rejection rates is a bit more complex because it requires estimating rates at which absentee ballots are requested, rates at which they are submitted, and then the rates at which they are rejected. Our beliefs about them are necessarily interdependent. The model I use estimates the probabilities for each ethnic group for each state. Different states have different electoral rules e.g. requiring a witness signature on the absentee ballot or not. They are therefore not entirely comparable.

### Preliminary results

There are different ways to look at the data.

First, we can look at the shares of each ethnic group among voters, those who requested an absentee ballot, those who submitted an absentee ballot, and those who had their absentee ballot rejected. Minorities are overrepresented among rejected ballots.

![m11_2020-09-02_group_shares_turnout_by_state](plots/model_state_level_allVBM_none2020-09-28_group_shares_turnout_by_state.jpeg)

The last graph makes a projection expected number of rejected absentee ballots at the national levels by ethnic group assuming the same turnout as in 2016 and considering the states that have sent absentee ballots to every registered voter as well as the preliminary absentee voter data published by the NYT. For the remaining states I assume that 70% of the voter population from 2016 will request an absentee ballot. It excludes CT (data errors), AK (no estimates), and DC (no estimates).

![m11_2020-09-02_n_rejected_national_turnout_by_state](plots/model_state_level_allVBM_none2020-09-28_n_rejected_national_turnout_by_state.jpeg)

[^1]: There are two alternative assumptions both of which are sufficient to interpret the parameters as individual-level probabilities. Either, first, the probability is independent of the geographic unit the individual lives in. Or, second, the individual probability is on average unaffected by the ethnic composition in the geographic unit or any other factor.

