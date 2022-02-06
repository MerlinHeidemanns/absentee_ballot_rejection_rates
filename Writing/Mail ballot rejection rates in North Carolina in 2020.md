# Mail ballot rejection rates in North Carolina in 2020

##### Merlin Heidemanns, PhD Student at Columbia University

Due to the ongoing COVID-19 pandemic, record numbers of voters will vote by mail in the General Election in 2020. Voting by mail is generally a safe way from the perspective of the electoral system. Yet, additional safety measures lead to greater rejection rates than voting in person, for example due to a missing witness signature or a signature mismatch. While intended to keep voting by mail safe, many of these measures result in **substantially higher rejection rates of mail ballots for minority groups** and those who are otherwise socio-economically disadvantaged. While white Americans in North Carolina face rejection rates between 1 and 3%, **rejection rates for Black citizens can range as high as 9%** for those in lower income areas. This is most likely not driven by first time voters.

## Data and Estimation

Below I use individual mail ballot data from North Carolina for the 2020 election to estimate mail ballot rejection rates. I exclude mail ballots that were undeliverable and those whose status is still pending. I designate the categories SIGNATURE DIFFERENT (7), ACCEPTED - CURED (1298), CONFLICT (9), DUPLICATE (5), NOT PROPERLY NOTARIZED (1), PENDING CURE (7272), SPOILED (2755), and WITNESS INFO INCOMPLETE (482) as rejected. I count cured ballots as rejected because not every state allows ballots to be cured and hence these would have been rejected otherwise.

Instead of using raw rejection rates from the data, I estimate them from the data to get uncertainty estimates and apply some regularization in cases where only few ballots are available. I estimate the rates by group over age and use a random walk prior to share information across nearby age categories. This assumes that rejection rates are smooth over age for the same category, for example Black Americans which seems  reasonable. Rejection rates for Black Americans for are 47 should not be substantially different from those who are 48 or 46. All code can be found [here](https://github.com/MerlinHeidemanns/absentee_ballot_rejection_rates). Data is taken from the official website of [North Carolina's State Board of Elections][https://www.ncsbe.gov/results-data/absentee-data]. 

### Raw summary

There are slightly over 330,000 individual mail ballots so far. White Americans make up approximately 71% (260092) of ballot submission and 53.4% (6221) of ballot rejections. Black Americans in North Carolina make up 16.7% (60304) of submitted ballots and 34.9% (4,065) of rejected ballots. This suggests a discrepancy in the rejection rates.

### Ethnicity

We can first look at only ethnicity as a predictor for the likelihood to see one's ballot rejected.

![Rejected_rates_by_age_ethnicity_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_ethnicity_NC.jpeg)

The estimates clearly show that rejection rates are by far higher for Black Americans than for white Americans in North Carolina so far.

### Ethnicity and Income

I split counties in below and above median income whereby the median is the unweighted median over the counties in North Carolina. Income comes from the American Community Survey from 2013 to 2018. I then estimate mail ballot rejection rates for Black and white citizens and the two income categories. I omit Latinx, Asian, and others because the number of ballots is very small

![Rejected_rates_by_age_income_race_white_black_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_income_race_white_black_NC.jpeg)

We can see that rejection rates for Black Americans in both lower and higher income areas are substantially higher than for White Americans. Rejection rates are also higher for both groups in lower income areas but this appears to be more pronounced for black Americans.

### Ethnicity and Gender

Here I look at the two by two comparison of gender (only including self-identified males and females) and Black and white Americans in North Carolina.

![Rejected_rates_by_age_gender_race_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_gender_race_NC.jpeg)

Rejection rates for Black Americans are substantially higher than for white Americans whether we compare female or male citizens.

### Ethnicity, gender, income

We can split the graphs up further and consider the interaction of gender and income within race.

![Rejected_rates_by_age_gender_race_income_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_gender_race_income_NC.jpeg)

This showed that the difference is most pronounced for middle-aged Black American women with median estimated rejection rates of 10%.

### Ethnicity, gender, party id

Instead of gender, we can look at party id. Given the adjustment for income and age, party id is most likely accounting for different levels of education in the voter profiles of each party rather than a unique effect of being a registered Democrat or Republican for that matter. 

![Rejected_rates_by_age_pid_race_income_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_pid_race_income_NC.jpeg)

The estimates suggest that Black registered Republicans in lower income areas could have among the highest rejection rates but the estimates are quite noisy as this group is comparatively small. The pattern of  higher rejection rates for Black American persists within party affiliations. Changing the comparison makes this even clearer.

![Rejected_rates_by_age_pid_income_race_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_pid_income_race_NC.jpeg)

Americans living in richer counties have lower rejection rates but they remain substantially higher for those who are Black.

### First time mail voters and ethnicity

A common perception is that high mail ballot rejection rates are driven by first time mail voters. Here I define somebody as having voted by mail before if they have voted by mail in the last two general elections (2016, 2018). This could be improved by considering the primaries in 2020 as well.

At this point, substantial overlap does not indicate that first time mail voters generally drive higher rejection rates. Instead, we only see a difference for middle age Black voters but in the opposite direction, i.e. lower rejection rates for those who haven't voted by mail before. This may be the case due to a lower degree of representativeness among Black voters who have voted by mail in previous years compared to this year.

![Rejected_rates_by_age_ethnicity_first_time_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_ethnicity_first_time_NC.jpeg)

### First time mail voters, ethnicity, and income

If we disaggregate further we can see some suggestive differences between first time mail voters and those who have voted by mail before for younger and middle age Black voters in poorer areas. This support the previos graph. There again do not appear to be substantial differences for white mail voters.

![Rejected_rates_by_age_ethnicity_first_time_income_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_ethnicity_first_time_income_NC.jpeg)

### 2016 vs 2020

Below are rejection rates for absentee ballots for 2016 and 2020 showing that rates this year are lower for white voters but largely the same for Black voters.

![Rejected_rates_by_age_race_zip_income_2016v2020_NC](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Rejected_rates_by_age_race_zip_income_2016v2020_NC.jpeg)

### Cure probability

North Carolina is one of the few states allowing voters to cure their ballot if they have made a mistake. White voters are far more likely to cure their ballot compared to other racial and ethnic groups.

![Cure_probability](/Users/merlinheidemanns/Documents/Research/research_self/absentee_ballot_rejection_rates/plots/States/NC/Cure_probability.jpeg)

## Conclusion

The estimates above show that voting by mail in North Carolina systematically disadvantages Black Americans compared to white Americans. It further increases the difficulty of lower income voters to have their voice heard. Female citizens appear to face higher rejection rates than male citizens.