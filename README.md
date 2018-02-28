# Predicting-who-wins-2008-Democratic-Primaries-Elections--Clinton-vs.-Obama
Imagine you are the front runner for democratic party primaries in 2008 - 1 week into elections you have won a few states(Obama) and your opponent (Hillary) is catching up. How you can use analytics to predict which of the remaining seats will you win using demographic data from states you won and lost. Can we accurately classify win or lose for the remaining seats using data. Can we use this prediction to find out factors which impact our chances of winning and improve our appeal to places that are predicted loss thus improving our chances of winning? Well , Let's find out!

Please run the associated R script to find all visualisations listed here - 

1)
This plot begins to make the case that those with higher rates of speaking Non English in the home,
are, on average, less likely to own a home. It could be argued that high levels of English proficiency
are often required to be able to succeed and afford a home. There are many other factors to include
in this correlation, but this visualization is a starting point.
Running a simple regression on these two variables does provide a significant negative correlation,
but there endogeneity issues to consider. For example, speaking Non-English could be correlated
with average income or urban vs rural environments.
This data does not show which candidate each county voted for, but it helps to explain the patterns
in each population. This is useful for political parties in creating campaign strategies and tailoring
messages to certain attributes of a group.

2.
I compared various models in order to predict the winning spread of Obama . I used simple
linear regression with all demographic variables, linear regression with interaction and selective
variables and a K Nearest Neighbour approach. We could not use logistic regression here because
it doesn't work when Outcome is negative.

I used Out of Sample R squared as our Performance measurement metric. We divided the given
Train dataset further into 2 fold where one was used to train and other part was tested upon. Since
we were evaluating models using OOS R2 , we could not know how would a KNN model perform
as compared to Linear regression with interaction. That's why we chose Linear regression with
interaction as our final model. Eventually, through some trial and error and attempting to eliminate
variables that would be highly correlated (like RetiredWorkers and SocialSecurityRate) we settled
on this model. This model favors variables expressed as proportions instead of variables expressed
in absolute terms (like Pop or FarmArea), and mixes population demographics with geographic and
political considerations (like ElectionType and Region). Some trial and error was used to find the
right balance of variables, as well as the interaction term that would provide the best use of
ElectionType.

The final model I chose was:

model.linear <- glm(Obama_margin_percent ~ Age65andAbove + Asian + White +
AmericanIndian + Hispanic + Bachelors + Poverty + IncomeAbove75K + MedianIncome +
MedicareRate + SocialSecurityRate + DisabilitiesRate + PopDensity + log(FarmArea) +
Homeowner + Region + Black + ElectionType*UnemployRate, data = a1, subset = train)

After choosing our model we applied a 10 Fold cross validation to see how our model will perform
on the test data set/population dataset. This gave us individual R2 for 10 iterations and a mean R2
of 10 iterations.

Mean R2 10 FOLD Cross validation for Model 1(Simple regression without interaction) as
compared to null model:

linear null
0.639853445 -7.213815e-05

Mean R2 10 FOLD Cross validation for Model 2( Linear Regression with interaction) as compared
to null model:
linear null
0.669853445 -0.006842169

KNN with K=15 is MODEL 3.

3.
With regard to an unsupervised learning tool applied to explore the data, firstly I clarified
Clustering as our Core Task. We are interested in the distribution of votes based on these 2
attributes of counties: Unemployment Rate and Average Income, so we applied the k-means tool
to see the clustering pattern.
As shown above, 3 distinct clusters are given by k-means method:
(1) low or medium Unemployment Rate, and low or medium Average Income;
(2) low or medium Unemployment Rate, and high Average Income;
(3) high Unemployment Rate, and low or medium Average Income.


Another Consideration: 
With regard to an unsupervised learning tool applied to explore the data, firstly I clarified
Clustering as our Core Task. We are interested in link between Social Security usage in a given
district and their voting habits. We used 6 clusters, hoping to have groups that represent strength
of affinity for a candidate as well as greater and lesser Social Security usage.
There is a broader trend here where those who use more SS are less likely to vote for Obama. That's
useful to know on its own, but, more than that, we could use this as a jumping off point to explore
these groups further. Political campaigns operate better when they can identify swing groups, and 


this could be a useful tool to begin to identify swing voters and to tailor messages to that specific
group. The orange, pink, and yellow groups are less likely to swing, but the blue and grey groups
show voting patterns without a clear preference in candidate. Between those swing groups, further
work can be done to identify patterns and demographic differences. The Obama campaign could
look at the undecided group that tends to use SS more (grey) and could frame social security talking
points when in similar distincts to the needs of those areas.

Final Summary :

Obama tends to win when districts are less white, more black, more wealthy, younger, and less
reliant on government services. Essentially, it seems that the further a given area is from the
pragmatic benefits of government, the more willing they seem to be to vote for Obama.
Our advice to Clinton would be to focus her messaging on the pragmatic and tangible aspects in
which government supports and benefits people, specifically targeting areas within states that are
more rural, wealthier communities, and communities that rely heavily on medicare, unemployment
benefits, and social security. These seem to be populations resistant to Obama’s more rhetorical
appeal.
A specific examination of the races where the margin of victory has been within 5% of total votes
for either candidate reveals 189 county races. A disproportionate number of those races have been
in the midwest, where the two candidates have been neck and neck in wins - this signals that there
are a number of races in the midwest being won on small margins, where a campaign focus can
shift close losses into close wins. Caucasus tend to be closer than primaries, and so focusing
volunteer organizing on caucus states may, likewise, yield disproportionate results. Other areas that
tend to be extremely close are lighter density suburban areas. In the April primary in Pennsylvania,
for instance, I would encourage Mrs Clinton to focus persuasive efforts and advertising on those in
suburban areas, and focus GOTV efforts on more rural communities or smaller towns. 
