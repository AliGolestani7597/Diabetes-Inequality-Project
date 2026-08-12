# Synthetic demonstration dataset

`steps_inequality_synthetic.dta` and `steps_inequality_synthetic.csv` contain
10,000 fully synthetic observations and the same variables as the restricted
combined analysis dataset. The two files contain identical synthetic content in
Stata and CSV formats.

## Intended use

The files support code review, teaching, software testing, and demonstration of
the published analysis workflow. They are not survey observations and must not
be used to report substantive prevalence estimates or article results.

## Generation method

The generator fits regularised conditional models to aggregate relationships in
the restricted source data, then draws new demographic, socioeconomic, clinical,
missingness, and survey-weight values using a fixed random seed. Disease and
care-cascade variables are recalculated under the definitions used in the
cleaning script. Small probability smoothing and fresh residual noise are used
to avoid reproducing participant records.

The generated data were checked for:

- identical variable names and order;
- valid disease/cascade missingness rules;
- broadly similar marginal distributions and missingness;
- zero exact full-row matches with the restricted source dataset.

These checks reduce disclosure risk but are not a formal institutional privacy
certification. The synthetic files should still be reviewed under the data
custodian's disclosure policy before public release.

## Variables

| Group | Variables |
|---|---|
| Survey and demographics | `year`, `age`, `sex`, `area`, `province`, `marriagestatus` |
| Socioeconomic position | `WI`, `WI_ranked`, `WI_National`, `WI_National_ranked`, `education`, `education_ranked` |
| Diabetes | `taking_dm_drug`, `ever_told_dm`, `FBS`, `HbA1C`, `DM`, `DM_detection`, `DM_treatment`, `DM_control_good`, `DM_control_fair` |
| Survey weights | `W_Questionnaire`, `W_Anthropometry`, `W_Laboratory` |

The coding follows the analysis scripts. In particular, the ranked wealth and
education variables reverse the direction of their corresponding source
categories, and cascade indicators are defined only among disease cases.
