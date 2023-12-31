Final Project Proposal
================

## Group Members

- Joshua Carpenter(jrc2270)
- Mingzhi Chen(mc5503)
- Ruoying Deng(rd3097)
- Ruijie He(rh3195)
- Zhuodiao Kuang(zk2275)

## Project Title

Factors influencing survival outcomes in AIDS patients

## Motivation

AIDS, or Acquired Immunodeficiency Syndrome, remains a significant
global health challenge, with millions affected by HIV\[1\]. Effective
treatment strategies are crucial for improving the quality of life of
patients and prolonging survival\[2\]. Our analysis seeks to leverage
this rich dataset to gain insights into factors influencing survival
outcomes in AIDS patients and to evaluate the efficacy of different
treatment modalities.

Additionally, we think this project will allow us to combine skills
learned in this class with skills from all our other classes, especially
Biostatistical Methods, Principals of Epidemiology, and Survival
Analysis.

## Intended Final Products

We intend to provide a report including an introduction to the data and
the project, visualizations of disease progression over time, and an
analysis of survival across treatment methods. Then, we will create a
website containing interactive graphics and life-tables.

## Anticipated Data Sources

The AIDS Clinical Trials Group Study 175 dataset\[3\] provides a
comprehensive collection of healthcare statistics and categorical
information about patients diagnosed with
[AIDS](https://archive.ics.uci.edu/dataset/890/aids+clinical+trials+group+study+175).
Initially published in 1996, this dataset aims to predict whether a
patient died within a specific observation window. Comprising 2139
instances with 23 features, the dataset was created to examine the
performance of two distinct AIDS treatments. Key variables include
treatment indicators, patient demographics, medical history, and the
Karnofsky score, which gauges a patient’s overall health and functional
status.

## Methods

For our final project, we will start with data wrangling. Then, we will
create a life table after a brief exploratory analysis using data
summaries and visualization. Besides, we will generally use R in the
research, and most of the work may be uploaded to Github. There are
various methods for handling right-censored and left-truncated survival
data with covariates, which are variables that may affect the survival
time or the risk of the event. Therefore, multiple methods will be
applied to our research.

## Planned Timeline

|          Date          |                    Descriptions                     |
|:----------------------:|:---------------------------------------------------:|
|       11/11/2023       |                Complete the proposal                |
| 11/12/2023- 11/19/2023 | Data collection, cleaning, and exploratory analysis |
| 11/20/2023- 12/02/2023 |            Data analysis, create report             |
| 12/03/2023- 12/09/2023 |       Create website and interactive graphics       |
|       12/10/2023       |                       Submit                        |

# Reference

\[1\] Whiteside, A. and D. Wilson, Health and AIDS in 2019 and beyond.
Afr J AIDS Res, 2018. 17(4): p. iii-v. \[2\] Eisinger, R. W., & Fauci,
A. S. (2018). Ending the HIV/AIDS pandemic. Emerging infectious
diseases, 24(3), 413. \[3\] Hammer, S. M., Katzenstein, D. A., Hughes,
M. D., Gundacker, H., Schooley, R. T., Haubrich, R. H., … & Merigan, T.
C. (1996). A trial comparing nucleoside monotherapy with combination
therapy in HIV-infected adults with CD4 cell counts from 200 to 500 per
cubic millimeter. New England Journal of Medicine, 335(15), 1081-1090.
