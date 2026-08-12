# Whole Person Health Index (WPHI) Psychometric Analyses R Code

Code Authors: Valerie Ryan, Ro Nadreau, Sarah Forrest, and Catherine Lamoreaux

## Code

There are three code files: one for descriptive statistical analyses; one for exploratory factor analyses (EFA), confirmatory factor analyses (CFA), and bifactor analyses; and one for differential item functioning (DIF).

### Data Preparation

There were 4 experimental groups. Use the following code, found in the factor analysis code file, to create a variable denoting experimental group and to split the data into exploratory and confirmatory sets.

```R
# create variable for experimental group
rands$exper_group = ifelse(rands$P_QOLEXP == 1 & rands$P_SOCEXP == 1, 1,
                           ifelse(rands$P_QOLEXP == 1 & rands$P_SOCEXP == 2, 2,
                                  ifelse(rands$P_QOLEXP == 2 & rands$P_SOCEXP == 1, 3, 4)))

table(rands$exper_group)

# randomly assign respondents to exploratory or confirmatory groups
set.seed(72)

rands$split_group = NA

for(g in 1:4){
  rands$split_group[rands$exper_group == g] = sample(c("explore", "confirm"), sum(rands$exper_group == g), replace = T)
}

table(rands$split_group, rands$exper_group)
```

## Data

The data used in these analyses are from the National Center for Health Statistics' (NCHS) Research and Development Survey, Round 11 (RANDS 11). You can find more information about RANDS [here](https://www.cdc.gov/nchs/rands/index.html) and the data and documentation for Round 11 [here](https://www.cdc.gov/nchs/rands/data-documentation/index.html) when it is released.

## Paper

These analyses were conducted for this paper: [Assessing the Structural Validity and Measurement Invariance of the Whole Person Health Index](https://doi.org/10.1177/27536130261453376).

## Get in touch!

If you have any questions about the code please reach out to Valerie Ryan at [vryan2@cdc.gov](mailto:vryan2@cdc.gov)

## License, Disclaimer, and Other Notices

Please see information about our license, disclaimers, and other notices [here](additional-info.md). The repository utilizes code licensed under the terms of the Apache Software
License and is free to use, redistribute, and modify.
