---
title: "External validation"
author: "Erik Bulow"
date: '2021-02-10'
output: 
#  github_document:
#    toc: yes
#    df_print: "kable"
  html_document:
    toc: yes
    df_print: "kable"
    toc_float: yes
    number_sections: yes
    code_folding: "hide"
    keep_md: yes
bibliography: 
  C:\\Users\\erik_\\Documents\\library.bib
  # H:\\zotero_lib.bib
---



# Start

Install and attach some useful packages


```r
# First, set random seed for reproducability:
set.seed(123)

# USeful packages
pkgs <- c("tidyverse", "doParallel", "pROC", "rms", "givitiR")

# Install if not already installed
pkgsinst <- setdiff(pkgs, rownames(installed.packages()))
if (length(pkgsinst)) install.packages(pkgsinst)

# Attatch
purrr::walk(pkgs, ~suppressPackageStartupMessages(library(., character.only = TRUE)))
```

Load the exported models to validate (also found "manually" at [Github](https://github.com/eribul/NH_luxation_infektion/blob/master/cache/ext_val_required_data.RData)).


```r
# Load the exported model object
load("../cache/model_reduced_lean.RData")
```

Let's inspect the model for 90 days just to get a sense of it:


```r
model_reduced_lean
```

```
## 
## Call:  glm(formula = f, family = binomial(), data = dfm)
## 
## Coefficients:
##                                          (Intercept)  
##                                             -6.30816  
##                                    c_cns_diseaseTRUE  
##                                              0.69078  
##                    c_fluid_electrolyte_disordersTRUE  
##                                              0.41817  
##                                  c_liver_diseaseTRUE  
##                                              0.74842  
##                                              P_ASAII  
##                                              0.17935  
##                                             P_ASAIII  
##                                              0.44308  
##                                      P_BMIoverweight  
##                                              0.38824  
##                                 P_BMIclass I obesity  
##                                              0.80532  
##                            P_BMIclass II-III obesity  
##                                              1.39988  
##                     P_DiaGrpSecondary osteoarthritis  
##                                              0.73652  
##         P_DiaGrpSequelae after childhood hip disease  
##                                              0.39169  
## P_DiaGrpAvascular necrosis of the femoral head (AVN)  
##                                              0.58384  
##                   P_DiaGrpInflammatory joint disease  
##                                              0.93573  
##                                            P_SexMale  
##                                              0.37483  
##                                     c_arrhythmiaTRUE  
##                                              0.26580  
##                           c_lung_airways_diseaseTRUE  
##                                              0.27218  
##                                                P_Age  
##                                              0.02289  
## 
## Degrees of Freedom: 88829 Total (i.e. Null);  88813 Residual
## Null Deviance:	    20420 
## Residual Deviance: 19550 	AIC: 19590
```

We should now use this model with the `predict` function combined with new data from Denmark. So, how should this data look like?

# Prepare data

## Inclusion/exlusion

Those were the inclusions/exclusions from Sweden. Similar criteria should apply also for the external validation data set to get comparable cohorts.

Ignore filtering of missing educational level, hospital type and fixation, however, since those variables are not needed in the model to validate.


```r
knitr::include_graphics("../graphs/flowchart.png")
```

<img src="../graphs/flowchart.png" width="1024" />

# Variables

## Outcome

The outcome variable is simply called `outcome` . This is a logical/boolean (or 0/1-numeric) variable indicating whether the patient got PJI within 90 days after THA (`TRUE`/1) or not (`FALSE`/0).

We identified PJI within 90 days as either the primary or secondary reason for reoperation performed within this time frame, as recorded to SHAR, or if a relevant ICD-10/NOMESCO code was recorded during a hospital visit/admission during this period.

We used regular expression to identify such codes:


```r
coder::hip_ae_hailer %>% 
  filter(group == "Infection")
```

<div class="kable-table">

|group     |icd10                                                                                        |kva                     |
|:---------|:--------------------------------------------------------------------------------------------|:-----------------------|
|Infection |(M(00(1&#124;[0289]F?)&#124;86([0-689]F?)))&#124;T(8(1[34]&#124;4(5[FX]?&#124;6F&#124;7F?))) |NF(S[0-59]&#124;W[56])9 |

</div>

## Predictors

The data to evaluate (in addition to the respective `outcome` variable) should look like this:


```r
head(ext_val_required_data) 
```

<div class="kable-table">

|c_cns_disease |c_fluid_electrolyte_disorders |c_liver_disease |P_ASA |P_BMI                |P_DiaGrp               |P_Sex  |c_arrhythmia |c_lung_airways_disease | P_Age|
|:-------------|:-----------------------------|:---------------|:-----|:--------------------|:----------------------|:------|:------------|:----------------------|-----:|
|FALSE         |FALSE                         |FALSE           |III   |overweight           |Primary osteoarthritis |Female |FALSE        |TRUE                   |    79|
|FALSE         |FALSE                         |FALSE           |II    |under/normal weight  |Primary osteoarthritis |Female |FALSE        |FALSE                  |    79|
|FALSE         |FALSE                         |FALSE           |I     |under/normal weight  |Primary osteoarthritis |Female |FALSE        |FALSE                  |    49|
|FALSE         |FALSE                         |FALSE           |II    |overweight           |Primary osteoarthritis |Female |FALSE        |FALSE                  |    76|
|FALSE         |FALSE                         |FALSE           |II    |overweight           |Primary osteoarthritis |Female |FALSE        |FALSE                  |    72|
|FALSE         |FALSE                         |FALSE           |II    |class II-III obesity |Primary osteoarthritis |Female |FALSE        |FALSE                  |    69|

</div>

thus with columns:


```r
names(ext_val_required_data)
```

```
##  [1] "c_cns_disease"                 "c_fluid_electrolyte_disorders"
##  [3] "c_liver_disease"               "P_ASA"                        
##  [5] "P_BMI"                         "P_DiaGrp"                     
##  [7] "P_Sex"                         "c_arrhythmia"                 
##  [9] "c_lung_airways_disease"        "P_Age"
```

Some of those are factor variables:


```r
ext_val_required_data %>% 
  select(where(is.factor)) %>% 
  pivot_longer(everything(), values_ptypes = list(value = character())) %>% 
  distinct() %>% 
  arrange(name, value) %>% 
  group_by(name) %>% 
  mutate(name = replace(name, duplicated(name), ""))
```

<div class="kable-table">

|name     |value                                        |
|:--------|:--------------------------------------------|
|P_ASA    |I                                            |
|         |II                                           |
|         |III                                          |
|P_BMI    |class I obesity                              |
|         |class II-III obesity                         |
|         |overweight                                   |
|         |under/normal weight                          |
|P_DiaGrp |Avascular necrosis of the femoral head (AVN) |
|         |Inflammatory joint disease                   |
|         |Primary osteoarthritis                       |
|         |Secondary osteoarthritis                     |
|         |Sequelae after childhood hip disease         |
|P_Sex    |Female                                       |
|         |Male                                         |

</div>

-   `P_Sex` and `P_ASA` should be self-explanatory
-   `P_BMI` is a broader categorization based on BMI and the [WHO classification] (<https://www.euro.who.int/en/health-topics/disease-prevention/nutrition/a-healthy-lifestyle/body-mass-index-bmi>) where overweight = "pre-obesity"
-   `P_DiaGrp` is based on ICD-10 codes recorded in SHAR and grouped into broader categories.


```r
readr::read_csv2("../data/P_DiaGrp.csv", trim_ws = TRUE)
```

<div class="kable-table">

|Diagnos PrimärOp                                           |Diagnosgrupp                                                    |
|:----------------------------------------------------------|:---------------------------------------------------------------|
|M16.9 - Koxartros, ospecificerad                           |Primär artros                                                   |
|M16.1 - Koxartros, primär                                  |Primär artros                                                   |
|M16.0 - Koxartros, primär dubbelsidig                      |Primär artros                                                   |
|M15.0 - Polyartros                                         |Primär artros                                                   |
|M24.6F - Ankylotisk led                                    |Inflammatorisk ledsjukdom                                       |
|M33.1 - Annan dermatomysit                                 |Inflammatorisk ledsjukdom                                       |
|M00.9F - Artrit UNS                                        |Inflammatorisk ledsjukdom                                       |
|M13.8 - Artrit, annan specificerad                         |Inflammatorisk ledsjukdom                                       |
|M45.9 - Bechterew, morbus                                  |Inflammatorisk ledsjukdom                                       |
|M65.9F - Ospecifik synovit                                 |Inflammatorisk ledsjukdom                                       |
|M07.3F - Psoriasisartrit                                   |Inflammatorisk ledsjukdom                                       |
|M02.9F - Reaktiv artrit UNS                                |Inflammatorisk ledsjukdom                                       |
|M08.0F - Reumatoid artrit juvenil                          |Inflammatorisk ledsjukdom                                       |
|M05.8F - Reumatoid artrit seropos                          |Inflammatorisk ledsjukdom                                       |
|M06.9F - Reumatoid artrit UNS                              |Inflammatorisk ledsjukdom                                       |
|M05.9F - Seropositiv reumatoid artrit, ospec               |Inflammatorisk ledsjukdom                                       |
|M32.9 - Systemisk lupus etrythematosus, ospec              |Inflammatorisk ledsjukdom                                       |
|S72.00 - Collumfraktur                                     |Akut trauma, höftfraktur                                        |
|S72.10 - Pertrokantär fraktur                              |Akut trauma, höftfraktur                                        |
|S72.20 - Subtrokantär fraktur                              |Akut trauma, höftfraktur                                        |
|M91.8 - Annan spec juvenil osteokondros i höft och bäcken  |Följdtillstånd efter barnsjukdom i höftleden                    |
|M91.2 - Coxa plana (sen diagnos)                           |Följdtillstånd efter barnsjukdom i höftleden                    |
|M21.0F - Coxa valga                                        |Följdtillstånd efter barnsjukdom i höftleden                    |
|M21.1F - Coxa vara                                         |Följdtillstånd efter barnsjukdom i höftleden                    |
|M93.0 - Förskjuten övre femurepifys (icke traumatisk)      |Följdtillstånd efter barnsjukdom i höftleden                    |
|M16.3 - Koxartros, annan dysplastisk                       |Följdtillstånd efter barnsjukdom i höftleden                    |
|M16.2 - Koxartros, orsakad av dysplasi, dubbelsidig        |Följdtillstånd efter barnsjukdom i höftleden                    |
|M91.1 - Perthes sjukdom                                    |Följdtillstånd efter barnsjukdom i höftleden                    |
|M87.3F - Annan sekundär osteonekros                        |Idiopatisk nekros                                               |
|M87.0F - Osteonekros                                       |Idiopatisk nekros                                               |
|M87.1F - Osteonekros orsakad av läkemedel                  |Idiopatisk nekros                                               |
|T93.1 - Collumfraktur, sena besvär efter                   |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|M84.0F - Felläkning av fraktur                             |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|M84.2F - Fördröjd frakturläkning i höftled/lårben          |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|T84.6F - Infektion efter osteosyntes                       |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|T84.3F - Mek kompl av andra instrument, implantat          |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|T84.1 - Mek kompl instr för inre fix av extremitetsben     |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|M87.2F - Ostenekros efter tidigare skada                   |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|T91.2 - Sena besvär av annan frakt på br-korgen o bäckenet |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|M84.1F - Utebliven läkning/pseudartros                     |Komplikation eller följdtillstånd efter fraktur el annat trauma |
|M90.7F - Benfraktur vid tumörsjukdom                       |Tumör                                                           |
|D16.2 - Benign tumör i nedre extremiteterna                |Tumör                                                           |
|C41.4 - Malign tumör i bäckenben, sakrum och coccyx        |Tumör                                                           |
|C40.2 - Malign tumör i nedre extremiteternas långa ben     |Tumör                                                           |
|C90.0 - Myelomatos                                         |Tumör                                                           |
|M84.4F - Patologisk fraktur UNS                            |Tumör                                                           |
|C79.5 - Sek malign tumör (metastas) i ben och benmärg      |Tumör                                                           |
|D21.2 - Synovial chondromatos                              |Tumör                                                           |
|D48.0 - Tumör av osäker el. okänd natur i ben och ledbrosk |Tumör                                                           |
|M16.5 - Koxartros, annan posttraumatisk                    |Annan sekundär artros                                           |
|M16.7 - Koxartros, annan sekundär                          |Annan sekundär artros                                           |
|M16.6 - Koxartros, annan sekundär dubbelsidig              |Annan sekundär artros                                           |
|M16.4 - Koxartros, posttraumatisk dubbelsidig              |Annan sekundär artros                                           |
|S32.40 - Fraktur på acetabulum                             |Akut trauma, övriga                                             |
|S72.30 - Fraktur på femurskaftet                           |Akut trauma, övriga                                             |
|S73.0 - Luxation i höft                                    |Akut trauma, övriga                                             |
|M84.3F - Stressfraktur                                     |Akut trauma, övriga                                             |
|M80.0F - Åldersosteoporos m fraktur                        |Akut trauma, övriga                                             |
|M94.8 - Andra spec sjukdomar i brosk                       |Övrigt                                                          |
|M00.8 - Artrit och polyartrit ors av annan spec bakterie   |Övrigt                                                          |
|M36.2 - Artropati vid hemofili                             |Övrigt                                                          |
|M25.5F - Ledvärk                                           |Övrigt                                                          |
|M93.2F - Osteochondrosis dissecans                         |Övrigt                                                          |
|M89.5 - Osteolys                                           |Övrigt                                                          |
|M86.6F - Osteomyelit, annan specificerad kronisk           |Övrigt                                                          |
|M88.8 - Pagets sjukdom i andra specificerade ben           |Övrigt                                                          |
|M96.0F - Pseudartros efter artrodes                        |Övrigt                                                          |
|M24.4F - Recidiverande lux och sublux i led                |Övrigt                                                          |
|M89.9 - Sjukdom i benvävnad, ospecificerad                 |Övrigt                                                          |
|M79.6F - Smärta ospecifik                                  |Övrigt                                                          |
|M90.0F - TBC i benvävnad                                   |Övrigt                                                          |
|M12.2F - Villonodulär synovit                              |Övrigt                                                          |

</div>

## Comorbidities

Variables prefixed by `c_`:


```r
nms     <- names(ext_val_required_data)
comorbs <- nms[startsWith(nms, "c_")]
comorbs
```

```
## [1] "c_cns_disease"                 "c_fluid_electrolyte_disorders"
## [3] "c_liver_disease"               "c_arrhythmia"                 
## [5] "c_lung_airways_disease"
```

... are logical/boolean indicators of comorbidities based on ICD-10 codes from one year prior to THA, as recorded in our National Patient Register. Individual codes were grouped according to Charlson and Elixhauser as codified by table 2 in \@Quan2005. Those conditions were then further combined according to table 1 in the drafted manuscript:


```r
comorbs_nms <- 
  comorbs %>% 
  tolower() %>% 
  {gsub("^c_", "", .)} %>%
  {gsub("_", " ", .)}

tab_categorization %>% 
  filter(
    tolower(`Comorbidities by groups`) %in% comorbs_nms
  )
```

<div class="kable-table">

|Comorbidities by groups     |Charlson                                             |Elixhauser                                                     |
|:---------------------------|:----------------------------------------------------|:--------------------------------------------------------------|
|Arrhythmia                  |NA                                                   |Cardiac arrhythmias                                            |
|CNS disease                 |Dementia, Hemiplegia or paraplegia                   |Depression, Paralysis, Other neurological disorders, Psychoses |
|Fluid electrolyte disorders |NA                                                   |Fluid electrolyte disorders                                    |
|Liver disease               |Mild liver disease, Moderate or severe liver disease |Liver disease                                                  |
|Lung airways disease        |Chronic pulmonary disease                            |Chronic pulmonary disease, Pulmonary circulation disorder      |

</div>

## Example validation for model of 90 days

We have the 90 day model from the shared R object;


```r
model <- model_reduced_lean
```

Let's assume we now have the `outcome` variable and a data frame `X` with the predictors (I will use the Swedish data just as an example but this should of course be changed for the external validation).


```r
outcome <- df$outcome
X       <- ext_val_required_data
```

# Validation of model as is


```r
# Tibble with observed and predicted outcome
obspred <- 
  tibble(
    obs  = outcome, 
    pred = predict(model, X, type = "response")
  )

# ROC curve
ROC <- pROC::roc(obspred, "obs", "pred", direction = "<")

# Estimate CI for AUC based on bootstrapping
# Use parallel processing to speed up the process
doParallel::registerDoParallel()
AUCci <- 
  pROC::ci.auc(
    ROC, 
    method          = "bootstrap", 
    boot.n          = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE, 
    parallel        = TRUE
  )


# Check calibration. Note that devel should actually be "internal" for this example but I use
# "external", since that's what you will use for the external validation. 
calibration <- 
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred$obs), 
    obspred$pred, 
    devel = "external"
  )
```

## Results

For this example we had AUC:


```r
AUCci
```

```
## 95% CI: 0.6674-0.6864 (100 non-stratified bootstrap replicates)
```

```r
plot(ROC)
```

![](README_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
plot(calibration, xlim = c(0, 0.3), ylim = c(0, 0.3))
```

![](README_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```
## $m
## [1] 2
## 
## $p.value
## [1] 0.3455003
```

# Re-calibrated intercept

Method 2 from table 1 in \@Steyerbeg2004.


```r
Z <- predict(model, X, type = "response")
  
# Refit the intercept using Z = a + Xb from above as offset
fit2 <- glm(outcome ~ 1, offset = Z)

# Same calibration and validation as above
obspred2     <- tibble(obs  = outcome, pred = predict(fit2, type = "response"))
ROC2         <- pROC::roc(obspred2, "obs", "pred", direction = "<")
AUCci2 <- 
  pROC::ci.auc(
    ROC2, 
    method = "bootstrap", 
    boot.n = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE, 
    parallel = TRUE
  )

calibration2 <- 
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred2$obs), 
    obspred2$pred, 
    devel = "external"
  )
```

## Results


```r
AUCci2
```

```
## 95% CI: 0.6639-0.6846 (100 non-stratified bootstrap replicates)
```

```r
plot(ROC2)
```

![](README_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
plot(calibration2, xlim = c(0, 0.03), ylim = c(0, 0.06))
```

![](README_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## $m
## [1] 2
## 
## $p.value
## [1] 0.3455003
```

# Re-calibration of intercenpt and calibration slope

Method 3 from table 1 in \@Steyerberg2004.


```r
fit3         <- glm(outcome ~ 1 + Z)
obspred3     <- tibble(obs  = outcome, pred = predict(fit3, type = "response"))
ROC3         <- pROC::roc(obspred3, "obs", "pred", direction = "<")

AUCci3 <- 
  pROC::ci.auc(
    ROC3, 
    method = "bootstrap", 
    boot.n = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE, 
    parallel = TRUE
  )

calibration3 <- 
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred3$obs), 
    obspred3$pred, 
    devel = "external"
  )
```

## Results


```r
AUCci3
```

```
## 95% CI: 0.6652-0.6888 (100 non-stratified bootstrap replicates)
```

```r
plot(ROC3)
```

![](README_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
plot(calibration3, xlim = c(0, 0.03), ylim = c(0, 0.06))
```

![](README_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```
## $m
## [1] 2
## 
## $p.value
## [1] 0.02293682
```

# Export data to Sweden

## ROC

If it would be OK to export coordinates for ROC-plots I would recommend this code to extract only the minimal data needed:


```r
roc_plot_coords <- 
  tibble(
    model = c("ias is", "re-calibrated intercept", "recalibrated"),
    data = 
      list( 
        tibble(
          specificities = ROC$specificities, 
          sensitivities = ROC$sensitivities
        ),
        tibble(
          specificities = ROC2$specificities, 
          sensitivities = ROC2$sensitivities
        ),
        tibble(
          specificities = ROC3$specificities, 
          sensitivities = ROC3$sensitivities
        )
      )
  )
```

## AUC with CI

The text output from `AUCci`, `AUCci2` and `AUCci3` should be enough. Hence, the same character string that gets printed above (but now stored in an object).


```r
AUCci_print  <- capture.output(AUCci)
AUCci2_print <- capture.output(AUCci2)
AUCci3_print <- capture.output(AUCci3)
```

## Export objects

Save objects above to file `export_90d.RData` (in the current working directory).


```r
save(
  roc_plot_coords,
  AUCci_print,
  AUCci2_print,
  AUCci3_print,
  calibration,
  calibration2,
  calibration3,
  file = "export_90d.RData"
)
```

# Bibliography
