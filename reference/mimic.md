# Clinical data from the MIMIC-II database for a case study on indwelling arterial catheters

The Indwelling Arterial Catheter Clinical dataset contains clinical data
for 1776 patients from the MIMIC-II clinical database. It was the basis
for the article: Hsu DJ, et al. The association between indwelling
arterial catheters and mortality in hemodynamically stable patients with
respiratory failure: A propensity score analysis. Chest,
148(6):1470–1476, Aug. 2015. This dataset was also used by Raffa et al.
in Chapter 5 "Data Analysis" of the forthcoming book: Secondary Analysis
of Electronic Health Records, published by Springer in 2016.

## Usage

``` r
mimic
```

## Format

A data frame with 1776 rows and 46 variables:

- `aline_flg`:

  Integer, indicates if IAC was used (1 = yes, 0 = no)

- `icu_los_day`:

  Double, length of stay in ICU (days)

- `hospital_los_day`:

  Integer, length of stay in hospital (days)

- `age`:

  Double, age at baseline (years)

- `gender_num`:

  Integer, patient gender (1 = male; 0 = female)

- `weight_first`:

  Double, first weight (kg)

- `bmi`:

  Double, patient BMI

- `sapsi_first`:

  Integer, first SAPS I score

- `sofa_first`:

  Integer, first SOFA score

- `service_unit`:

  Character, type of service unit (FICU, MICU, SICU)

- `service_num`:

  Integer, service as a numeric value (0 = MICU or FICU, 1 = SICU)

- `day_icu_intime`:

  Character, day of week of ICU admission

- `day_icu_intime_num`:

  Integer, day of week of ICU admission (numeric)

- `hour_icu_intime`:

  Integer, hour of ICU admission (24hr clock)

- `hosp_exp_flg`:

  Integer, death in hospital (1 = yes, 0 = no)

- `icu_exp_flg`:

  Integer, death in ICU (1 = yes, 0 = no)

- `day_28_flg`:

  Integer, death within 28 days (1 = yes, 0 = no)

- `mort_day_censored`:

  Double, day post ICU admission of censoring or death (days)

- `censor_flg`:

  Integer, censored or death (0 = death, 1 = censored)

- `sepsis_flg`:

  Integer, sepsis present (0 = no, 1 = yes)

- `chf_flg`:

  Integer, congestive heart failure (0 = no, 1 = yes)

- `afib_flg`:

  Integer, atrial fibrillation (0 = no, 1 = yes)

- `renal_flg`:

  Integer, chronic renal disease (0 = no, 1 = yes)

- `liver_flg`:

  Integer, liver disease (0 = no, 1 = yes)

- `copd_flg`:

  Integer, chronic obstructive pulmonary disease (0 = no, 1 = yes)

- `cad_flg`:

  Integer, coronary artery disease (0 = no, 1 = yes)

- `stroke_flg`:

  Integer, stroke (0 = no, 1 = yes)

- `mal_flg`:

  Integer, malignancy (0 = no, 1 = yes)

- `resp_flg`:

  Integer, respiratory disease (non-COPD) (0 = no, 1 = yes)

- `map_1st`:

  Double, mean arterial pressure (mmHg)

- `hr_1st`:

  Integer, heart rate

- `temp_1st`:

  Double, temperature (F)

- `spo2_1st`:

  Integer, S_pO_2 (percent)

- `abg_count`:

  Integer, arterial blood gas count (number of tests)

- `wbc_first`:

  Double, first white blood cell count (K/uL)

- `hgb_first`:

  Double, first hemoglobin (g/dL)

- `platelet_first`:

  Integer, first platelets (K/u)

- `sodium_first`:

  Integer, first sodium (mEq/L)

- `potassium_first`:

  Double, first potassium (mEq/L)

- `tco2_first`:

  Double, first bicarbonate (mEq/L)

- `chloride_first`:

  Integer, first chloride (mEq/L)

- `bun_first`:

  Integer, first blood urea nitrogen (mg/dL)

- `creatinine_first`:

  Double, first creatinine (mg/dL)

- `po2_first`:

  Integer, first PaO_2 (mmHg)

- `pco2_first`:

  Integer, first PaCO_2 (mmHg)

- `iv_day_1`:

  Double, input fluids by IV on day 1 (mL)

## Source

<https://physionet.org/content/mimic2-iaccd/1.0/>
