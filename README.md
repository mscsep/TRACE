# TRACE: Meta-analysis of learning and memory in PTSD

### Step 1: Prepare data

#### Merge datasets
- Merge data S1, S2, S3 in one file: `merge_datasets.rmd`.-> `prepare_merge.rmd`
- Result stored in:`TRACEmerged.RDS`
  
#### Recode data
- recode variables in `recode_merged_data.rmd` -> `prepare_recode.rmd` and add method codes
- output in `TRACErecoded.RDS`

#### Process QA data
- process QA data with `prepare_QA.rmd`

#### Calculate effect sizes
- prepare data for analysis in `add_effect_size_QA.rmd` -> `prepare_effect_size_QA.rmd` (was dataPreparation) 
- output in `TRACEprepared.RData` 


### Step 2: Study descriptives
- Overview numbers for screening steps (for flowchart) via `Flowchart.rmd`
- Characteristics of the included studies via `Descriptives.rmd` (uses `data.explore.rds`, created in `DataDrivenAnalysis.rmd` script)

### step 3: Meta-regression Valence x Phase

- check potential comparisons `meta_regression_check_comparisons.rmd`
- meta-regression Valence x Phase: `meta_regression.rmd`. This script uses
`meta_regression_influentials.r` to calculate potential influential case and outliers

### step 4: MetaForest and MetaCART
- `meta_forest_meta_cart.rmd`
- used data saved in `data.explore.rds`, this data is also used for descriptives table
