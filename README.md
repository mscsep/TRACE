# TRACE: Meta-analysis of learning and memory in PTSD

### Step 1: Prepare data

#### Merge datasets
- Merge data S1, S2, S3 in one file: `prepare_merge.rmd`
- input: search 1 ['TRACE data_collection_search1.xlsx'](https://osf.io/tqmwb), search 2 ['TRACE data_collection_search2.xlsx'](https://osf.io/xqpm3), search 3 ['TRACE_data_collection_search3.xlsx'](https://osf.io/wbrcm)



- output:`TRACEmerged.RDS`
  
#### Recode data
- recode variables in `prepare_recode.rmd` and add method codes
- output in `TRACErecoded.RDS`

#### Process QA data
- process QA data with `prepare_QA.rmd`

#### Calculate effect sizes
- prepare data for analysis in `prepare_effect_size_QA.rmd`
- output in `TRACEprepared.RData` 


### Step 2: Study descriptives
- Overview numbers for screening steps (for flowchart) via `Flowchart.rmd`
- Characteristics of the included studies via `Descriptives.rmd` (uses `data.explore.rds`, created in `DataDrivenAnalysis.rmd` script)

### Step 3: Meta-regression Valence x Phase

- check potential comparisons `meta_regression_check_comparisons.rmd` | 11/2/23 niet alle code hierin werkt? oude variable niet meer aangepast?


- meta-regression Valence x Phase: `meta_regression.rmd`. This script uses
`meta_regression_influentials.r` to calculate potential influential case and outliers

### Step 4: MetaForest and MetaCART
- `meta_forest_meta_cart.rmd`
- used data saved in `data.explore.rds`, this data is also used for descriptives table
