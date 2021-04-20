# TRACE: Meta-analysis of learning and memory in PTSD


### step 1: Merge

- Merge data S1, S2, S3 in one file: `merge_datasets.rmd`. 
- Result stored in:`TRACEmerged.RDS`
  
  
  
### step 2: Recode

- recode variables in `recode_merged_data.rmd` 
- output in `TRACErecoded.RDS`


### step 3: Prepare

- prepare data for analysis in `add_effect_size_QA.rmd` (was dataPreparation) 
- output in `TRACEprepared.RData` 







### step 4: Theory-driven Analysis: 

-  check potential comparisons `check_comparisons.rmd`


- meta-regression Valence x Phase: `TheoryDrivenAnalysis.r` [earlier analysis.r]



### step 5: Data-driven Analysis: metaForest / metaCART?

- 
