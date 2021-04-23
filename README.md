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

--> 1e analysis is phase x valence 
[ with different sub comparisons for animal and human ]

- follow-up might be with cuectx for animals (not enough context data for humans)



### step 5: Data-driven Analysis: metaForest / metaCART?

- 
