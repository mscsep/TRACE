# TRACE: Meta-analysis of learning and memory in PTSD

### Step 1: Prepare data

#### Merge datasets
- script: Merge data S1, S2, S3 to one file with `prepare_merge.rmd`.
- input: search 1 [TRACE data_collection_search1.xlsx](https://osf.io/tqmwb), search 2 [TRACE data_collection_search2.xlsx](https://osf.io/xqpm3), search 3 [TRACE_data_collection_search3.xlsx](https://osf.io/wbrcm)
- output:`TRACEmerged.RDS`

#### Recode data
- script: Recode variables and add method codes with `prepare_recode.rmd`.
- input:`TRACEmerged.RDS`, [TRACE_method_codes.xlsx](https://osf.io/25et8)
- output: `TRACErecoded.RDS`

#### Process QA data
- script: process QA data with `prepare_QA.rmd`
- input: [TRACE_RoBQA_data.xlsx](https://osf.io/tckb5), 
- output: `TRACE_QA_animal.RData`, `TRACE_QA_human.RData`, and `RoB.jpeg` (optional: `RoB_clinical.jpeg`, `RoB_preclinical.jpeg`)

#### Calculate effect sizes
- script: prepare data for analysis in `prepare_effect_size_QA.rmd`
- input: `TRACErecoded.RDS`, `TRACE_QA_animal.RData`, and `TRACE_QA_human.RData`
- output: `TRACEprepared.RData` (nb n=1647)


### Step 2: Meta-regression Valence x Phase
- script: meta-regression Valence x Phase: `meta_regression.rmd`. This script uses `meta_regression_influentials.r` to calculate potential influential case and outliers
- input: `TRACEprepared.RData`
- output: datasets used in analyses: `clinical.data.metaregression.RDS`, `preclinical.data.metaregression.RDS` and results (main, diagnostics, sensitivity, graphs)
  - clinical: `phase_valence_PTSD_clinical.csv`, `phase_valence_PTSD_clinical.tiff`, `funnel.colours.clinical.tiff`, `phase_valence_PTSD_clinical.FSN.csv`, `influentials.clinical.rds`, `sens.clinical.infout.csv`, `sens.mod.A.csv`, `sens.mod.B.csv`, `sens.mod.C.csv`
  - preclinical: `phase_valence_PTSD_preclinical.csv`, `phase_valence_PTSD_preclinical.tiff`, `funnel.colours.preclinical.tiff`, `phase_valence_PTSD_preclinical.FSN.csv`, `influentials.preclinical.rds`, `sens.preclinical.infout.csv`, `sens.mod.E.csv`, `sens.mod.D.csv`, `sens.mod.F.csv`
  - figure: `PTSD.clinical.preclinical.tiff`
 
 
### Step 3: MetaForest and MetaCART
- script: `meta_forest_meta_cart.rmd`
- input: `TRACEprepared.RData`
- output: `data.explore.rds` (NB: also used for descriptives table)
  - clinical: `clinical.data.explorative.RDS`, `preclinical.data.explorative.RDS`, `fitted.clinicalMetaForest.RDS`, `metaforest_Clinical_convergence.tiff`, `metaforest_Clinical_varImportance.tiff`, `important_variables_clinical_metaforest.csv`, `metaforest_PD_clinical.tiff`
  - preclinical: `fitted.preclinicalMetaForest.RDS`, `metaforest_Preclinical_convergence.tiff`, `metaforest_Preclinical_varImportance.tiff`, `REtree.P.rds`, `metaCART.preclinical.tiff`, `important_variables_preclinical_metaforest.csv`, `metaforest_PD_preclinical.tiff`, `metaforest_PD_preclinical_metaCARTfollowup.tiff`, `VarImp.clinical.preclinical.tiff`


### Step 4: Vizualization and Study descriptives

#### Flowchart
- script: `flowchart.rmd`
- input: PMID hits & screening search 1, 2 and 3: [Review PTSD Cognition AbstractScreening_Overeenstemming EG & MS v15.11.2016.xlsx](https://osf.io/7k6wh), [pubmed_result_search2_human 6.1.20.txt](https://osf.io/7kdmu), [pubmed_result_search2_animal 6.1.20.txt](https://osf.io/fs2gq), [pmid.human.s3.learn.22.5.20.txt](https://osf.io/nqgaf), [pmid.animal.s3.learn.22.5.20.txt](https://osf.io/s3ake), [Checked_TRACE_screening_search3_MS_EG.xlsx](https://osf.io/famr7)
- some additional code for screening in search 2
  - step 1: identify inconsistencies in search 2. *input* (initial screening data): [TRACE_screening_search2_SH.xlsx](https://osf.io/pu6bf) & [TRACE_screening_search2_MS.xlsx](https://osf.io/56fyu); *output* (abstract screening inconsistencies): [inconsistencies_human_s2.csv](https://osf.io/jgf9v) &  [inconsistencies_animal_s2.csv](https://osf.io/9z2tj)
  - step 2: identify required full text checks in search 2. *input* (files with abstract screening feedback): [inconsistencies_animal_s2_SH.csv](https://osf.io/ugyrp) & [inconsistencies_human_s2_SH.csv"](https://osf.io/kve78); *output* (uploaded to OSF): [required_full_checks_human_s2.csv](https://osf.io/92mf5) & [required_full_checks_animal_s2.csv](https://osf.io/pwtxq)
  - step 3: get results full text check search 2. *input* (files with full text screening feedback): [required_full_checks_animal_s2_MS.csv"](https://osf.io/xmua6) & [required_full_checks_human_s2_SH.csv"](https://osf.io/ce35r); *output* (uploaded to OSF): [animal_inclusions_s2.csv](https://osf.io/m9rey) & [human_inclusions_s2.csv](https://osf.io/fe5sh)
- input: Data extraction information: [TRACE data_collection_search1.xlsx](https://osf.io/tqmwb), [TRACE data_collection_search2.xlsx](https://osf.io/xqpm3), [TRACE_data_collection_search3.xlsx](https://osf.io/wbrcm)  
- input: analyses datasets (NB output analyses scripts): `TRACEprepared.RData`, `clinical.data.metaregression.RDS`, `clinical.data.explorative.RDS"`, `preclinical.data.metaregression.RDS`, `preclinical.data.explorative.RDS`
- output: `unique_screened_articles.csv`. en `flowchart.tiff`
- optional output (used to select papers for QA): `Human.PMIDs.QA.S2.csv`, `Animal.PMIDs.QA.S2.csv`. en `Human.PMIDs.QA.S3.csv` en `Animal.PMIDs.QA.S3.csv`

#### Characteristics of the included studies
- Script: `descriptives.rmd`
- input: `data.explore.rds`, created in `DataDrivenAnalysis.rmd`
- output: tables in word files: `descriptives.comparison.doc`, `descriptives.csv` (optional), `descriptives.combined.doc`, `descriptives.clinical.doc`, `descriptives.preclinical.doc`
  
#### Visualize QA
- script: `visualize_QA.Rmd`
- input: `TRACE_QA_animal.RData`, `TRACE_QA_human.RData`, `clinical.data.metaregression.RDS` `preclinical.data.metaregression.RDS` (Note: these objects are outputs from `prepare_QA.RMD` and `meta_regression.rmd`)
- output: `RoB.jpeg`