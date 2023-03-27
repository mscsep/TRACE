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
- input: [TRACE_RoBQA_data.xlsx](https://osf.io/tckb5)
- output: `TRACE_QA_animal.RData`, `TRACE_QA_human.RData`, and `RoB.jpeg` (optional: `RoB_clinical.jpeg`, `RoB_preclinical.jpeg`)

> count data in meta-analyes:
> readRDS("processed_data/clinical.data.metaregression.RDS") %>%
+   distinct(PMID) ->clinical.meta.papers
> nrow(clinical.meta.papers)
[1] 92
> readRDS("processed_data/preclinical.data.metaregression.RDS") %>%
+   distinct(PMID)->preclinical.meta.papers
> nrow(preclinical.meta.papers)
[1] 182


#### Calculate effect sizes
- script: prepare data for analysis in `prepare_effect_size_QA.rmd`
- input: `TRACErecoded.RDS`, `TRACE_QA_animal.RData`, and `TRACE_QA_human.RData`
- output: `TRACEprepared.RData` (nb n=1647)


### Step 2: Meta-regression Valence x Phase
- script: meta-regression Valence x Phase: `meta_regression.rmd`. This script uses `meta_regression_influentials.r` to calculate potential influential case and outliers
- input: `TRACEprepared.RData`
- output: datasets used in analyses: `clinical.data.metaregression.RDS`, `preclinical.data.metaregression.RDS` and results (main, diagnostics, sensitivity, graphs)
-- clinical: `phase_valence_PTSD_clinical.csv`, `phase_valence_PTSD_clinical.tiff`, `funnel.colours.clinical.tiff`, `phase_valence_PTSD_clinical.FSN.csv`, `influentials.clinical.rds`, `sens.clinical.infout.csv`, `sens.mod.A.csv`, `sens.mod.B.csv`, `sens.mod.C.csv`
-- preclinical: `phase_valence_PTSD_preclinical.csv`, `phase_valence_PTSD_preclinical.tiff`, `funnel.colours.preclinical.tiff`, `phase_valence_PTSD_preclinical.FSN.csv`, `influentials.preclinical.rds`, `sens.preclinical.infout.csv`, `sens.mod.E.csv`, `sens.mod.D.csv`, `sens.mod.F.csv`
-- figure: `PTSD.clinical.preclinical.tiff`
 
 -> 8.3.22 checked tot hier (en code gerund.) -> bekijkn of resultaten overeen komen met paper (in tabellen).. indien ja dan 'comments' uit script halen.
 
### Step 3: MetaForest and MetaCART
- script: `meta_forest_meta_cart.rmd`
- input: `TRACEprepared.RData`
- output: `data.explore.rds` (NB: this data is also used for descriptives table; 

file='processed_data/clinical.data.explorative.RDS'); 
file='processed_data/preclinical.data.explorative.RDS')
 "processed_data/REtree.P.rds")
lts/metaCART.preclinical.tiff"
/fitted.clinicalMetaForest.RDS")

="results/metaforest_Clinical_convergence.tiff", 
results/metaforest_Clinical_varImportance.tiff",
/important_variables_clinical_metaforest.csv")
"results/metaforest_PD_clinical.tiff",
e="results/metaforest_PD_clinical_metaCARTfollowup.tiff", 

file="processed_data/fitted.preclinicalMetaForest.RDS")
="results/metaforest_Preclinical_convergence.tiff", h
e="results/metaforest_Preclinical_varImportance.tiff",
),],"results/important_variables_preclinical_metaforest.csv")
="results/metaforest_PD_preclinical.tiff", 
="results/metaforest_PD_preclinical_metaCARTfollowup.tiff",
("results/VarImp.clinical.preclinical.tiff", 


### Step 4: Study descriptives

- Overview numbers for screening steps (for flowchart) via `Flowchart.rmd`
-- input: 

** PMID hits & screening search 1, 2 and 3
[Review PTSD Cognition AbstractScreening_Overeenstemming EG & MS v15.11.2016.xlsx](https://osf.io/7k6wh)
[pubmed_result_search2_human 6.1.20.txt](https://osf.io/7kdmu) 
[pubmed_result_search2_animal 6.1.20.txt](https://osf.io/fs2gq) 
[pmid.human.s3.learn.22.5.20.txt](https://osf.io/nqgaf) 
[pmid.animal.s3.learn.22.5.20.txt](https://osf.io/s3ake)

*** some additional code for screening in search 2
**** step 1: identify inconsistencies in search 2
input (initial screening data): [TRACE_screening_search2_SH.xlsx](https://osf.io/pu6bf) & [TRACE_screening_search2_MS.xlsx](https://osf.io/56fyu)
output:abstract screening inconsistencies: [inconsistencies_human_s2.csv](https://osf.io/jgf9v) &  [inconsistencies_animal_s2.csv](https://osf.io/9z2tj)
**** step 2: identify required full text checks in search 2
input (files with abstract screening feedback): [inconsistencies_animal_s2_SH.csv](https://osf.io/ugyrp) & [inconsistencies_human_s2_SH.csv"](https://osf.io/kve78)
output (uploaded to OSF): [required_full_checks_human_s2.csv](https://osf.io/92mf5) & [required_full_checks_animal_s2.csv](https://osf.io/pwtxq)
**** step 3: get results full text check search 2
input (files with full text screening feedback): [required_full_checks_animal_s2_MS.csv"](https://osf.io/xmua6) & [required_full_checks_human_s2_SH.csv"](https://osf.io/ce35r)
output (uploaded to OSF): [animal_inclusions_s2.csv](https://osf.io/m9rey) & [human_inclusions_s2.csv](https://osf.io/fe5sh)

[Checked_TRACE_screening_search3_MS_EG.xlsx](https://osf.io/famr7)

** Data extraction information
[TRACE data_collection_search1.xlsx](https://osf.io/tqmwb)
[TRACE data_collection_search2.xlsx](https://osf.io/xqpm3)
[TRACE_data_collection_search3.xlsx](https://osf.io/wbrcm)  

** Analyses datasets (NB output analyses scripts)
`TRACEprepared.RData`, `clinical.data.metaregression.RDS`, `clinical.data.explorative.RDS"`, `preclinical.data.metaregression.RDS`, `preclinical.data.explorative.RDS`


-- output: `processed_data/unique_screened_articles.csv`. en `flowchart.tiff`
-- optional output (used to select papers for QA): `Human.PMIDs.QA.S2.csv`, `Animal.PMIDs.QA.S2.csv`. en `Human.PMIDs.QA.S3.csv` en `Animal.PMIDs.QA.S3.csv`



- Characteristics of the included studies via `Descriptives.rmd` (uses `data.explore.rds`, created in `DataDrivenAnalysis.rmd` script)
[input data<-readRDS("processed_data/data.explore.rds")]


