# Impact_alien_species_Reunion_Island

The present digital archive is the outcome of the paper: "Assessing the ecological impacts of alien plants in the subalpine and rainforest vegetation of Reunion Island".


Biological invasions are a major driver of biodiversity change, and understanding how alien plants impact native species at the local scale is essential for conservation prioritisation and management. However, the ecological impacts of many alien plant species remain poorly documented, particularly on islands. Large vegetation datasets are increasingly available, primarily designed for monitoring or management objectives, yet their potential for quantifying ecological impacts has only partially been explored. This study aimed to assess the impacts of alien species on native population size and community composition in two major vegetation types on Reunion Island: rainforest and subalpine vegetation.

Impacts were assessed using complementary approaches to better capture the range of responses observed across alien species by using existing vegetation surveys. Comparisons between invaded and non-invaded plots, regression analyses of alien species cover (i.e., independent variables) on native species cover and richness (i.e., dependent variables), and multivariate analyses of community composition were conducted. Decline in native species cover was used as an indicator for native population size, and decline in native species richness as an indicator for local population extinction.
 
Alien species significantly reduced native cover and/or richness in both vegetation types, although impacts varied strongly among species. In rainforest, several woody alien species caused declines in native cover and richness at their high abundance. In subalpine vegetation, most alien herbaceous species showed weak or no detectable effects, but a few species caused declines in native cover and richness at high abundance. 

Our findings show the potential of large-scale observational datasets for ecological impact assessments. We showed that even in areas with relatively low levels of invasion, high alien plant cover is associated with measurable decline in native communities. Local-scale assessments provide critical evidence for management prioritisation, complementing global assessments and supporting more effective conservation strategies.


The present repository contains data, and R codes necessary for conducting the analyses on impact of alien species in Reunion Island, as presented in the main text. It includes detailed steps for assessing the impact on native population size, using deux different methods: comparison of native cover in non-invaded and invaded plots (01_paired_plot), and relationship between native and alien species cover (02_glmm_cover); and on native species richness: relationship between the presence of each native and alien species (03_RDA) and relationship between native species richness and alien species cover (04_glmm_native_sp).
The R codes are organised into four files corresponding to each method. 

-	01_paired_plot file is organised into four R scripts that source each other to perform different data processing and analysis steps
-	02_glmm_cover file is organised into three R scripts that source each other to perform different data processing and analysis steps
-	03_RDA file is organised into four R scripts that source each other to perform different data processing and analysis steps
-	04_glmm_native_sp03_RDA file is organised into four R scripts that source each other to perform different data processing and analysis steps

Below is a description of each document within the three designated folders: data, and codes

### DATA ###
-	"01_di_simplified_rel.csv": Raw data from the simplified version of the standardised systematic sampling protocol to measure invasion degree, which include global information at the plot level, such as the total cover of alien and native species per stratum, from Boulesnane-Guengant et al., (2026) 
-	"02_di_simplified_sp.csv": Raw data from the simplified version of the standardised systematic sampling protocol to measure invasion degree, which included cover of each alien species per plot and stratum from Boulesnane-Guengant et al., (2026)
-	"02_di_complete_rel": Raw data from the complete version of the standardised systematic sampling protocol to measure invasion degree, which include global information at the plot level, such as total cover of alien species and native species richness per plot and strata, from Boulesnane-Guengant et al., (2026)
-	"02_di_complete_sp.csv": Raw data from the complete version of the standardised systematic sampling protocol to measure invasion degree, which included cover of each alien species and list of each native species per plot and stratum from Boulesnane-Guengant et al., (2026) 
 
### CODE ###
#### code/00_prepa_data ####
##### *01_prepa_data_for_analyses.R*
- __Objective__: Prepare the data from the simplified version of the standardised systematic sampling protocol.
-	__Outcomes__: One dataframe used in “code/00_prepa_data/02_nb_plot_species.R”; “code/01_paired_plot/02_prepa_data_paired_plot.R”; "code/01_paired_plot/03_prepa_data_paired_plot_strata.R"; “code/01_paired_plot/04_prepa_verif_paired_plot.R” “code/01_paired_plot/06_analyse_paired_plot_strata.R”; and “code/02_glmm_cover/01_prepa_data_glmm_strata.R”;
    -	*data_analyse*: data combining information from “01_di_simplified_rel.csv" and "02_di_simplified_sp.csv", on a long format

##### *02_nb_plots_species.R* 
-	__Objective__: calculate the number of plots per species from the simplified version of the standardised systematic sampling protocol
-	__Outcomes__: One dataframe
    -	*Nb_plot*: number of plots and frequence of the presence of each alien species in both vegetation type

#### code/01_paired_plot/ ####
##### *01_function_for_paired_plot.R*
-	__Objective__: Create function for the paired plot analyses: one function to create paired plots and one function for the verification of paired plot
-	__Outcomes__: Two functions used in "code/01_paired_plot/02_prepa_data_paired_plot.R"
    -	*fct_paired_plot*: function to create paired plots;
    -	*fct_effet_verif*: function to the verification of paired plot

##### *02_prepa_data_paired_plot.R* 
-	__Objective__: Preparation of the paired plot data for each species, per site. 
-	__Outcomes__: One dataframe, used in “code/01_paired_plot/03_prepa_data_paired_plot_strata.R”; “code/01_paired_plot/04_prepa_verif_paired_plot.R”
    -	*data_pp*: paired plot data per alien species (have to be dominant within the invaded plots), saved as Rdata in “code/01_paired_plot/02_prepa_data_paired_plot.RData”

##### *03_prepa_data_paired_plot_strata.R*
-	__Objective__: Preparation of the paired plot data for each species and stratum, per site, for the comparison of native cover in non-invaded and invaded plots analyses
-	__Outcomes__: One dataframe used in “code/01_paired_plot/04_analyse_paired_plot_strate.R”
    -	*data_pp_strata*: paired plot data per alien species for each stratum

##### *04_prepa_verif_paired_plot.R*
-	__Objective__: Data preparation for the verification of the assumption that native species cover in non invaded plot is independent to alien target species cover in invaded plot, used in “code/01_paired_plot/05_verif_paired_plot.R”
-	__Outcomes__: list and dataframe
    -	*model_lm_list*: List of lmer models per alien species paired plot
    -	*list_pp*: list of all paired plot per species
    -	*full_names*: dataframe with the name alien species and it reference code

##### *05_verif_paired_plot.R*
-	__Objective__: Verification of the assumption that native species cover in non invaded plot is independent to alien target species cover in invaded plot
-	__Outcomes__: 
    -	*p*: graphical representation of native species cover along with cover of alien species in invaded and non-invaded plots.
    -	*Table*: Result of the emmeas analyses

##### *06_analyse_paired_plot_strata.R*
-	__Objective__: Results of the paired plot analyses per species for each stratum
-	__Outcomes__: 
    - *table_wilco*: analysis table of the Wilcoxon signed-rank two-sided test
    - *list_plot*: Graphical representation of the native species cover in non-invaded and invaded plot by each alien species, per strata

#### code/02_glmm_cover ####
##### *01_prepa_data_glmm_strata.R*
-	__Objective__: preparation of the data to turn regression analyses on native population size
-	__Outcomes__: one dataframe used in “code/02_glmm_cover/02_analyses_regression_rainforest.R”; “code/02_glmm_cover/03_analyses_regression_subalpine.R”
    -	*data_all*: dataframe with all the variables need to turn regression analyses on native population size, per alien species

##### *02_analyses_regression_rainforest.R*
-	__Objective__: Regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the rainforest vegetation
-	__Outcomes__: Results of the generalised linear mixed models (glmm) analyses
    -	*mes_mods*: list of the glmm model of each alien species
    -	*coef_df*: coefficient results of each model

##### *03_analyses_regression_subalpine.R*
-	__Objective__: Regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the subalpine vegetation
-	__Outcomes__: Results of the generalised linear mixed models (glmm) analyses
    -	*mes_mods*: list of the glmm model of each alien species
    -	*coef_df*: coefficient results of each model

#### code/03_RDA ####
##### *01_prepa_RDA_sterose.R*
-	__Objective__: data preparation for the RDA analyses in order to evaluated the global impact of alien species on species composition in the rainforest vegetation, used in: “code/03_RDA/03_RDA_sterose.R”
-	__Outcomes__:  two dataframes:
    -	*data_alien*: matrix of alien species presence-absence in each plot and strata, with habitat and elevation
    -	*data_native*: matrix of native species presence-absence in each plot and strata

##### *02_prepa_RDA_volcan.R*
-	__Objective__: data preparation for the RDA analyses in order to evaluated the impact of alien species on species composition in the subalpine vegetation, used in: “code/03_RDA/04_RDA_volcan.R”
-	__Outcomes__:  two dataframes:
    -	*data_alien*: matrix of alien species presence-absence in each plot and strata, with habitat and elevation
    -	*data_native*: matrix of native species presence-absence in each plot and strata

##### *03_RDA_sterose.R*
-	__Objective__: Assessing the impact of alien species on native composition in the rainforest vegetation
-	__Outcomes__: results of the RDA analyses and graphical representation

##### *04_RDA_volcan.R* 
-	__Objective__: Assessing the impact of alien species on native composition in the subalpine vegetation
-	__Outcomes__: results of the RDA analyses and graphical representation

#### code/04_glmm_native_sp ####
##### *01_prepa_data_glmm_native.R*
-	__Objective__: preparation of the data to turn regression analyses on native species richness used in: “code/04_glmm_native_sp/02_glmm_native_richness.R”
-	__Outcomes__: one dataframe 
    -	*data_all*: data combining information from “01_di_simplified_rel.csv" and "02_di_simplified_sp.csv", 

##### *02_glmm_native_richness.R*
-	__Objective__: Regression analyses to evaluate the relationship between native species richness and alien species cover in each stratum in the rainforest and subalpine vegetations
-	__Outcomes__: Results of the generalised linear mixed models (glmm) analyses
    -	*mes_mods*: list of the glmm model of each alien species
    -	*coef_df*: coefficient results of each model

### RESULTS ###
The different results produced in the 16 codes are presented above:

#### results/00_prepa_data ####
-	" 01_nb_plots_species_complete.csv":
The number of plots (n_releve) per alien species (name_sp) with their frequencies of occurence (freq_presence) from the complete version were used to generate Appendix S3. It have been produced using the script titled “02_nb_plot_species.R” in “code/00_prepa_data”.

-	" 01_nb_plots_species_simplified.csv":
The number of plots (n_releve) per alien species (name_sp) with their frequencies of occurence (freq_presence) from the simplified version were used to generate Appendix S3. It have been produced using the script titled “02_nb_plot_species.R” in “code/00_prepa_data”.

#### results/01_paired_plot ####
- "nb_pp_rainforest.csv"
Number of paired plot per species in the rainforest vegetation and producing using the script titled "06_analyse_paired_plot_strata.R" in "code/01_paired_plot".

- "nb_pp_subalpine.csv"
Number of paired plot per species in the subalpine vegetation and producing using the script titled "06_analyse_paired_plot_strata.R" in "code/01_paired_plot".

-	“rainforest_boxplot_pp.csv”
Result of the comparison of native cover in non-invaded and invaded plots analyses (paired plot method) per alien species present in the rainforest vegetation, produced using the script titled “06_analyses_paired_plot_strata.R” in “code/01_paired_plot”.

-	“subalpine_boxplot_pp.csv”
Result of the comparison of native cover in non-invaded and invaded plots analyses (paired plot method) per alien species present in the subalpine vegetation, produced using the script titled “06_analyses_paired_plot_strata.R” in “code/01_paired_plot”.

Both of these results were used to produced Table 2 presented in the main text and Appendix S4.

#### results/02_glmm_cover ####
-	“rainforest_results_models_native_cover.csv”: Result of the regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the rainforest vegetation, produced using the script titled “03_analyses_regression_rainforest.R” in “code/02_glmm_cover”.

-	“subalpine_results_models_native_cover.csv”: Result of the regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the subalpine vegetation, produced using the script titled “03_analyses_regression_subalpine.R” in “code/02_glmm_cover”.

Both of these data were used to produced Table 2 presented in the main text and Appendix S5.

#### results/03_RDA #### 
-	“rainforest_RDA_plot.png”: Results of the RDA analyses to evaluate the impact of alien species on native composition in the rainforest vegetation, produced using the script titled “04_RDA_volcan.R” in “code/03_RDA”.

-	“subalpine_RDA_plot.png”: Results of the RDA analyses to evaluate the impact of alien species on native composition in the subalpine vegetation, produced using the script titled “03_RDA_sterose.R” in “code/03_RDA”.

Both of these data were used to produced Appendix S6.

#### results/04_glmm_native_sp ####
-	“results_models_native_richness.csv”: Result of the regression analyses to evaluate the relationship between native species richness and alien species cover in each stratum in the rainforest and subalpine vegetation, produced using the script titled “02_aglmm_native_richness.R” in “code/04_glmm_native_sp”.

These results were used to produced Table 3 presented in the main text and Appendix S5.



