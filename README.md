# **CFAR-SN-Project**
-----------------------
#### **PI**: Dr. Jeffrey Campbell
#### **Author**: Max Schultz
#### **Description**: Codebase for CFAR SN Project 


# Files: 
#### CFAR_code.rmd 
- Main coding file
- Cleaning pipeline: raw RedCap data, deals with repeated measures, extra variables, etc.
- Network preparation: multi-ego & multi-ego with intrahousehold ties
- Contains social network plots and diagrams
- Minor Analysis of social networks

#### color_schemes.r
- Secodary file containing code to assign groupings to data in order to plot
- Assigns color schemes to specific plots
- Allows for generic Igraph plots as well as more complex plots like 'blot' plots
- Sourced in the main file for plotting


#### sn_analysis.rmd
- Main analysis file
- Operational datasets: most up-to-date data from study
- Histograms and violin plots of variables
- SN metrics and various regressions 

# Folders: 
#### Tests
- incorporating new data element (hh_act_mrg)




