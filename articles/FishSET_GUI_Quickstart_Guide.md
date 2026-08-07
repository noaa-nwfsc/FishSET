# FishSET GUI Quickstart Guide

#  FishSET GUI Quickstart Guide

FishSET is a set of statistical programming and data management tools
developed to improve fishery modeling. The tools standardize data
management and organization, enable the use of location choice models to
provide input into fishery management, and provide various other
modeling and visualization tools. The FishSET toolbox is provided as a
set of R functions that can be run in an R console or here in this
FishSET Graphical User Interface (FishSET GUI). This guide gives new
users a practical path through the FishSET GUI.

**Recommended path:** Upload Data -\> Select variables -\> QAQC -\>
Explore the data -\> Format Data -\> Modeling -\> Policy

This document includes basic instructions on how to use the FishSET GUI.
Refer to the [FishSET User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)
for information on installing R software and to the [main
page](https://noaa-nwfsc.github.io/FishSET/index.html) of this website
for FishSET installation instructions.

###  What the GUI does

The FishSET GUI is a user-friendly interface that guides users through
the steps necessary to run discrete choice models. The FishSET GUI
requires no knowledge of coding or the R programming language as all
steps to prepare and run discrete choice models can be run in a
graphical user interface.

###  Before you start

- Have your main and spatial data files ready. Port, gridded, and
  auxiliary files are optional.
- Choose a FishSETFolder where project files can be saved.
- For mapping or policy tabs, make sure your spatial data contain the
  required zone information.

###  What gets saved

- Project data and edits are stored in a local SQLite database on the
  user's computer (data not shared with other users or FishSET
  developers) known as the FishSET database.
- The database is housed in the project's directory within the FishSET R
  package directory (FishSETFolder).
- Plots and tables are written to the project output folder.
- Function calls and input values are written to the project logs.
- The report template is stored in the project `doc` folder.

**Open the GUI:** run
[`library(FishSET)`](https://github.com/noaa-nwfsc/FishSET) and then
[`run_fishset_gui()`](../reference/run_fishset_gui.md) in R or RStudio.
The main and spatial data tables are required; supporting files depend
on your workflow.

## Quick path through the GUI

*Click any step below to expand the instructions.*

Information across all tabs

All tabs include controls to close the GUI, manage database tables,
record notes, and run an R expression. Within the GUI, the primary data
frame is called `values$dataset`.

Upload Data

#### Load Files

1.  Click **Change FishSET Folder** to set where project files will be
    stored.
2.  Type a new project name or select an existing project and select it
    from the dropdown.
3.  Provide the primary data; port and auxiliary data are optional. If
    loading an existing project, select tables from the dropdown menus;
    otherwise, upload your files.
4.  Provide your spatial files. You can upload a standard spatial file
    (CSV, GeoJSON), or check the "Uploading shape file instead?" box to
    upload a multi-file shapefile. Gridded data is optional. (See
    [Chapter 4 of the FishSET User
    Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)
    for more information on the different types of data.)
5.  Click **Load data**.

**Tip:** Confidentiality settings, log reset, table management, and full
data refresh are also available in this tab. Refreshing restores the
original loaded table unless another table is selected on the **Upload
Data** tab.

#### Select Variables

Use the Main and Spatial data sections to assign IDs, coordinates, and
dates, then click **Save selected variables**. Use the **Create ID**
buttons if trip, haul, or zone IDs were not included in your uploaded
dataset. These selections are saved to the database and only need to be
selected when starting a new project.

QAQC

#### Data Quality Checks

Use the sidebar to check for missing values, empty variables, duplicate
data, incorrect classes, outliers, and invalid spatial coordinates. Use
**Preview data** and **Summary table** before applying corrections.

See the recommended data quality checks in [Chapter 4 of the FishSET
User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)

#### Explore the Data

Switch between tables and temporal, spatial, and correlation plots to
understand the loaded data. Generated plots and tables are saved to the
project output folder.

For more details see [Chapter 6 of the FishSET User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)

Format Data

#### Compute New Variables

Use the radio buttons in the left sidebar to access various tools for
creating new data columns. You can apply custom R expressions, calculate
spatial metrics (like trip distances or centroids), aggregate your data
(such as collapsing haul-level data into trip-level data), or generate
group-level statistics (like quantiles, percentages, and cumulative
sums). Follow the on-screen prompts for each selected tool to compute
and save your new variables to the dataset.

#### Define Alternative Fishing Choices

Use this tab to define how the distance matrix between observed and
alternative fishing locations should be calculated for your choice
model.

1.  Set your **Starting location** (Port, Lagged haul, or Coordinates).
2.  Set your inclusion threshold (the minimum number of observations
    needed for a zone to be included as an alternative).
3.  Type a unique name for your matrix and click **Create alternative
    location matrix** to generate and save it to your project database.

#### Create Expected Catch Matrix

Use this tab to estimate the expected catch (or revenue) for the
alternative fishing zones you defined in the previous step (zones where
fishing could have happened but did not).

1.  Under **Core Inputs**, name your new matrix, select your catch
    variable, and link it to your saved Alternative Matrix (select a
    price variable as well if you want to calculate revenue).
2.  Adjust the **Temporal Settings**, **Grouping & Calculation**, and
    **Data Handling** sections to define how the moving averages should
    be calculated and how empty data should be treated.
3.  Click **Create Expectations** to calculate and save the matrix to
    your project database.

#### Format Model Data

Reshape project data into the long format required for modeling.

1.  Under **Core Inputs**, provide a unique name for this dataset,
    select which variables to retain, and select the Alternative Choice
    and Expectations matrices you created in the previous steps.
2.  Under **External Data Integration**, you can optionally join
    auxiliary or gridded datasets to your model data.
3.  Under **Configuration**, select how to handle missing data
    (imputation) and whether to calculate spatial distances between
    observations and zones.
4.  Click **Run Format Data** to process and save the formatted dataset
    to your project.

**See [Chapter 8 of the FishSET User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)
for more information on defining alternative choices, how expected
catch/revenue is calculated, and model development.**

Modeling

#### Model Design

Use this tab to construct the design matrices (the mathematical
structures) required to actually run your discrete choice models.

1.  Under **Source Data & Naming**, provide a unique name for this
    design, select your newly formatted dataset from the dropdown, and
    choose your Model Type (Standard Logit or Expected Profit Model).
2.  Under **Model Specification**, type in your Utility Formula (e.g.,
    `chosen ~ catch + distance | vessel_length`). You can also check the
    box to automatically scale your numeric covariates for better model
    stability. If you selected the Expected Profit Model, you will also
    need to provide a Catch Formula and select a Price Variable.
3.  Click **Create Design Object** to generate and save the matrices to
    your project.

#### Model Fit

Select a saved model design, provide an optional fit name, choose an EPM
distribution when required, and click **Fit Model**. Use the model
comparison controls to review saved fits side by side.

#### Model Cross Validation

Use this tab to perform k-fold cross-validation to assess the
out-of-sample predictive performance of your model. Cross-validation can
take a few minutes to run depending on your fold count. Once finished,
scroll down to review the Overall Out-of-Sample Performance summary, as
well as the detailed accuracy metrics and estimated coefficients broken
down by each individual fold.

**See [Chapter 8 of the FishSET User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)
for more information on model development.**

Policy

#### Zone Closure

Use this tab to create management scenarios where specific fishing zones
are closed or have restricted catch limits.

1.  Select an **Alternative Matrix** from the dropdown to load your
    valid fishing zones onto the interactive map.
2.  Click directly on the map to select the specific zones you want to
    include in your closure scenario.
3.  In the **Allowable TAC by Zone** table, adjust the total allowable
    catch percentage (0-100%) for each zone you selected.
4.  Enter a unique **Scenario Name** and click **Add closure** to save
    the scenario to your project database.

#### Run Policy Simulations

Use this tab to simulate redistributed fishing effort and calculate
welfare changes following implementation of the policy scenarios.

1.  Under the first column, select your baseline fitted **Model** and
    choose one or more **Closure Scenarios** to apply (or leave this
    blank to run a baseline simulation).
2.  Under the second column, set your **Simulation Draws**. If you are
    using a Standard Logit model, you must also select your **Marginal
    Utility of Income (MUI)** variable and check the box if it
    represents a cost. (This option is automatically hidden if you are
    using an Expected Profit Model).
3.  Click **Run Simulation** to calculate the expected welfare and save
    the results to your project database.

#### Summarize Effort Redistribution

Extract and visualize how fishing effort shifts geographically under
policy simulations.

#### Summarize Welfare Impacts

Evaluate expected welfare changes or compensating variation resulting
from policy simulations.

**For more information on simulating policy scenarios see [Chapter 9 of
the FishSET User
Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html).**

## Need more help?

-  [FishSET User
  Manual](https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html)
-  Questions: <nmfs.fishset@noaa.gov>
