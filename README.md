# bilingualboundary
This is the repository for all code linked to the bilingual boundary eye tracking experiment with Davide Crepaldi and Irina Elgort, written as part of Anna Liebelt's PhD thesis project (project acronym: BBET).

For the OSF pre-registration: <a href="https://osf.io/hpmtu">OSF</a>

- [bilingualboundary](#bilingualboundary)
- [Running the experiment](#running-the-experiment)
  - [Experimental script](#experimental-script)
  - [Stimuli files](#stimuli-files)
  - [Data files](#data-files)
- [Analysing data](#analysing-data)

# Running the experiment
## Experimental script
The `main.py` script is Python code using PsychoPy version 2024.2.3, and is intended to be used with an EyeLink 1000 Plus eye tracker. It needs PyLink and `EyeLinkCoreGraphicsPsychoPy.py` to run. It has been adapted from the `main - JonCarr.py` script by Jon Carr.

## Stimuli files
**.\images** contains some images created mainly for participant instruction during the experiment.

Additionally, the `nonword_generation.py` file was used to create options for the nonword previews in the experiment.

Most importantly, the **.\stimuli** folder contains stimuli files to run the experiment. The majority of files in this folder contain materials used to create the final stimuli set for the experiment, and are therefore non-crucial. The files named `version1.csv`, `version2.csv`, `version3.csv`, and `version4.csv` are imported by the experimental script to select the sentences to show participants. These are split in four different versions to counterbalance which preview-target combinations are seen by the subject.

## Data files
The .\data folder contains folders containing the data files for each participant. At time of writing, these have been transfered elsewhere due to OneDrive space constraints.

# Analysing data
The `BBET_analysis.py` file runs the pre-processing of the eye tracking data. It takes ASCII files as input, and outputs the `all_scores.csv` and  `all_ET.csv` files in **.\data**, which list participants' behavioural scores and eye tracking data, respectively.