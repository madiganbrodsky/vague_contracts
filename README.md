# An experimental template for assessing lay interpretation of vague contract language 

This repository contains example code for creating experiments modeled after those reported by Lawrence Solan, Terri Rosenblatt and Daniel Osherson in their 2012 paper *False Consensus Bias in Contract Interpretation* (available at https://brooklynworks.brooklaw.edu/faculty/276/). 

## To demo the experiment 

Simply open https://bwaldon.github.io/vague-contracts/index.html in a compatible browser. (Chrome works best). 

## To modify the experiment stimuli and run your own experiments:

0. Pull the repo onto your local machine. 

1. Open `stimuli.csv` and note the column labels `item,version,header,continuation`. Stimuli must be entered in this format in order to be read by the experiment script. The defaul stimuli in this file correspond to the stimuli used by Solan et al. in their original study. The column `version` corresponds to the condition of the experiment - you can provide any label that makes sense for the purposes of your experiment (to replicate Solan et al.'s study, we use the version labels `insurance`, `exclusion`, `unambiguously_true`, and `unambiguously_false`). The column `item` corresponds to the vague language of interest and is also read by the experiment script to render the interpretation question to the participant, i.e. see line 48 of `js/index.js`: 

`$("#question").html('<i>1. Do you think that the damage was caused by ' + this.item + '?</i>');`

2. After creating your own stimuli (maybe just one suffices for the purposes of a live demo), you can run the R script (`csvtojson.r`) to convert the .csv file into .json. (If you're not familiar with R, it should be trivial to use another programming language - e.g. Python - to accomplish the same task). This output is saved as `stimuli.json`. 

3. The contents of `stimuli.json` need to be copy and pasted into `js/stimuli.js`, such that `js/stimuli.js` should read as follows: 

`var stimuli = THE-CONTENTS-OF-stimuli.json` 

4. Test the experiment locally by opening `index.html` in a browser. This should suffice for running a demo locally; just open `index.html` on Chrome and share screen with your audience.  

5. (Optional). Push your changes to GitHub so that your modified experiment is viewable from the link above. 


 
