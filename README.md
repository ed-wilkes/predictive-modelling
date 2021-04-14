# predictive-modelling
Various useful functions for predictive modelling in R.

This respository is centred around **performNestedCVParallel()**, which fits and evaluates a chosen machine learning model to the input data via a nested k-fold cross-validation process. The additional functions are designed to manipulate the output of this function. For more information on the nested CV process, see Wilkes *et al.* (2018) *Clin Chem*. 

**ClinChem_2020-317479_Supplemental_Analysis_Guide.R** provides a walkthrough of the functions on example data - see Wilkes *et al.* (2020) *Clin Chem* for more details.

# Citation
If you find the functions within this repo useful for your own work, please consider citing the associated publications:

`@article{wilkes2018clinchem,
    title={Using Machine Learning to Aid the Interpretation of Urine Steroid Profiles},
    author={Wilkes, Edmund H and Rumsby, Gill and Woodward, Gary M},
    journal={Clinical Chemistry},
    publisher={Oxford Academic},
    year={2018}
}`

`@article{wilkes2020clinchem,
    title={A Machine Learning Approach for the Automated Interpretation of Plasma Amino Acid Profiles},
    author={Wilkes, Edmund H and Mozley, Erin and Beltran, Luisa and Woodward, Gary M and Carling, Rachel S},
    journal={Clinical Chemistry},
    publisher={Oxford Academic},
    year={2020}
}`
