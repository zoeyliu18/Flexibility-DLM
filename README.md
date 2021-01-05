# Flexibility-DLM
Code and data for paper "The Crosslinguistic Relationship between Ordering Flexibility and Dependency Length Minimization: A Data-Driven Approach"

1. **Take data from the Universal Dependencies project**

1. **Extract instances and calculate flexibility score for each language**
   1. ```python3 codes/flexibility.py --input INPUT_TO_UD_DATA --output data/```

1. **Generate data for regression**
   1. ```python3 codes/data-process.py --input data/ --output data/```
   
1. **Run regression**
   1. ```python3 codes/lr.py --input data/ --output data/```

1. **Collect coefficeints from each language (plot/corr.csv)**

1. **Run analysis to test relationship between flexibility and DLM, as well as draw graphs (codes/analysis.R)**
   
