#### Purpose:
This **HCC Calculator** estimates the risk of developing hepatocellular carcinoma in HCV patients treated with DAA. It leverages longitudinal data of serum biomarkers, including alpha-fetoprotein (AFP), total bilirubin, direct bilirubin, alanine transaminase (ALT), aspartate aminotransferase (AST), cholinesterase, alkaline phosphatase (ALP), gamma-glutamyl transferase (GGT), total protein, and albumin.

#### Calculator Options:
1. **AFP-only Calculator:** Uses only AFP longitudinal data. Ideal for cases with limited data.
2. **Comprehensive Calculator:** Incorporates all 10 biomarkers, as well as baseline age and gender, for a more accurate prediction. Ensure that all biomarker values are provided for each measurement; missing values will cause errors. 

#### Instructions:
1. **Download** the sample CSV to understand the required data format.
2. **Inputting** the patient's ID, measurement time, and corresponding serum biomarker values into the CSV.
3. **Uploading** the filled CSV.
4. **Specifying** a time interval from baseline as the prediction window.

#### Important Notes:
1. For the comprehensive calculator, ensure consistent gender and baseline age values across all measurement time points. Use "1" for male and "2" for female.
2. The time of each measurement and the prediction window should be provided in **days**. 
3. The prediction window should be longer **than** the most recent measurement for any patient.



