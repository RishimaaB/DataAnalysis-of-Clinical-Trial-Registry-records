SELECT * FROM imp_info_filtered i 
LEFT JOIN Trial_india t
ON i.Serial_no = t.'TRIAL.ID' 
LEFT JOIN ethics_india e 
ON i.Serial_no = e.'TRIAL.ID' 
LEFT JOIN primary_outcome_india p
ON i.Serial_no = p.'TRIAL.ID'
LEFT JOIN collaborator_india c
ON i.Serial_no = c.'TRIAL.ID'
LEFT JOIN funding_source_india f
ON i.Serial_no = f.'TRIAL.ID'
LEFT JOIN secondary_outcome_india so
ON i.Serial_no = so.'TRIAL.ID'
LEFT JOIN contacts_india ci
ON i.Serial_no = ci.'TRIAL.ID'
LEFT JOIN secondary_sponsor_india ss
ON i.Serial_no = ss.'TRIAL.ID';

