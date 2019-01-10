This folder contains all the data reported in Poppels & Kehler (2019, Glossa).
For each of the 4 experiments the paper reports on, there is a file containing
the raw results as downloaded from Ibex, as well as an RDS file containing the
result of applying the relevant pre-processing script, which can be found in
../data-analysis/. Additionally, there is a separate RDS file with excluded data
for those experiments that had at least one self-reported non-native speaker of
English.

Raw data files are exactly as downloaded from Ibex, except that participants
hash IPs have been replaced with "Participant_IP_XX" using 'anonymize-data.R'.
