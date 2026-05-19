# Generate Synthetic ALprekDB Data for Demonstrations

These functions create synthetic datasets that mirror the structure and
types of real ADECE Pre-K administrative data without using real
row-level values. Classroom identifiers use fake `9xx` county prefixes
and `9xxxxx` program codes so public examples cannot be confused with
confidential source records.

All three generators share classroom codes when called with the same
`seed` and compatible `n_classrooms`, enabling cross-module linkage.
