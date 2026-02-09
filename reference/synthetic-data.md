# Generate Synthetic ALprekDB Data for Demonstrations

These functions create realistic synthetic datasets that mirror the
structure and types of real ADECE Pre-K administrative data. They are
useful for demonstrations, vignettes, and testing without requiring
access to confidential data files.

All three generators share classroom codes when called with the same
`seed` and compatible `n_classrooms`, enabling cross-module linkage.
