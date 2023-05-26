
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

Ciao

This is the companion repository for the paper (Vossen et al. 2023) we
submitted to the *2023 Integrated Communication, Navigation and
Surveillance Conference (ICNS)*.

Our work uses GraphWave (Donnat et al. 2018) to analyse the European
Aviation network and to highlight the structural impacts on it by the
COVID-19 pandemic.

You can find here

- a subset of the data, six weeks of flight list data in 2019, 2020,
  2021, and 2022
- the code to reproduce the model for the paper (and the provided data
  subset)
- the validation paper of the used model

## Setup

We suggest you to create a dedicated conda environment for this project,
we refer to it as `aviation-network` in the following paragraphs.

Note that we use Python 3.9 even if the GraphWave repo states it was
developed in 2.7 (and we define do NOT use version 2.1 of `networkx` as
per the [GraphWave
repo](https://github.com/snap-stanford/graphwave/blob/master/requirements.txt)).
So to make `graphwave` import-able we have to fix the print statements
in `graphwave/utils/graph_tools.py` from `print "message"` to
`print("message")`

``` shell
$ conda create -n aviation-network python=3.9 \
    numpy networkx pandas scikit-learn pillow=9.0.0 \
    matplotlib seaborn pyemd pygsp
```

Clone (or copy the relevant folder) [`graphwave` source
code](https://github.com/snap-stanford/graphwave) in the parent
directory of this project.

``` text
<somewhere>
├── aviation-network-structure-model
│   ├── data
│   │   ├── ...
│   │   └── sample_2022.csv
│   └── sample_code_for_repo.R
│   └── ...
└── graphwave
    ├── graphwave
    │   └── ...
    └── requirements.txt
```

## Usage

You can run the script `sample_code_for_repo.R` to reproduce the
analysis of the paper on the sample data.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-donnat2018" class="csl-entry">

Donnat, Claire, Marinka Zitnik, David Hallac, and Jure Leskovec. 2018.
“Learning Structural Node Embeddings via Diffusion Wavelets.” In
*International ACM Conference on Knowledge Discovery and Data Mining
(KDD)*. Vol. 24.

</div>

<div id="ref-vossen2023" class="csl-entry">

Vossen, Juul, Enrico Spinielli, Quinten Goens, and Rainer Koelle. 2023.
“Studying the Impact of COVID-19 on the European Air Transportation
Network.” In *2023 Integrated Communication, Navigation and Surveillance
Conference (ICNS)*, 1–12. Herndon, VA, USA.
<https://doi.org/10.1109/ICNS58246.2023.10124296>.

</div>

</div>
