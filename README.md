# MMC-CBA

Benefits and costs of five medical male circumcision (MMC) demand creation policies in Mukono, Uganda, using a discrete choice experiment (DCE) to value the benefits.

This is the model behind the working paper:

> Lubinga SJ, Babigumira JB, Graham SM, Mukose AD, Garrison LP Jr, Basu A. Preferences for medical male circumcision services and the benefits and costs of alternative service delivery policies: a discrete choice experiment among uncircumcised men in Uganda. Working paper, updated draft November 2026 (first version April 2018). [PDF](www/working_paper.pdf)

**Live app:** https://theejonslo.shinyapps.io/mmc-cba/

**Status:** illustrative. The preference estimates are from the 2017 DCE; the costs come from published studies in other parts of Uganda; several of the treatment-cost inputs are assumptions.

The benefit of each policy is its compensating variation (CV), the log-sum measure of Small and Rosen, from a conditional logit model calibrated to the observed share of uncircumcised men. The CV is set against the incremental cost of providing the services, including incentive payments, and then against the HIV treatment costs averted by additional circumcisions. The methods are in section 2 of the working paper.

The preferences come from the 2017 DCE with 406 uncircumcised, HIV-negative men aged 18–45 in Ntenjeru sub-county. Two conditional logits were estimated with CMC in 2018, and `estimation/mmc_torch.R` re-estimates both from the choice data in this repository:

| Model | Price coefficients | Log-likelihood | Source |
|---|---|---|---|
| `cl_one_price_slope` (base case) | One for all payments | −3703.301 | CMC run of 26 January 2018 (the January 2018 CV workbook) |
| `cl_three_price_slopes` | Out-of-pocket, voucher and cash payments | −3703.081 | CMC run of 1 May 2018 (Table 2 of the MDM submission) |

The three price coefficients are not different from each other (likelihood ratio test 0.44 on 2 df, p = 0.80), and AIC and BIC both prefer a single price coefficient, which narrows the CV intervals about five-fold.

## Contents

| Path | What it is |
|---|---|
| `data/dce/` | The choice data the conditional logit needs, and nothing else: each man's 12 choices (`choices.csv`) and the attributes of the service options in each block and task (`design.csv`) |
| `estimation/mmc_torch.R` | Estimates the conditional logits (checked against the 2018 CMC runs) and uncorrelated mixed logits from `data/dce/` with R torch |
| `data/cba_inputs.xlsx` | All inputs: settings, service options, policies, costs, both models' estimates and robust covariance matrices, US CPI |
| `data-raw/make_inputs.R` | How the spreadsheet was first built. Edit the spreadsheet, not this script |
| `data-raw/estimates/` | The 2018 CMC estimation outputs, unchanged |
| `R/functions.R` | The model: choice probabilities, calibration, CV, costs, net benefit, Krinsky-Robb, PSA, one-way and scenario analyses |
| `R/data_input.R` | Reads the spreadsheet into one list |
| `R/00_packages.R` | Loads the packages the model uses |
| `main.R` | Runs the base case, Krinsky-Robb, PSA, one-way analyses and scenarios, and writes tables and figures to `output/` |
| `R/mixl.R`, `main_mixl.R` | The benefit-cost analysis with the mixed logits: each man's CV averaged over his posterior distribution, mean and median, with Krinsky-Robb intervals; tables and figures to `output/mixl_*` |
| `paper/` | The working paper: LaTeX source, the script that writes its tables, figures and numbers, and the build script |
| `app.R`, `app/`, `www/` | The Shiny app: page layout and server (`app.R`), helpers, charts and input editors (`app/`), styles, fonts, icon and the working paper PDF (`www/`) |

## The choice data

`data/dce/choices.csv` has one row per man and choice task: `id` (1–406, in order of study ID), `block`, `task` and `choice` (1 = service option 1, 2 = service option 2, 3 = neither). `data/dce/design.csv` has the attributes of the two service options in each of the 36 block–task combinations, coded as in the model (`permhc`, `dist5`, `dist15`, `avail`, `somepriv`, `notpriv`, `device`, `voucher`, `cash`, `price`). Every man in a block saw the same tasks, so the two files together are the full estimation data. No other information about the men is included.

```r
install.packages("torch"); torch::install_torch()   # once
```
```bash
Rscript estimation/mmc_torch.R                     # every model, 1000 draws, seed 1
Rscript estimation/mmc_torch.R models=cl           # the conditional logits only
Rscript estimation/mmc_torch.R draws=3000 seed=1   # mixed logits on more draws
```

The conditional logits reproduce CMC: log-likelihoods of −3703.301 and −3703.081, and estimates and robust standard errors equal to CMC's at its four reported decimals.

The mixed logits have independent normal coefficients, with one price coefficient or three, and the price coefficients either fixed (the same marginal utility of income for every man), lognormal (negative for every man) or censored at zero. Draws, start values and the likelihood follow the MMC-Demand torch code. Each fit writes its estimates, robust covariance matrix and the distribution of each coefficient across men to `estimation/output/`, and is saved in `estimation/output/fits/` so a rerun only fits what is new. The likelihood is written per man, so latent class and correlated mixed logit models are another likelihood function.

## Policies

As was the practice in the study area, the initial state is surgical circumcision at permanent health facilities only. The opt-out (*neither*) is always available.

| Policy | Service options |
|---|---|
| Initial state | Surgery (hospital) |
| Policy 1: outreach services | Surgery (hospital); Outreach (surgery) |
| Policy 2: device circumcision | Surgery (hospital); Device (hospital) |
| Policy 3: outreach and device | Surgery (hospital); Outreach (surgery); Device (hospital) |
| Policy 4: cash incentives | Incentive (device, US$12 cash); Incentive (surgery, US$8 cash) |
| Policy 5: cash incentives and outreach | Incentive (device, US$12 cash); Incentive (surgery, US$8 cash); Outreach (surgery) |

## Results

Base case: one price coefficient, opt-out calibrated to 69.2% uncircumcised in the initial state. Per uncircumcised man, 2017 US$; 95% Krinsky-Robb intervals in brackets.

| Policy | Uptake | CV | Incremental cost | CV − cost | P(CV − cost > 0) | Net benefit, with averted ART costs |
|---|---|---|---|---|---|---|
| Policy 1: outreach services | 43.4% | $7.39 (5.20, 12.07) | $9.83 | −$2.44 (−4.52, 1.89) | 9% | $68.09 (59.87, 78.11) |
| Policy 2: device circumcision | 41.3% | $6.04 (4.30, 9.62) | $5.08 | $0.95 (−0.70, 4.50) | 83% | $59.60 (52.18, 68.49) |
| Policy 3: outreach and device | 50.6% | $12.40 (8.91, 19.78) | $12.85 | −$0.45 (−3.86, 6.85) | 42% | $110.41 (101.21, 122.24) |
| Policy 4: cash incentives | 57.2% | $17.70 (12.76, 28.15) | $16.97 | $0.73 (−4.19, 11.03) | 59% | $148.67 (131.54, 167.76) |
| Policy 5: cash incentives and outreach | 62.4% | $22.44 (16.26, 35.52) | $21.45 | $0.99 (−5.22, 14.14) | 60% | $177.87 (163.31, 195.78) |

- For every policy, the CV was within $2.50 of the incremental cost. The CV of outreach services was lower than their cost in most simulations, mainly because surgery in outreach camps costs nearly twice as much per procedure as at fixed sites.
- Device circumcision had the highest probability of a positive net benefit based on the CV (86% in the PSA); cash incentives with outreach services had the highest net benefit in 49% of PSA draws.
- Each additional circumcision averts about $559 in HIV treatment costs, against $48–78 spent per additional circumcision, so all policies have large positive net benefits once these are included; cash incentives with outreach services is best in every PSA draw.
- Calibration matters most: without it the model predicts that 89% of men would already be circumcised, and the CVs are two to two and a half times higher.

Tables, figures and sensitivity analyses are in the working paper and in `output/`.

## Comparison with the 2018 working paper

The 2018 version reported CL estimates of the CV of $27.25 (outreach), $14.77 (device) and $44.64 (cash), with respect to the out-of-pocket price coefficient and without calibration. Its code (`plots.R`) added the options one at a time to a growing choice set (outreach to the initial state, the device to that, then both cash options), so the device and cash CVs were increments over the preceding policies rather than each policy compared with the initial state, as the text says. This model reproduces them with that sequence ($27.29, $14.80, $44.71; the differences come from the four-decimal estimates). Section 4.3 of the working paper shows what each change since then does to the CV.

## Running it

Install the packages listed at the top of `R/00_packages.R`, then open `MMC-CBA.Rproj` in RStudio and run `main.R`.

The model functions take everything they need as arguments:

```r
inputs <- read_inputs()
base_case <- RunCBA(inputs)                                          # model and price coefficient from the settings sheet
three_slopes <- RunCBA(inputs, "cl_three_price_slopes", "pricecash") # the MDM submission's model, cash price coefficient
kr_res <- RunKR(base_case)                                           # Krinsky-Robb, utility parameters only
TabulateResults(base_case, kr_res)
psa_res <- RunPSA(base_case, genPSA(base_case, nsims = 5000))        # utility parameters and costs
owsa(base_case, "cash_outreach", "nb_cv")                           # one-way analysis for Policy 5
```

The result of `RunCBA()` carries its inputs, so `RunKR()`, `genPSA()`, `RunPSA()` and `owsa()` only need the model object.

## The working paper

`paper/make_paper.R` runs `main.R` and writes the paper's tables (`paper/tables/`), figures (`paper/figures/`) and every number quoted in the text (`paper/numbers.tex`), so the paper always matches the model. The figures need the `svglite` and `rsvg` packages.

```bash
Rscript paper/make_paper.R      # after any change to the model or inputs
bash paper/build.sh mac         # Sabon, Source Sans Pro, Museo Slab and STIX Two from /Library/Fonts; copied to www/working_paper.pdf
bash paper/build.sh draft       # free stand-in fonts (TeX Gyre Pagella, DejaVu), for building without those fonts
```

The published paper, `www/working_paper.pdf`, is the Mac build; the app and this README link to it. The fonts themselves are licensed and are not in this repository, so the Mac build only works where they are installed. The draft build stays at `paper/workingpaper.pdf`, which git ignores.

## The app

The Shiny app runs the same model. You can:

- see each policy's uptake, CV, incremental cost and net benefit, and who would choose which service;
- change any input: the choice model and its price coefficient, the calibration share, provider and treatment costs with their ranges and distributions;
- design your own services from the experiment's attributes, and your own policies from those services;
- run the one-way analysis for any policy as a tornado diagram, and the probabilistic analysis with a benefit-cost plane and the probability each policy has the highest net benefit;
- download your inputs as a spreadsheet and upload them again later;
- read the working paper.

To run it locally, install the packages the model uses plus the app's own:

```r
install.packages(c("shiny", "bslib", "reactable", "plotly", "writexl"))
shiny::runApp()
```

Shiny loads the files in `R/` before `app.R`. The app's fonts are Atkinson Hyperlegible Next and Mono (SIL Open Font License, `www/fonts/OFL.txt`).

## License

MIT (`LICENSE`).
