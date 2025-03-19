# Intro to Statistics with R

This website [https://r4statistics.com/](https://r4statistics.com/) hosts some teaching material on **“Introduction to Statistics & Machine Learning with R”** — an intensive workshop that includes both Lectures and Practice Sessions to teach & consolidate the concepts learned using programming. 

+ Originally designed for a summer workshop offered in July of 2024 to a group of PhD students from various EU universities specializing in life sciences ---see [https://R4biostats.com/](https://R4biostats.com/). 


## TO DO

+ ✅ Must do manual 
  + ~~\*.PPTX 2 \*.PDF LOOSES THE EQUATIONS!!!!!~~

+ ✅  ultimare `practice/practice_slides/slides_lab05.qmd` + practice/RCODE 5 
  + ~~aggiungere decision trees??? NO (dopo!)~~
  + ~~K-MEANS CLUSTERING ? NO (dopo!)~~
  + ~~logistic multiple variables ? NO (too much!)~~

+ ️🔳 rivedere `practice/practice_slides/slides_lab06.qmd` &&&  verificare file `practice/Rcode/lab06_code.R`
  +️ 🖍️🖍️OKKIO che ho fatto nuova funzione che pass da slide a R code `R_resources/extract_r_code.sh`

+ ️🔳 `practice/practice_slides/slides_lab05.qmd`-->> standalone lesson on logistic regression for my website

+ ️🔳 ripulire un po' di cose extra (lez e labs)

### Extra TO DO

+ ️◽ **[add `renv`]** bc this is going to last 
+ ️◽ turn re-usable practice slides code into `tidyeval` functions (plot etc) to be able to replace with different datasets 

## How this website was built

This is a static website built with [Quarto](https://quarto.org/), shared on a Github [repo](https://github.com/Lulliter/R4biostats) and served via [Github Pages](https://docs.github.com/en/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) ~~rendered at this link [https://lulliter.github.io/R4stats/](https://lulliter.github.io/R4stats/), or~~ now (after buying a "custom domain") deployed over a custom domain registered with AWS Route 53: [https://r4statistics.com/](https://r4statistics.com/).

The main content of the website pages is contained in *Quarto Markdown* files (`*.qmd`). `*.qmd` files are very similar to regular Markdown (`*.md`) and R Markdown (`*.rmd`) files, except they are designed to be language agnostic.

+ `Google Analytics` Account under property r4biostats.com
 + Nome stream: r4statistics.com
 + URL dello stream: https://r4statistics.com
 + ID stream: 10407079869
 + ID misurazione: G-TDWG2LL0S7


## Things needed to build the site

1. Install **Quarto** (an open source tool that can be used from RStudio, Jupyter, CLI, etc.)... Most R users will use RStudio
2. Install **git** (distributed version control software)... here some [instructions](https://github.com/git-guides)
3. Create a **Github** account
4. Follow instructions/examples available [here](https://quarto.org/docs/websites/)...
5. Font-Awesome Icons downloaded in `./images/*` from [Font-Awsome GH repo /svgs](https://github.com/FortAwesome/Font-Awesome/tree/6.x/svgs). To use in:

- `*.md`
  - ~~`![fa-crown](images/copyright-regular.svg)`~~ (no size spec!!!)
  - `<img src="images/copyright-regular.svg" width="16" height="16">` (yes size spec!!!)
- `*.qmd`
  - quarto wrap `{{< fa brands copyright > }}`
  - r inline code `r fontawesome::fa("fab fa-windows", fill = "steelblue")`

6. (An AWS account ...) but only for the custom domain part


# Quirks 
Little things to keep in mind (because semi-automated): 

1. ~~I execute a script `R_resources/slides_fromDrive_2_here.R` from my shell file `___render_deploy.sh` to copy the `*.pptx` files from [GoogleDrive](https://drive.google.com/drive/folders/1mkits-PaRC8SGnDuMn2h_bKHAVTHXI50?usp=drive_link) to the `./slides/` folder.~~

2. I execute some shell commands (in `./___render_deploy.sh`) to convert `*.pptx` slides to `*.pdf` as soon as they are ~ready... 


## Attributions

The content of this website is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/), except for the borrowed and mentioned with proper *"Source:"* statements.

## Content

I acknowledge inputs received from these very valuable sources:

- An **Introduction ro R software course** taught by Davide Guido, an instructor of the [Master in Biostatistics and Epidemiology Methods](https://spmsf.unipv.it/master/bioepic/index.html), I attended in 2018/2019
- [R for Epidemiology](https://www.r4epi.com/), **electronic book** by the University of Texas Health Science Center School of Public Health\
- [Appplied Epi](https://appliedepi.org/tutorial/) **Interactive R Tutorials** (Licensed <img src="images/creative-commons.svg" width="16" height="16"/>)
- [Statology](https://www.statology.org/) **Introduction to Statistics with R Tutorials** (Copyrighted <img src="images/copyright-regular.svg" width="16" height="16"/>)
- [Autism dataset analysis](Sydney-informatics-hub-github.io)
- [Introductory Biostatistics with R](https://tuos-bio-data-skills.github.io/intro-stats-book/index.html) by Dylan Z. Childs, Bethan J. Hindle and Philip H. Warren
- [Biostatistics - Concepts and approaches for collecting good data and turning it into knowledge](https://jsgosnell.github.io/cuny_biostats_book/content/getting_started/getting_started.html) by J. Stephen Gosnell

## Cool Stuff you should check out

- [R package {metabolic}](https://fmmattioni.github.io/metabolic/) by Felipe Mattioni Maturana, Ph.D

## Quarto specific resources

- Mine Çetinkaya-Rundel's **blog** [ A Quarto tip a day](https://mine-cetinkaya-rundel.github.io/quarto-tip-a-day/)
- Cornell University's **lesson** [Publishing reproducible documents with Quarto](https://info5940.infosci.cornell.edu/slides/publishing-reproducible-documents/#/themesappearance) 
https://mine-cetinkaya-rundel.github.io/quarto-tip-a-day/
https://info5940.infosci.cornell.edu/slides/publishing-reproducible-documents/#/themesappearance
- Parameterized reports/slides 
  + Mandy Norrbo's **tutorial** [Generate multiple presentations with Quarto parameters](https://www.jumpingrivers.com/blog/r-parameterised-presentations-quarto/)
  + **Jadey Ryan Talk** [website](https://jadeyryan.quarto.pub/rladies-dc-quarto-params/materials.html)
  + **R Medicine David Keyes workshop** [Presentation](https://static.sched.com/hosted_files/rmed2023a/9c/parameterized-reporting-slides.pdf)
  + **Jumping Rivers Mandy Norrbo** [Presentation](https://www.jumpingrivers.com/blog/r-parameterised-presentations-quarto/)


## Web input attributions

<!-- - Favicon1 (giallo) <a target="_blank" href="https://icons8.com/icon/110187/grafico-combinato">Grafico combinato</a> icona di <a target="_blank" href="https://icons8.com">Icons8</a> -->
- Website favicon R-project icon by [icons8](https://icons8.com/icon/xJd_7yBGvl5J/r-project)
- Google Scholar icon from [icons8.com](https://icons8.com/icon/pU44R9xgF3wq/google-scholar)
- Font-Awsome Icons downloaded in `./images/*` from [Font-Awsome svgs](https://github.com/FortAwesome/Font-Awesome/tree/6.x/svgs)
- Great tutorial on "Customizing Quarto Websites" by Sam Csik: [slides](https://ucsb-meds.github.io/customizing-quarto-websites/#/title-slide)
- Free [Adobe Express logo maker](https://www.adobe.com/express/create/logo)
