# BEsmarterGUI

## Summary Video

https://youtu.be/oF43oKd-A4Q

## What is  this project?
This app is designed to make Bayesian regression analysis easier for applied researchers and practitioners who do not have programming skills. Although there are many R packages that facilitate Bayesian regression analysis, they still require users to write code. Our goal is to remove this barrier so that applied researchers and practitioners can use Bayesian techniques through a simple and intuitive graphical interface.

In the Help tab of our app, we include links to my book — the online version (https://bookdown.org/aramir21/IntroductionBayesianEconometricsGuidedTour/
) and the PDF version (https://drive.google.com/file/d/1_IBKe7vS5a2XLnvg74T_UNESRoKNDUUt/edit
) — where all technical concepts are presented so users can understand the underlying theory.

We encourage everyone to use our app. The folder dataSim contains toy datasets that users can explore to get started (see Table A.2 in the book or the “Help” tab for details). The folder DataApp includes all datasets necessary to replicate the applications shown in our paper (see Table A.4 in the book or the “Help” tab for details). We also encourage users to work with their own datasets, following the same structure as the CSV files provided in these folders.

We welcome all comments and feedback.

## Usage from R

First, install the shiny package. Then run the following command in R (or any R script editor):
```r
shiny::runGitHub('besmarter/BSTApp', launch.browser = TRUE)
```


After launching, follow the steps described in my book, which is linked in the Help tab of the app (top panel, left tab).

## Usage from Posit Cloud

Open the app directly at:

👉 https://andres-ramirez-hassan.shinyapps.io/BSTApp/

Please note: the free Posit Cloud tier sometimes runs out of memory, which can cause the app to stop. Sorry for the inconvenience.

## Usage from Fly.io

You can also access the app via:

👉 https://fly-besmarter.fly.dev/

As with Posit Cloud, occasional memory limits on the free tier may affect performance.

## Usage from a local Docker Image (Recommended)

Install Docker and then run the following commands in your terminal:
```bash
docker pull aramir21/besmartergui:latest
docker run --rm -p 3838:3838 aramir21/besmartergui
```

Once running, access the app in your browser at:

👉 http://127.0.0.1:3838 or 👉 http://localhost:3838/