# BEsmarterGUI

## Summary Video

https://youtu.be/oF43oKd-A4Q

## What is  this project?
This app is developed to make easier applications of Bayesian regression analysis. It is designed thinking about applied researchers and practitioners without programming skills. There are lots a packages that made easy to do a Bayesian regression analysis in R. However, users require programming skills. We want to overcome this requirement such that applied researchers and practitioners applied Bayesian techniques in a very easy graphical environment. In addition, we have in the "Help tab" in our app a paper called “A GUIded tour of Bayesian regression” where all technical concepts are presented such that users can know all underlying theory. This paper also includes a brief user guide.


We encourage anyone who wants to use our app to do it. There is the folder named "dataSim" where users can find some toy files that they can use to start using our app (see Table 3 in our paper: “A GUIded tour of Bayesian regression”, “Help tab” in our app for details). In addition, the folder "DataApp" has all necessary datasets to replicate the applications that we performed in our paper (see Table 4 in our paper, “Help tab” in our app for details). We encorege users to use their own datatsets, and set them using same structure as in our csv files in previous folders. We welcome any comments and feedback.

## Usage from R
Run the following command in R or any R script editor: 
```r
shiny::runGitHub('besmarter/BSTApp', launch.browser = TRUE)
```
and follow the steps that we describe in our paper “A GUIded tour of Bayesian regression” which is located in the "Help" tab in our GUI (top panel, left tab).

## Usage from posit Cloud
Get into [https://posit.cloud/content/4328505](https://posit.cloud/content/10283833), log in or sing up in posit Cloud. Once you are in this project (BEsmarterGUI), click on BSTApp folder in the right-bottom window (Files), then click on app.R file, and finally click on Run App button. 

## Usage from a docker image (Recomended)

```bash
docker pull magralo95/besmartergui:latest
docker run --rm -p 3838:3838 magralo95/besmartergui
```

Then you can access the app going to 127.0.0.1:3838 or http://localhost:3838/


