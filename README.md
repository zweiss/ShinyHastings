# ShinyHastings Readme

This is the repository for the Shiny app "ShinyHastings", which was designed to let users experience the influence of proposal types on the Metropolis Hastings algorithm. It was designed by Zarah Weiß in the course of the seminar "Bayesian data analysis & cognitive modeling 2015" in the winter term 2015/16.

## Repository Content

The repository contains the following files:
* **README.md**, this readme file
* **shiny-hastings.pdf**, a paper describing theoretical background and implementational details to ShinyHasitings
* **/shiny-hastings**, the directory containing the Shiny code to run the program
* **/shiny-hastings/server.R**, the file containing the server side of the program
* **/shiny-hastings/ui.R**, the file containing the user interface of the program
* **/shiny-hastings/helper.R**, the helper file containing the actual implementation of the MH algorithms

## ShinyHastings Description & Usage

The ShinyHastings app was created to show the influence of proposal types on the Metropolis Hastings algorithm.
It consists of two panels: a settings panel to the left or at the top of the page (depending on the browser size), and a result panel displaying the summary statistics and plots to investigate the algorithm's performance to the right or at the bottom of the page.

Each panel has separate tabs, further structuring the application based on functionality. While the result panel displays a separate tab for each of the offered statistics and convergence measures, the settings panel consits of three tabs: one to set the algorithm settings, one to set the experiment settings and one containing short instructions on how to handle the app.

In order to start a sampling process, the "Approximate target!" button on the algroithm settings tab needs to be clicked.
Before starting a process, the user may choose a proposal type as well as the tuning parameters specific to it. S/He may also adjust sampling duration in terms of chain and iteration number and burn-in size as well as the target distribution to approximate.

More detailed information may be found in the article on this site.
