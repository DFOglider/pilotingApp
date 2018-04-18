# pilotingApp

UNDER DEVELOPMENT

This is a Shiny app that is intended to be used by pilots of
[SeaExplorer](https://www.alseamar-alcen.com/products/underwater-glider/seaexplorer)
gliders. The app downloads real-time data transmitted from the glider
over Iridium, processes it into a format suitable for plotting, and
then displays plots of the various fields of interest.

# How to use:

## Standalone (i.e. local shiny server)

1. Install [R](www.r-project.org) and [RStudio](www.rstudio.com)

2. Download the Shiny app code from this repository, either through
   direct download, using Github Desktop, or by typing:
   ```
   $ git clone https://github.com/melanybelzile/pilotingApp.git
   ```
   in a terminal.

   The latter is recommended, as then updating the app can be done
   with a simple `git pull` command or by pulling changes in Github Desktop.

3. Open the file `app.R` in Rstudio. You will need to install several packages (as listed at the
   top of `app.R`). To do this run the following in the R console (copy/paste):
   ```r
   install.packages(c('shiny', 'oce', 'ocedata', 'measurements', 'leaflet', 'RCurl', 'geosphere', 'XML'))
   ```
   
4. After installing all the required packages, run the app by clicking "Run App" at the top of the code editor.

5. Once the app is running, select the `glider` and `mission` from the
   pull-down menu, and click "Download and load data" to fetch data
   from the glider FTP site and load it into the app. The downloaded
   data is stored in a local folder within the app directory.
   
## On a shiny server

1. Set up and configure a web accesible Shiny server (see e.g. https://www.digitalocean.com/community/tutorials/how-to-set-up-shiny-server-on-ubuntu-16-04)

2. Copy `pilotingApp` code to the shiny server directory and install all required packages (see above).

3. Go to the server URL, and follow step 5 above.
