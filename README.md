
# Facets Preview
`facets-preview` is an R shiny based desktop app that enables loading, reviewing and annotating fits as well as function to generate downstream genomic calls. This repository represents the build used for the docker container at price0416/fp_docker.

# FAQs
*  Please check our [FAQ section](faq.md) for information on commonly asked FACETS-related issues.

# Setup Instructions

## Setup Docker on your local computer.
*   [Mac Install](https://docs.docker.com/desktop/install/mac-install/)

## Pull the Docker Image to your local computer.
* Open a terminal and pull the docker image with `docker pull price0416/fp_docker`

## Prepare access to your FACETS data from remote sources.
* If you want to access FACETS data on a remote machine (i.e. pyr/juno), setup [macFUSE](https://osxfuse.github.io/).
* Make a local drive to host your mount, i.e. `mkdir ~/mskcc/juno/impact/facets`
* Add a mount command to your ~/.bash_profile on your local computer. For example, to mount pyr, `alias mountfacetspyr='sudo umount -f ~/mskcc/juno/impact/facets || sshfs yourUser@pyr.mskcc.org:/rtsess01/compute/juno/cmo/juno/work/ccs/shared/resources/impact/facets ~/mskcc/juno/impact/facets -o auto_cache -o defer_permissions -o local -o IdentityFile=/Users/yourUser/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks'`
* Run your bash_profile. `source ~/.bash_profile`
* Mount the remote location. `mountfacetspyr`. This will create a remote mount of the drive to your local drive, in this example at `~/mskcc/juno/impact/facets`. Note that you may need to execute this command twice, as the first execution attempt will sometimes ask for your local adminstrator password, and the second attempt will ask for your remote login credentials.
* Navigate to the mounted directory. `cd ~/mskcc/juno/impact/facets`

## Run the Docker Image
* Run the docker image. `docker run -v $PWD:$PWD --workdir $PWD  -p 3838:3838 --name test --rm -i -t price0416/fp_docker /bin/bash`. You will notice the prompt on your terminal now shows `root@randomNumberString`.  At this point you are inside the running container.  If you ever need to exit the container, use CTRL+D.
* Start FACETS Preview with the following command from inside the running container. `Rscript -e "facets_preview_config_file = '/usr/bin/facets-preview/fp_config.json' ; options(shiny.port = 3838, shiny.host = '0.0.0.0', shiny.launch.browser = FALSE) ; library(facetsPreview); facetsPreview::launch_application()"`
* Open a browser and navigate to [http://0.0.0.0:3838/](http://0.0.0.0:3838/)
* FACETS Preview should load.

## Loading a Sample
* Note that in this version of FACETS Preview that samples can only be loaded using the "Paste facets run directories" box at the bottom of the load screen.
* Provide full paths to the samples you want to load in this box. If they are not found or do not load properly, be sure to confirm that you started your docker image after navigating to the target facets data location before starting the container. i.e. `cd ~/mskcc/juno/impact/facets`
