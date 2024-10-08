# Setup Instructions
Follow the directions below to setup Facets Preview on your local machine. 

## Setup Docker on your local computer.
*   [Mac Install](https://docs.docker.com/desktop/install/mac-install/)

## Pull the Docker Image to your local computer.
* Open a terminal and pull the docker image with `docker pull price0416/fp_docker`

## Setup a folder for Facets Preview.
Facets Preview has a variety of optional features that you may wish to setup, such as accessing institutional data or using HPC resources for data processing.  
To facilitate these features, Facets Preview uses SSHFS directory mounting.  Create the following recommended directory structure somewhere on your local machine.

![Directory Structure](../images/dirStruct.png)

# Download the Facets Preview launch script.
Download the [launch_fp.sh](../launch_fp.sh) script and save it to your facets preview directory.

Prepare the script for launch by running `chmod 775 launch_fp.sh`.

## Prepare any SSHFS mounts for desired functionality.
Facets Preview has a several features that rely on SSHFS mounts of institutional resources.  If you would like to setup your install of Facets Preview to use these functions, see the following sections:
* [Access institutional FACETS IMPACT data repository.](access_impact.md)
* [Access FACETS TCGA data repository.](access_tcga.md)
* [Perform refits with HPC scheduler.](access_refits.md)
* [Create personal refits of institutional data.](personal_refits.md)


## Run the Docker Image using the launch_fp.sh script.
* Navigate to your Facets Preview directory and run `./launch_fp.sh`. This will initiate the required processes for Facets Preview and launch the Docker image. You will notice the prompt on your terminal now shows `root@randomNumberString`.  
* The container will automatically launch Facets Preview.
* Open a browser and navigate to [http://0.0.0.0:3838/](http://0.0.0.0:3838/)
* At this point you can consider yourself to be "inside" the running container.  If you want to stop running Facets Preview, press CTRL+D in the docker container.  If you want to exit the container, use CTRL+D.
