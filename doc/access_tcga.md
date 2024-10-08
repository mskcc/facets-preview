# Accessing the FACETS institutional data repository with Facets Preview.
MSKCC maintains a repositorty of FACETS data for TCGA samples.  
This data currently is stored on terra, and you will need an account with access to the data to set up the link between the data repository and Facets Preview.

In the initial installation step outlined in the [installation instructions](setup.md), you should have prepared a directory structure for Facets Preview as follows:

![Directory Structure](../images/dirStruct.png)

This document will address setting up your tcga/ folder.

## Prepare access to your FACETS data from remote sources.
* Setup [macFUSE](https://osxfuse.github.io/) for running SSHFS mounts if you have not already done so.
* Add a mount command to your ~/.bash_profile on your local computer to mount TCGA. `alias mounttcga='sudo umount -f /your/fp/directory/tcga || sshfs YOUR-USER-ID@terra.mskcc.org:/juno/work/ccs/shared/resources/tcga /your/fp/directory/tcga -o auto_cache -o defer_permissions -o local -o IdentityFile=/Users/YOUR-USER-ID/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks'`
* Update your bash_profile. `source ~/.bash_profile`
* Mount the remote location. `mounttcga`. This will create a remote mount of the drive to your local drive, in this example at `/your/fp/directory/tcga`. Note that you may need to execute this command twice, as the first execution attempt will sometimes ask for your local adminstrator password, and the second attempt will ask for your remote login credentials.
* You can confirm your mount is functioning properly by examining /your/fp/directory/tcga/ or by using the `mount` command on the command line.
* When you next launch Facets Preview, on the session tab, you can setup your local TCGA repository path using your mount directory path.
*    <img src="../images/tcga_repo_path.png" alt="TCGA Repository Configuration" width="50%">
* You should now be able to load samples from the TCGA repository on the Load Samples tab by ID or by path.


