# Oregon Metro VisionEval Extras
This repo is for additional, Oregon Metro specific, VE packages, modules, and model variants that relate to Metro's VE climate modeling. 

The *setup.bat* script allows user to either install the modules that are available for VE 3.0 or 
setup the *.Renviron* to point to the right VE library so that users can run RStudio from this directory.
The setup script can install all the modules in the visioneval library provided dependencies are already
installed. If the installation is unsuccessful then it is recommend for users to start RStudio and install
the dependencies and the packages manually. The library path to visioneval library should be set when the *setup.bat* script is run.



**Here is the process for getting a model variant up and running:**

<u>Model variant</u>: “metro-reference”  

<u>Assumptions</u>: VERSPM model (We need these software tools installed and running git, RStudio, R v4.2.3, and Rtools v4.2 are installed on the machine).  Note: if you build from scratch (do Option B), you can select which R version and what Rtools you are using. 



Install the runtime VisionEval

Option A:

- Install the Executable Zip file that will place a Runtime folder on your machine.



Option B: Build a runtime VisionEval

1. Clone https://github.com/RSGInc/VisionEval-Dev repository on a server or local machine. I’ll assume the location of the clone is C:\VisionEval-Dev
2. Navigate to C:\VisionEval-Dev and double click the VisionEval-dev.Rproj. This will open RStudio. Make sure that the version of R on your machine is not higher than v4.2.3.
3. Run the command “ve.build()”. This will build a runtime environment in “C:\VisionEval-Dev\built\visioneval\4.2.3\runtime”

Install Metro Extra packages:

1. Clone https://github.com/RSGInc/Metro-VE-Extras repository on a server or local machine. I’ll assume the location of the clone is C:\Metro-VE-Extras

2. Navigate to C:\Metro-VE-Extras, double click setup.bat, and follow the prompts. The first prompt asks for the R version. If you say "n", then it goes to the second prompt. 

   Prompts:

   a. First prompt. For R prompt choose the version used to build VisionEval runtime environment (in the instructions above 4.2.3). For e.g. C:\Program Files\R\R-4.2.3.

   b. 2nd Prompt. VE Library. The visioneval library prompt choose the visioneval library built. From instructions above it is “C:\VisionEval-Dev\built\visioneval\4.2.3\ve-lib” 

   

   RStudio to Install Packages

   1. When choosing the modules, choose to install all modules. Ideally this should install all the modules but for the very first time it might fail. This is because the work from home (WFH) and the multimodal modules require additional R libraries that are not installed during the VisionEval build. 
      1. If this step fails then:

​                     i.  Verify that “.Renviron” file is present in the “C:\Metro-VE-Extras” directory. If not then:  double click “setup.bat” in “C:\Metro-VE-Extras” and choose “n” for installation. On the visioneval library prompt enter C:\VisionEval-Dev\built\visioneval\4.2.3\ve-lib. This will create the write ".Renviron” file.

​                     ii.  Double click on VisionEval-dev.Rproj in “C:\Metro-VE-Extras” directory. This will open RStudio and set the library to the one specified in “.Renviron” file. Install the missing dependencies using R command “install.packages”

​                    iii.  Install the modules using R command “install.packages”. Here’s an example to install multimodal module – “*install.packages(“VERSPMMetroModels”, type=”source”, repos=NULL)*”. The additional arguments are necessary when installing modules from a local source.  ***Note***: If you are re-installing a package sometimes the build process fails to overwrite existing packages in the ve-lib built runtime. Sometimes it is helpful to delete the package that is being overwritten before re-installing.  

 

Install and Run VERSPM model

1. After successful installation of all the modules from Metro-VE-Extras navigate to “C:\VisionEval-Dev\built\visioneval\4.2.3\runtime” and double click     VisionEval-dev.Rproj. This will open RStudio and setup visioneval environment and give access to the metro models.
2. Enter the R command “baserefmodel <- installModel(“VERSPM”,“metro-reference”, confirm=FALSE)”. This will install the AP powertrain version of the Metro RSPM model variant.
3. Enter the R command “baserefmodel$run()”. To run the model.

 




## Model Variants

### Portland Metro Model (VERSPM) variants

**Metro Base Multimodal '17 with Work From Home module using reference case inputs (variant name: metro-reference)**

*VERSPMMetroModels/inst/models/model-index.cnf*

**Metro Base Multimodal '17 with Work From Home module with STS target Rule inputs (variant name: metro-targetsts)**

*VERSPMMetroModels/inst/models/model-index.cnf*

**Metro Base Multimodal '17 with Work From Home module with STS target Rule inputs but with Older and Heavier vehicle fleet (variant name: metro-targetsts-newvehinputs)**

*VERSPMMetroModels/inst/models/model-index.cnf*

**Metro Base Multimodal '17 with Work From Home module with STS target Rule inputs but with edits to lane miles PAYD insurance for RTP Round 2 (variant name: metro-rtp-round2)**

*VERSPMMetroModels/inst/models/model-index.cnf*





