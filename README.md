
This project contains the source files to reproduce the results of experiments on the MARIUS model. This experiment is described in the paper:

"An incremental method for building and evaluating multi-agent models", submitted to JASSS, under review, 2014.

Generate documentation of the model is published [here](http://iscpif.github.io/marius-method/).
A description of the model following the ODD protocol is available here [here](https://github.com/ISCPIF/marius-method/blob/master/ODD-MARIUS.pdf).

Licence
-------

This software is licenced under the GNU Affero GPLv3 free software licence. 

Usage (simulation)
------------------

To compile and run this project you need sbt 0.13 (http://www.scala-sbt.org/).

Go to the model directory.

`cd model`

To execute a single run: 

`sbt run`

To build and publish the OpenMoLE plugin:

`sbt osgi-bundle`

Get the plugin in your target directory, for instance:

We use OpenMoLE to describe and launch our experimentation.

> OpenMOLE (Open MOdeL Experiment) is a workflow engine designed to leverage the computing power of parallel execution environments for naturally parallel processes. A process is told naturally parallel if the same computation runs many times for a set of different inputs. OpenMOLE workflows are suitable for many types of naturally parallel processes such as model experiment, image processing, text analysis�. It is distributed under the AGPLv3 free software license.

Description of OpenMoLE installation is described on www.openmole.org website.

You can find multiple other great tutorials and examples of other applications on same website.

To launch OpenMoLE in console mode and load the exploration jar : 

`openmole -c -p /path/to/marius_2.11.jar`

Then you can use the workflows available in the openmole directory (it is compatible with OpenMoLE 0.9). Those workflows are configured to run on the biomed VO of the grid EGI, however switching the execution environment in OpenMoLE is easy so you can use this workflow on you own multi-core machine, cluster or grid virtual organisation (you can find examples of workflows in the tutorial section on the openmole website).

Usage (graphics)
----------------

You can find graphics scripts into the R_script folder.
The scripts have been developed on  R version 2.15.1  and require the following libraries:

	-ggplot2
	-reshape2
	-animation
	-RColorBrewer
	-gridExtra

Gif animations generations may require the ImageMagick tool to silently generate the .gif files.

Before executing the script, edit it to add the path to the simulation results and the data file (see last part of the 0 section)

The last section of the script (6) contains examples of function call to generate various graphics and a gif animation. 

