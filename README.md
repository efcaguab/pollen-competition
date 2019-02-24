# The mutualistic trade-off

We explore competition for pollinators at the community level.

### Reproduce the analyses

We use docker to encapsulate all the dependences of the project. 
All code was tested on a x86_64-apple-darwin15.6.0 (64-bit) platform but should run in most unix systems. 

### Requirements 

* Docker 18.09 or later (might work on earlier versions but has not been tested)
* Muticore processor with 4 cores or more
* 8GB RAM or more

### Steps

**1. Run the docker container**

There are a couple alternatives here.
The easiest one if you are in a Unix system (Mac OS or Linux) is to run `bash start-docker.ps`.
This script will build the container image and run it for you

**2. Run the analyses**

To reproduce the analyses, just run `make` from a the main directory in your terminal. 

