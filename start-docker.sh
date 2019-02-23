PWD="$(pwd)"
docker build -t pollen-competition .
docker run --rm -e DISABLE_AUTH=true -p 8787:8787 -v $PWD:/home/rstudio/pollen-competition pollen-competition
