# Smtml Benchmarking

## Building and running Docker image

- If you haven't already, install [Docker](https://docs.docker.com/engine/install/)

- Clone `smtml`'s repository

```sh
git clone https://github.com/formalsec/smtml.git
cd smtml
```

- Build the Docker image

```sh
docker build -t smtml .
```

- Run the Docker image

```sh
docker run -it smtml /bin/bash
```

## Running evaluation benchmarks

- Once running the Docker image perform initialise the benchmarking submodules

```sh
git submodule init
```

### EQ2
To run and generate the plots for EQ2, perform the following steps:

- Clone benchmark submodule

```sh
cd bench
git submodule update --remote -- smt-comp/
```

- Run the `eq2.sh` script

```sh
./eq2.sh
```

### EQ3
To run and generate the plots for EQ3, perform the following steps:

- Clone benchmark submodule (if you already did this for EQ2, skip this step)

```sh
cd bench
git submodule update --remote -- smt-comp/
```

- Run the `eq3.sh` script

```sh
./eq3.sh
```

### Case Study
To run and generate the plots for the symbolic execution case study, perform the following steps:

- Clone benchmark submodule

```sh
cd bench
git submodule update --remote -- smt-testcomp23/
```

- Run the `testcomp.sh` script

```sh
./testcomp.sh
```