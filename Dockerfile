FROM ghcr.io/rikorose/gcc-cmake:latest

RUN apt-get update && apt-get install -y re2c bison
