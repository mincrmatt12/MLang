FROM rikorose/gcc-cmake:gcc-7

RUN apt-get update && apt-get install -y re2c bison
