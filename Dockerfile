FROM rikorose/gcc-cmake:gcc-7

RUN apt-get update && apt-get install re2c bison
