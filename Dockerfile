FROM ubuntu:16.04

RUN apt-get update \
    && apt-get install -y gcc gdb gcc-multilib make unzip wget \
    && mkdir /tmp/chibi \
    && cd /tmp/chibi \
    && wget https://github.com/ashinn/chibi-scheme/archive/master.zip \
    && cd /tmp/chibi \
    && unzip master && cd chibi-scheme-master \
    && make && make install \
    && cd /tmp \
    && rm -rf chibi \
    && apt-get clean -y