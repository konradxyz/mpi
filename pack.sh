#!/bin/bash

make clean
rm msp_kp306410.tar.gz
tar -cvvf msp_kp306410.tar.gz Makefile matgen.h matgen-mt.c msp-par.c msp-seq-naive.c msp-par.ll

