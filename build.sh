#!/usr/bin/bash -e

stack build
stack exec Brainard-exe gen
tsc 