#! /usr/bin/python3
from os import listdir, rename

for filename in listdir('.'):
    rename(filename, filename.title())
