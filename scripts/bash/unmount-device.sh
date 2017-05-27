#! /bin/sh

udiskctl unmount -b "$1"
udiskctl power-off -b "$1"
