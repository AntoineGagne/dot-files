#! /bin/sh

udisksctl unmount -b "$1"
udisksctl power-off -b "$1"
