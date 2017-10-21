#! /usr/bin/env bash

newsbeuter_directory="${HOME}/.newsboat"
private_urls_file="${newsbeuter_directory}/private"
public_urls_file="${newsbeuter_directory}/public"

paste -d "\n" "${private_urls_file}" "${public_urls_file}" | grep -v '^$' | sort -u > "${newsbeuter_directory}/urls"
