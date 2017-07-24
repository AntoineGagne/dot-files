#! /usr/bin/env bash

newsbeuter_directory="${HOME}/.newsbeuter"
private_urls_file="${newsbeuter_directory}/private"
public_urls_file="${newsbeuter_directory}/public"

paste -d "\n" "${private_urls_file}" "${public_urls_file}" > "${newsbeuter_directory}/urls"
