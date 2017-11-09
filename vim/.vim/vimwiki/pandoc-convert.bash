#! /usr/bin/env bash

declare -r default_template_name="notes"

to_html() {
    local -r chosen_syntax="${2}"
    local -r wiki_file_extension="${3}"
    local -r output_directory="${4}"
    local -r input_file="${5}"
    local -r css_file="light-responsive/css/$(basename ${6})"
    local -r template_path="${7}"
    local -r template_name="${8}"
    local -r template_extension="${9}"

    pandoc --from="${chosen_syntax//default/vimwiki}" \
           --to="${template_extension:1}" \
           --standalone \
           --template="${template_path}${template_name:-${default_template_name}}${template_extension}" \
           --wrap=auto \
           --table-of-contents \
           --highlight-style=haddock \
           --section-divs \
           --email-obfuscation=references \
           --css="${template_path}${css_file}" \
           --mathml \
           --variable="lang: ${LANG:-en-ca}" \
           --variable="otherlangs: fr" \
           --variable="title: $(basename "${input_file%.${wiki_file_extension}}")" \
           "${input_file}" \
           --output="${output_directory}$(basename "${input_file%.${wiki_file_extension}}")${template_extension}"
}

to_html "${@}"
