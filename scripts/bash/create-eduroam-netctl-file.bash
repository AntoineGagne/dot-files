#! /usr/bin/env bash

if [ -z "${1}" ]; then
    declare -r profile_name="eduroam-profile"
else
    declare -r profile_name="${1}"
fi

read -p "Enter your IDUL: " -r idul
read -s -p "Enter your password: " -r password

cat << EOF > "$(pwd)/${profile_name}"
Description='School Network'
Interface=wlo1
Connection=wireless

Security='wpa-configsection'
IP=dhcp
ESSID='eduroam'

WPAConfigSection=(
    'ssid="eduroam"'
    'key_mgmt=WPA-EAP'
    'eap=PEAP'
    'phase2="auth=MSCHAPV2"'
    'identity="${idul}@ulaval.ca"'
    'psk="$(echo "${password}" | iconv -t utf16le | openssl md4 | awk '{print $2}')"'
)
EOF

if [ "$?" -ne 0 ]; then
    echo "There was a problem with the creation of the profile."
    exit 1
fi

echo "The profile was created successfully!"
