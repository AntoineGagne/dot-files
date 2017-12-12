#! /usr/bin/env bash

main() {
    local -r manufacturer_name="Corsair"
    local -ir devices_number=$(lsusb | grep -c ${manufacturer_name})

    if [[ ${devices_number} -lt 1 ]]; then
        (>&2 echo "No Corsair devices connected. Exiting with status code 0.")
        exit 0
    fi

    /usr/bin/nohup /usr/bin/ckb -b &>/dev/null &
}

main
