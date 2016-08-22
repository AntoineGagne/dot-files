# Tabs to spaces

tabs2spaces() {
    if [ -z "${1}" ]; then
        echo "You must enter a filename"
    else
        if [ -f "${1}" ]; then
            find . -name "${1}" ! -type d -exec bash -c 'expand -t 4 "$0" > /tmp/e && mv /tmp/e "$0"' {} \;
        else
            echo "${1} does not exist"
        fi
    fi
}


# Output CSV files as tables

outputcsvtables() {
    if [ -z "${1}" ]; then
        echo "You must enter a filename"
    else
        if [ -f "${1}" ]; then
            sed 's/,,/, ,/g;s/,,/, ,/g' "${1}" | column -s, -t
        else
            echo "${1} does not exist"
        fi
    fi
}


# Output a funny message

startup_message() {
    cow_forms=(apt beavis.zen bong bud-frogs bunny calvin cheese cock cower daemon default
               dragon dragon-and-cow duck elephant elephant-in-snake eyes flaming-sheep
               ghostbusters gnu head-in hellokitty kiss kitty koala kosh luke-koala
               mech-and-cow meow milk moofasa moose mutilated pony pony-smaller ren sheep
               skeleton snowman sodomized-sheep stegosaurus stimpy suse three-eyes turkey
               turtle tux unipony unipony-smaller vader vader-koala www)

    array_size=${#cow_forms[*]}
    
    echo -e "Today is: $(nowdate) \nTime is:  $(now) \n$(fortune)" | cowsay -f ${cow_forms[$((${RANDOM}%${array_size}))]}
}

compile_tex() {
    pdflatex "${1}" && rm *.toc *.lof *.log *.lot *.out *.aux
}
