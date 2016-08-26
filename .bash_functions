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

build_tex() {
    pdflatex "${1}" && pdflatex "${1}" && rm *.toc *.lof *.log *.lot *.out *.aux *.lol
}

playlist_to_links() {
    if [[ -z "$1" ]];
    then
        echo "Please, specify the playlist code as argument" 1>&2
        exit 1
    fi
    URL="https://www.youtube.com/playlist?list=$1"

    TMPFILE=`mktemp /tmp/${tempfoo}.XXXXXX` || {
        echo "Temporary file creation failed" 1>&2
        exit 1
    }

    COMMAND=""
    if [[ -n $(type -p wget) ]];
    then
        COMMAND="wget -o /dev/null -O '${TMPFILE}.html' '$URL'"
    fi
    if [[ -z "$COMMAND" ]] && [[ -n $(type -p curl) ]];
    then
        COMMAND="curl -s -o '${TMPFILE}.html' '$URL'"
    fi
    if [[ -z "$COMMAND" ]];
    then
        echo "Please, install wget or curl to use this script" 1>&2
        exit 1
    fi

    eval $COMMAND
    grep -e "^<tr.*<a href=\"/watch?v=.*$1" "${TMPFILE}.html" | sed 's/.*watch/https:\/\/www.youtube.com\/watch/g' | sed 's/&amp.*//g' > playlist_$1.txt

    while [[ $(grep "continuation=" ${TMPFILE}.html) ]]
    do
        sed -i 's/_continuation=1/\n/g' ${TMPFILE}.html
        CONTINUATION=$((sed 's/\(.*\)continuation=/https:\/\/www.youtube.com\/browse_ajax?action_continuation=1&/g' | sed 's/".*//g' | sed 's/\\u0026/\&/g' | tr -d '\' ) <<< $(grep "continuation=" ${TMPFILE}.html))
        
        if [[ -n $(type -p wget) ]];
        then
            wget -o /dev/null -O "${TMPFILE}.html" "$CONTINUATION"
        else
            curl -s -o "${TMPFILE}.html" "$CONTINUATION"
        fi

        tr -s '\\|/' '\n' < ${TMPFILE}.html >  "${TMPFILE}"
        grep "^watch" ${TMPFILE} | sed 's/\\u0026amp;\(.*\)//g' | sed 's/watch/https:\/\/www.youtube.com\/watch/g' | sort -u >> playlist_$1.txt
    done

    rm -f ${TMPFILE}*
}

parallel_download() {
    parallel youtube-dl -o "\%\(title\)s.\%\(ext\)s" -x :::: "${1}"
}
