# Tabs to spaces

function tabs2spaces {
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

function outputcsvtables {
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

function startup_message {
    cow_forms=(apt beavis.zen bong bud-frogs bunny calvin cheese cock cower daemon default
               dragon dragon-and-cow duck elephant elephant-in-snake eyes flaming-sheep
               ghostbusters gnu head-in hellokitty kiss kitty koala kosh luke-koala
               mech-and-cow meow milk moofasa moose mutilated pony pony-smaller ren sheep
               skeleton snowman sodomized-sheep stegosaurus stimpy suse three-eyes turkey
               turtle tux unipony unipony-smaller vader vader-koala www)

    array_size=${#cow_forms[*]}
    
    echo -e "Today is: $(nowdate) \nTime is:  $(now) \n$(fortune)" | cowsay -f ${cow_forms[$((${RANDOM}%${array_size}))]}
}

function build_tex {
    pdflatex "${1}" && pdflatex "${1}" && rm *.toc *.lof *.log *.lot *.out *.aux *.lol
}

function playlist_to_links {
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

function parallel_download {
    parallel youtube-dl -o "\%\(title\)s.\%\(ext\)s" -x :::: "${1}"
}

# Set marks to quickly jump to directories
export MARKPATH=$HOME/.marks
function jump {
    cd -P "${MARKPATH}/${1}" 2>/dev/null || echo "No such mark: ${1}"
}

function mark {
    mkdir -p "${MARKPATH}"; ln -s "$(pwd)" "${MARKPATH}/${1}"
}

function unmark {
    rm -i "${MARKPATH}/${1}"
}

function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' | column -t && echo
}

function _completemarks {
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist=$(find $MARKPATH -type l -printf "%f\n")
    COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    return 0
}

# Run the process in background
function rb {
    "$@" &>/dev/null &
}
complete -F _command rb


# Extract archives of any type
function extract {
    if [ -z "$1" ]; then
        # display usage if no parameters given
        echo -n "Usage: extract <path/file_name>"
        echo -n ".<zip|rar|bz2|gz|tar|tbz2|tgz|Z|"
        echo "7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    else
        if [ -f "$1" ] ; then
            NAME=${1%.*}
            #mkdir $NAME && cd $NAME
            case "$1" in
                *.tar.bz2)   tar xvjf ./"$1"    ;;
                *.tar.gz)    tar xvzf ./"$1"    ;;
                *.tar.xz)    tar xvJf ./"$1"    ;;
                *.lzma)      unlzma ./"$1"      ;;
                *.bz2)       bunzip2 ./"$1"     ;;
                *.rar)       unrar x -ad ./"$1" ;;
                *.gz)        gunzip ./"$1"      ;;
                *.tar)       tar xvf ./"$1"     ;;
                *.tbz2)      tar xvjf ./"$1"    ;;
                *.tgz)       tar xvzf ./"$1"    ;;
                *.zip)       unzip ./"$1"       ;;
                *.Z)         uncompress ./"$1"  ;;
                *.7z)        7z x ./"$1"        ;;
                *.xz)        unxz ./"$1"        ;;
                *.exe)       cabextract ./"$1"  ;;
                *)           echo -n "extract: '$1'"
                echo "- unknown archive method" ;;
            esac
        else
            echo "'$1' - file does not exist"
        fi
    fi
}
complete -F _command extract

# Execute previous command as root
function sprev {
    sudo $(history -p \!\!)
}
