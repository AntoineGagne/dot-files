#! /usr/bin/env bash

create-template() {
    local -r template="${HOME}/.pandoc/templates/html/notes.html"

    tee "${template}" << 'EOF'
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="$lang$" xml:lang="$lang$"$if(dir)$ dir="$dir$"$endif$>
    <head>
        <meta charset="utf-8" />
        <meta name="generator" content="pandoc" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />
        $for(author-meta)$
            <meta name="author" content="$author-meta$" />
        $endfor$
        $if(date-meta)$
            <meta name="dcterms.date" content="$date-meta$" />
        $endif$
        $if(keywords)$
            <meta name="keywords" content="$for(keywords)$$keywords$$sep$, $endfor$" />
        $endif$
        <title>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$</title>
        <style type="text/css">
            code{white-space: pre-wrap;}
            span.smallcaps{font-variant: small-caps;}
            div.line-block{white-space: pre-line;}
            div.column{display: inline-block; vertical-align: top; width: 50%;}
        $if(quotes)$
            q { quotes: "“" "”" "‘" "’"; }
        $endif$
        </style>
        $if(highlighting-css)$
            <style type="text/css">
                $highlighting-css$
            </style>
        $endif$
        $for(css)$
            <link rel="stylesheet" href="$css$">
        $endfor$
        $if(math)$
            $math$
        $endif$
        <!--[if lt IE 9]>
            <script src="//cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.3/html5shiv-printshiv.min.js"></script>
        <![endif]-->
        $for(header-includes)$
            $header-includes$
        $endfor$
        $for(javascript)$
        <script src="$javascript$"></script>
        $endfor$
    </head>
    <body>
        $for(include-before)$
            $include-before$
        $endfor$
        <div class="navbar">
            <button id="toggle-nav">
                <span class="sr-only">Toggle navigation</span>
                <span class="bar"></span>
                <span class="bar"></span>
                <span class="bar"></span>
            </button>
        </div>
        $if(title)$
            <header>
                <h1 class="title">$title$</h1>
                $if(subtitle)$
                    <p class="subtitle">$subtitle$</p>
                $endif$
                $for(author)$
                    <p class="author">$author$</p>
                $endfor$
                $if(date)$
                    <p class="date">$date$</p>
                $endif$
            </header>
        $endif$
        $if(toc)$
            <nav id="$idprefix$TOC">
                $table-of-contents$
            </nav>
        $endif$
        $body$
        $for(include-after)$
            $include-after$
        $endfor$
    </body>
EOF

    tee -a "${template}" << EOF
    <script src="${HOME}/.pandoc/templates/html/common/scripts/menu.js"></script>
</html>
EOF
}

create-template &>/dev/null
