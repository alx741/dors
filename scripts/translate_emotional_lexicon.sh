#!/bin/sh

LEXICON_EN="../data/lexicon_en.csv"
LEXICON_ES="../data/lexicon_es.csv"

header_handled=false
cat "$LEXICON_EN" | \
while read line; do

    if [ ! $header_handled = true ]; then
        echo "$line" > "$LEXICON_ES"
        header_handled=true
        continue
    fi

    word_id=$(echo "$line" | cut -d"," -f1)
    word=$(echo "$line" | cut -d"," -f2)
    rest=$(echo "$line" | cut -d"," --complement -f1,2)

    echo "$word_id: $word"
    translation=$(trans -e bing -b "en:es" -no-warn "$word")

    echo "$word_id,$translation,$rest" >> "$LEXICON_ES"

done
