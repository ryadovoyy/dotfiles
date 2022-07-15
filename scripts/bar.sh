#!/bin/sh

dwm_lang() {
    case "$(xset -q | grep LED | awk '{ print $10 }')" in
        "00000000") echo "󰌌 En";;
        "00001000") echo "󰌌 Ru";;
                 *) echo "󰌌 Err";;
    esac
}

dwm_pulse () {
    vol=$(pactl list sinks | grep '^[[:space:]]Volume:' | \
        head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')
    if [ "$vol" -eq 0 ]; then
        echo "󰝟"
    elif [ "$vol" -gt 0 ] && [ "$vol" -le 33 ]; then
        echo "󰕿$vol"
    elif [ "$vol" -gt 33 ] && [ "$vol" -le 66 ]; then
        echo "󰖀$vol"
    else
        echo "󰕾$vol"
    fi
}

dwm_date () {
    echo "󰃰 $(date "+%a, %d %b %H:%M")"
}

generate_content () {
    echo "[$(dwm_lang)] [$(dwm_pulse)] [$(dwm_date)]"
}

while true; do
    xsetroot -name "$(generate_content)"
    sleep 1m
done
