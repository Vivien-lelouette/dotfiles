#!/usr/bin/env bash
#herbstclient rule once maxage=10 pseudotile=on focus=on
echo "RULES: ${RULES}" > ~/temp-rules
echo "herbstclient rule once maxage=10 ${RULES[@]}" >> ~/temp-rules
herbstclient rule once maxage=10 ${RULES[@]}
exec "$@"
