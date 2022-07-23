#!/bin/bash

dir=$(dirname $0)

cmd=""
if [[ ${VELISP} != "" ]]; then
    cmd=${VELISP}
else
    cmd="node ${dir}/../../src/main.js"
fi

echo "${cmd}"


RAND_SEED=0 ${cmd} ${dir}/../../examples/fifteen.lsp &
PID=$!
echo "PID: $PID"

WID=''
for ((i = 0; i < 30; i++)); do
    WID=$(xdotool search --name 'Fifteen Puzzle')
    if [[ -z $WID ]]; then
        sleep 1
    else
        break
    fi
done
if [[ -z $WID ]]; then
    echo "Window not found"
    exit 1
else
    echo "WID: $WID"
fi

# To determine coordinates
# $ WID=$(xdotool search --name "Fifteen Puzzle")
# $ xdotool mousemove -window $WID 40 30

declare -A coord=(
    [1x1]=' 40  30'
    [1x2]='110  30'
    [1x3]='180  30'
    [1x4]='250  30'
    [2x1]=' 40 100'
    [2x2]='110 100'
    [2x3]='180 100'
    [2x4]='250 100'
    [3x1]=' 40 170'
    [3x2]='110 170'
    [3x3]='180 170'
    [3x4]='250 170'
    [4x1]=' 40 230'
    [4x2]='110 230'
    [4x3]='180 230'
    [4x4]='250 230'
    [NewGame]='100 280'
    [Exit]='200 280'
)

# To collect manual clicks
# $ node src/main.js examples/fifteen.lsp | sed -En "s/Click: (.*)/'\1'/p" | while read x; do echo -n $x ' '; done

clicks=(
    '1x2'  '2x2'  '2x1'  '1x1'  '1x2'  '2x2'  '3x2'  '4x2'  '4x1'  '3x1'
    '3x2'  '4x2'  '4x3'  '3x3'  '2x3'  '2x2'  '3x2'  '3x3'  '2x3'  '1x3'
    '1x2'  '2x2'  '1x2'  '1x3'  '1x4'  '2x4'  '2x3'  '1x3'  '1x2'  '2x2'
    '2x3'  '3x3'  '3x2'  '2x2'  '2x3'  '3x3'  '3x2'  '3x1'  '2x1'  '2x2'
    '3x2'  '4x2'  '3x3'  '4x3'  '3x3'  '2x3'  '2x2'  '3x2'  '3x3'  '4x3'
    '4x2'  '3x2'  '3x3'  '4x3'  '4x4'  '3x4'  '2x4'  '2x3'  '3x3'  '3x4'
    '4x4'  '4x3'  '3x3'  '3x2'  '2x2'  '2x3'  '2x4'  '3x4'  '3x3'  '2x3'
    '2x2'  '3x2'  '3x1'  '4x1'  '3x1'  '3x2'  '3x3'  '4x3'  '4x4'  '3x4'
    '3x3'  '3x2'  '3x1'  '4x1'  '4x2'  '4x3'  '4x4'
)

for click in ${clicks[@]}; do
    xdotool mousemove -window $WID ${coord[$click]} click 1
done

sleep 1
xdotool mousemove -window $WID ${coord['NewGame']} click 1
xdotool mousemove -window $WID ${coord['Exit']} click 1
sleep 1

#echo 'Press Enter to exit...'; read

if kill $PID &>/dev/null; then
    echo "Failed: still running"
    exit 1
else
    echo "Success"
    exit 0
fi
