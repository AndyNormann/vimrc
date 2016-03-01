#!/bin/bash
#mute=`amixer get Master | grep "Front Left:" | awk '{print $6}'`
#vol=`amixer get Master | grep "Front Left:" | awk '{print $5}' | tr -d '[%]'`
#mute="[on]"
vol=`pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 2 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'`
mute=`pactl list sinks | grep '^[[:space:]]Mute:' | head -n $(( $SINK + 2 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'`

#mute=`pactl list sinks | grep '^[[:space:]]Mute:' | head -n $(( $SINK + 2 )) | tail -n 1'`

#if [ $mute == "Mute: no" ]
ret=`grep -q "yes" $mute`
if [ $ret ];
then
    if [ $vol -eq 100 ]
    then 
        echo "  $vol"
    elif [ $vol -ge 50 ]
    then 
        echo "   $vol"
    elif [ $vol -lt 50 ]
    then
        if [ $vol -lt 10 ]
        then
            echo "       $vol"
        else 
            echo "    $vol"
        fi
    else
        :
    fi
else
    #echo $mute
    echo $ret
    #echo "  Muted " 
fi
