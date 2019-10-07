#!/bin/sh

devID=$( xinput list | grep "TouchPad" | sed -n -e 's/.*id=\([0-9]*\).*/\1/p' )
propID=$( xinput list-props $devID | grep "Tapping Enabled (" | sed -n -e 's/.*(\([0-9]*\)).*/\1/p' )
xinput set-prop $devID $propID 1
