# Battery
battery_charge=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "percentage" | awk '{print $2}')
battery_status=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "state" | awk '{print $2}')

audio_volume = $(snd check)
audio_icon = ""
if [[ audio_volume -le 0 ]]; then
	audio_icon = " "
elif [[ audio_volume -le 30 ]]; then
	audio_icon = " "
elif [[ audio_volume -le 60 ]]; then
	audio_icon = " "
else
	audio_icon = " "
fi

