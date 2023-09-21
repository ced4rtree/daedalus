static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{"🎵 "  , "mpc | head -n 1"                                              , 3  , 0} ,
	{""     , "weather"                                                       ,3600, 0},
	{"🌡️ "  , "printf \"%.0f°\" \"$(cat /sys/class/thermal/thermal*/temp | awk '{count+=($1/1000)} END {print count/8}')\"", 1, 0},
    {"💻 "  , "~/.config/blocks/scripts/cpu_usage"                           , 1  , 0} ,
    {"💾 "  , "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g"      , 1  , 0} ,
    {"🔊 "  , "echo \"$(snd check)\"%"                                       , 0  , 1} ,
	{"🌞 "  , "echo \"$(real-brightness check)\"%"                           , 0  , 2} ,
    {"🔋 "  , "~/.config/blocks/scripts/battery | tr \'\n\' \' \' "          , 15 , 0} ,
    {"🕓 "  , "~/.config/blocks/scripts/clock"                               , 5  , 0} ,
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
