//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{" Bat:", "echo $(cat /sys/class/power_supply/BAT0/capacity)%", 60, 12},
	{"Bright:", "brightness check", 0, 11},
	{"Sound:", "echo $(amixer sget Master | sed -e 's/\\[//' -e 's/\\%]//' | awk '/\\[/ { if ($5 >= 0 && $5 <= 100) print $5; else print $4 }' | awk 'NR==1 {print $1}')%"
,		0,		10},
	{"CPU:", "echo $(ps -e -o %cpu | awk '{s+=$1} END {print s+0.05}')%", 5, 0},
	{"Mem:", "echo $(free | awk '/^Mem/ { print $2/$3 }' | awk -F . '{ print $1 }')%",	5,		0},
	{"", "date '+%b %d (%a) %I:%M:%S%p'",					1,		0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
