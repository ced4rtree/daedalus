/* See LICENSE file for copyright and license details. */

/* Adds some weird keys like sound keys */
#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int gappx     = 10;       /* Distance between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int swallowfloating    = 0;		/* 1 means swallow floating windows by default */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int usealtbar          = 1;        /* 1 means use non-dwm status bar */
static const char *altbarclass      = "Polybar"; /* Alternate bar class name */
static const char *alttrayname      = "main";    /* Polybar tray instance name */
static const char *altbarcmd        = "HOME/.config/polybar/launch.sh"; /* Alternate bar launch command */
static const char *fonts[]          = { "monospace:size=10", "notocoloremoji:size=10" };
static const char dmenufont[]       = "MesloLGS NF:size=10";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#005577";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
};

/* tagging */
static const char *tags[] = { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "XI" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   isterminal  noswallow  monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,			 0,			 0,			-1 },
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,			 0,			 -1,		-1 },
	{ "urxvt",	  NULL,		  NULL,		  0,			0,			 1,			 0,			-1 },
	{ "st",		  NULL,		  NULL,		  0,			0,			 1,			 0,			-1 },
	{ NULL,		  NULL,		  "Event Tester", 0,		0,			 0,			 1,			-1 },
	{ NULL,		  NULL,		  "Display Image", 0,		0,			 0,			 1,			-1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", NULL };
static const char *termcmd[]  = { "urxvtc", "--perl-lib", "HOME/.config/urxvt", NULL };
/* user defined functions */
static const char *brightup[] = { "brightness", "up", NULL };
static const char *brightdwn[] = { "brightness", "down", NULL };
static const char *slock[] = { "slock", NULL };
static const char *scrot[] = { "scrot", "-s", NULL};
static const char *background[] = { "feh", "--bg-scale", "HOME/photos/$(ls", "HOME/photos", "|", "shuf)", NULL};
static const char *emacs[] = { "emacsclient", "-c", "-s", "HOME/.emacs.d/server-dir/server", NULL };
    /* Music */
static const char *volumeup[] = { "snd", "up", NULL };
static const char *volumedown[] = { "snd", "down", NULL };
static const char *nextSong[] = { "cmus-remote", "--next", NULL };
static const char *prevSong[] = { "cmus-remote", "--prev", NULL };
static const char *pauseMus[] = { "cmus-remote", "--pause", NULL };
static const char *killMus[] = { "cmus-remote", "--stop", ";", "killall", "cmus", NULL };
static const char *startMus[] = { "cmus-remote", "--play", "&&", "cmus-remote", "--shuffle", NULL };
static const char *shuffleMus[] = { "cmus-remote", "--shuffle", NULL };
static Key keys[] = {
/* End of user defined functions */
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	/* user defined functions */
	{ 0,							XF86XK_MonBrightnessDown,		spawn,	   {.v = brightdwn } },
	{ 0,							XF86XK_MonBrightnessUp,	    	spawn,	   {.v = brightup } },
	{ MODKEY,						XK_grave,  spawn,	       {.v = slock } },
	{ MODKEY,		        		XK_s,	   spawn,	  	   {.v = scrot} },
	{ MODKEY,			        	XK_w,	   spawn,	 	   {.v = background} },
	{ MODKEY,  	    	            XK_e,      spawn,   	   {.v = emacs } },
	    /* Music */
	{ 0,		      				XF86XK_AudioRaiseVolume,		spawn,	   {.v = volumeup } },
	{ 0,			       			XF86XK_AudioLowerVolume,		spawn,	   {.v = volumedown } },
	{ MODKEY|ShiftMask,             XK_h,      spawn,          { .v = prevSong } },
	{ MODKEY|ShiftMask,             XK_l,      spawn,          { .v = nextSong } },
	{ MODKEY|ShiftMask,             XK_j,      spawn,          { .v = pauseMus } },
	{ MODKEY|ShiftMask,             XK_k,      spawn,          { .v = killMus  } },
	{ MODKEY|ShiftMask,             XK_g,      spawn,          { .v = startMus } },
	{ MODKEY|ShiftMask,				XK_s,	   spawn,		   { .v = shuffleMus } },
	/* end of user defined functions */
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY,                       XK_minus,  setgaps,        {.i = -1 } },
	{ MODKEY,                       XK_equal,  setgaps,        {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_equal,  setgaps,        {.i = 0  } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

static const char *ipcsockpath = "/tmp/dwm.sock";
static IPCCommand ipccommands[] = {
  IPCCOMMAND(  view,                1,      {ARG_TYPE_UINT}   ),
  IPCCOMMAND(  toggleview,          1,      {ARG_TYPE_UINT}   ),
  IPCCOMMAND(  tag,                 1,      {ARG_TYPE_UINT}   ),
  IPCCOMMAND(  toggletag,           1,      {ARG_TYPE_UINT}   ),
  IPCCOMMAND(  tagmon,              1,      {ARG_TYPE_UINT}   ),
  IPCCOMMAND(  focusmon,            1,      {ARG_TYPE_SINT}   ),
  IPCCOMMAND(  focusstack,          1,      {ARG_TYPE_SINT}   ),
  IPCCOMMAND(  zoom,                1,      {ARG_TYPE_NONE}   ),
  IPCCOMMAND(  incnmaster,          1,      {ARG_TYPE_SINT}   ),
  IPCCOMMAND(  killclient,          1,      {ARG_TYPE_SINT}   ),
  IPCCOMMAND(  togglefloating,      1,      {ARG_TYPE_NONE}   ),
  IPCCOMMAND(  setmfact,            1,      {ARG_TYPE_FLOAT}  ),
  IPCCOMMAND(  setlayoutsafe,       1,      {ARG_TYPE_PTR}    ),
  IPCCOMMAND(  quit,                1,      {ARG_TYPE_NONE}   )
};

