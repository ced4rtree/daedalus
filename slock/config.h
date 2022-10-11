/* user and group to drop privileges to */
static const char *user  = "${USER}";
static const char *group = "$(groups | awk '{print $1}')";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "black",     /* after initialization */
	[INPUT] =  "#005577",   /* during input */
	[FAILED] = "#CC3333",   /* wrong password */
};

/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;
