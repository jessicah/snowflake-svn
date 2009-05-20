
type command
	= Numeric of int
	| Join
	| Part
	| Mode
	| Topic
	| Names
	| List
	| Invite
	| Kick
	| Privmsg
	| Notice
	| Quit
	| Ping
	| Unknown of string
