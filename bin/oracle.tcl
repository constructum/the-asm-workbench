#!/usr/X11/bin/wish -f

set blinking off


proc display_prompt line {
    regexp {(\@[A-Z]*)[ \t]*} $line whole_match metaprefix
    set line [string range $line [string length $whole_match] [string length $line]]
    if {$metaprefix == "@QUERY"} {
	regexp {\"([a-zA-Z0-9\_]*)\"[ \t]*} $line whole_match f_name
	set line [string range $line [string length $whole_match] [string length $line]]
	if {$line == "()"} {
	  set line [format "%s" $f_name]
	} else {
	  set line [format "%s %s" $f_name $line]
	}
	.oracle_label1 configure -fg black -text "Please enter oracle value"
	.oracle_label2 configure -text $line
    } elseif {$metaprefix == "@RETRY"} {
	.oracle_label1 configure -fg black -text "ERROR!  Please re-enter oracle value!"
    } elseif {$metaprefix == "@QUIT"} {
	exit
    }
}

proc blink {colour1 colour2 noblink_colour} {
  global blinking
  if {$blinking == "on"} { 
      .oracle_label1 config -fg $colour1
      after 200 [list blink $colour2 $colour1 $noblink_colour]
  } else {
      .oracle_label1 config -fg $noblink_colour
  }
}

proc start_blinking {} {
  global blinking
  set blinking on
  blink red black black
}

proc stop_blinking {} {
  global blinking
  .oracle_label1 config -fg black
  set blinking off
}

proc getinput {} {
  global oracle_value
  gets stdin line
  gets stdin emptyline
  .enter_oracle delete 0 [string length $oracle_value]
  start_blinking
  display_prompt $line
}

wm title . "Oracle"
label .oracle_label1 -fg black -text "Please enter oracle value"
label .oracle_label2 -fg black -text "  "
entry .enter_oracle -width 40 -relief sunken -textvariable oracle_value
bind .enter_oracle <Return> {puts $oracle_value; puts ""; flush stdout; stop_blinking; getinput}
pack .oracle_label1 .oracle_label2 .enter_oracle
getinput

