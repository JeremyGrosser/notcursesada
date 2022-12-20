package Notcurses.Keys
   with Preelaborate
is
   --  Synthesized input events, i.e. any input event we can report that isn't
   --  representative of some Unicode. This covers both keyboard and mouse events,
   --  as well as signals and even window events.

   --  Rather than using one of the Private Use Areas of Unicode, we use the area
   --  beyond the 17 65536-entry Planes 1114112. We round up to 5000 so that it's
   --  trivial to identify synthesized characters based on their numeric definition
   --  here. This is safe, since we needn't convert these synthesized characters
   --  into UTF8 they would otherwise require more than four bytes.
   PRETERUNICODEBASE : constant := 1_115_000;

   --  Special composed key definitions. These values are added to 0x100000.
   NCKEY_INVALID   : constant := PRETERUNICODEBASE + 0;
   NCKEY_RESIZE    : constant := PRETERUNICODEBASE + 1; --  we received SIGWINCH
   NCKEY_UP        : constant := PRETERUNICODEBASE + 2;
   NCKEY_RIGHT     : constant := PRETERUNICODEBASE + 3;
   NCKEY_DOWN      : constant := PRETERUNICODEBASE + 4;
   NCKEY_LEFT      : constant := PRETERUNICODEBASE + 5;
   NCKEY_INS       : constant := PRETERUNICODEBASE + 6;
   NCKEY_DEL       : constant := PRETERUNICODEBASE + 7;
   NCKEY_BACKSPACE : constant := PRETERUNICODEBASE + 8; --  backspace sometimes
   NCKEY_PGDOWN    : constant := PRETERUNICODEBASE + 9;
   NCKEY_PGUP      : constant := PRETERUNICODEBASE + 10;
   NCKEY_HOME      : constant := PRETERUNICODEBASE + 11;
   NCKEY_END       : constant := PRETERUNICODEBASE + 12;
   NCKEY_F00       : constant := PRETERUNICODEBASE + 20;
   NCKEY_F01       : constant := PRETERUNICODEBASE + 21;
   NCKEY_F02       : constant := PRETERUNICODEBASE + 22;
   NCKEY_F03       : constant := PRETERUNICODEBASE + 23;
   NCKEY_F04       : constant := PRETERUNICODEBASE + 24;
   NCKEY_F05       : constant := PRETERUNICODEBASE + 25;
   NCKEY_F06       : constant := PRETERUNICODEBASE + 26;
   NCKEY_F07       : constant := PRETERUNICODEBASE + 27;
   NCKEY_F08       : constant := PRETERUNICODEBASE + 28;
   NCKEY_F09       : constant := PRETERUNICODEBASE + 29;
   NCKEY_F10       : constant := PRETERUNICODEBASE + 30;
   NCKEY_F11       : constant := PRETERUNICODEBASE + 31;
   NCKEY_F12       : constant := PRETERUNICODEBASE + 32;
   NCKEY_F13       : constant := PRETERUNICODEBASE + 33;
   NCKEY_F14       : constant := PRETERUNICODEBASE + 34;
   NCKEY_F15       : constant := PRETERUNICODEBASE + 35;
   NCKEY_F16       : constant := PRETERUNICODEBASE + 36;
   NCKEY_F17       : constant := PRETERUNICODEBASE + 37;
   NCKEY_F18       : constant := PRETERUNICODEBASE + 38;
   NCKEY_F19       : constant := PRETERUNICODEBASE + 39;
   NCKEY_F20       : constant := PRETERUNICODEBASE + 40;
   NCKEY_F21       : constant := PRETERUNICODEBASE + 41;
   NCKEY_F22       : constant := PRETERUNICODEBASE + 42;
   NCKEY_F23       : constant := PRETERUNICODEBASE + 43;
   NCKEY_F24       : constant := PRETERUNICODEBASE + 44;
   NCKEY_F25       : constant := PRETERUNICODEBASE + 45;
   NCKEY_F26       : constant := PRETERUNICODEBASE + 46;
   NCKEY_F27       : constant := PRETERUNICODEBASE + 47;
   NCKEY_F28       : constant := PRETERUNICODEBASE + 48;
   NCKEY_F29       : constant := PRETERUNICODEBASE + 49;
   NCKEY_F30       : constant := PRETERUNICODEBASE + 50;
   NCKEY_F31       : constant := PRETERUNICODEBASE + 51;
   NCKEY_F32       : constant := PRETERUNICODEBASE + 52;
   NCKEY_F33       : constant := PRETERUNICODEBASE + 53;
   NCKEY_F34       : constant := PRETERUNICODEBASE + 54;
   NCKEY_F35       : constant := PRETERUNICODEBASE + 55;
   NCKEY_F36       : constant := PRETERUNICODEBASE + 56;
   NCKEY_F37       : constant := PRETERUNICODEBASE + 57;
   NCKEY_F38       : constant := PRETERUNICODEBASE + 58;
   NCKEY_F39       : constant := PRETERUNICODEBASE + 59;
   NCKEY_F40       : constant := PRETERUNICODEBASE + 60;
   NCKEY_F41       : constant := PRETERUNICODEBASE + 61;
   NCKEY_F42       : constant := PRETERUNICODEBASE + 62;
   NCKEY_F43       : constant := PRETERUNICODEBASE + 63;
   NCKEY_F44       : constant := PRETERUNICODEBASE + 64;
   NCKEY_F45       : constant := PRETERUNICODEBASE + 65;
   NCKEY_F46       : constant := PRETERUNICODEBASE + 66;
   NCKEY_F47       : constant := PRETERUNICODEBASE + 67;
   NCKEY_F48       : constant := PRETERUNICODEBASE + 68;
   NCKEY_F49       : constant := PRETERUNICODEBASE + 69;
   NCKEY_F50       : constant := PRETERUNICODEBASE + 70;
   NCKEY_F51       : constant := PRETERUNICODEBASE + 71;
   NCKEY_F52       : constant := PRETERUNICODEBASE + 72;
   NCKEY_F53       : constant := PRETERUNICODEBASE + 73;
   NCKEY_F54       : constant := PRETERUNICODEBASE + 74;
   NCKEY_F55       : constant := PRETERUNICODEBASE + 75;
   NCKEY_F56       : constant := PRETERUNICODEBASE + 76;
   NCKEY_F57       : constant := PRETERUNICODEBASE + 77;
   NCKEY_F58       : constant := PRETERUNICODEBASE + 78;
   NCKEY_F59       : constant := PRETERUNICODEBASE + 79;
   NCKEY_F60       : constant := PRETERUNICODEBASE + 80;
   --  ... leave room for up to 100 function keys, egads
   NCKEY_ENTER     : constant := PRETERUNICODEBASE + 121;
   NCKEY_CLS       : constant := PRETERUNICODEBASE + 122; --  "clear-screen or erase"
   NCKEY_DLEFT     : constant := PRETERUNICODEBASE + 123; --  down + left on keypad
   NCKEY_DRIGHT    : constant := PRETERUNICODEBASE + 124;
   NCKEY_ULEFT     : constant := PRETERUNICODEBASE + 125; --  up + left on keypad
   NCKEY_URIGHT    : constant := PRETERUNICODEBASE + 126;
   NCKEY_CENTER    : constant := PRETERUNICODEBASE + 127; --  the most truly neutral of keypresses
   NCKEY_BEGIN     : constant := PRETERUNICODEBASE + 128;
   NCKEY_CANCEL    : constant := PRETERUNICODEBASE + 129;
   NCKEY_CLOSE     : constant := PRETERUNICODEBASE + 130;
   NCKEY_COMMAND   : constant := PRETERUNICODEBASE + 131;
   NCKEY_COPY      : constant := PRETERUNICODEBASE + 132;
   NCKEY_EXIT      : constant := PRETERUNICODEBASE + 133;
   NCKEY_PRINT     : constant := PRETERUNICODEBASE + 134;
   NCKEY_REFRESH   : constant := PRETERUNICODEBASE + 135;
   NCKEY_SEPARATOR : constant := PRETERUNICODEBASE + 136;
   --  these keys aren't generally available outside of the kitty protocol
   NCKEY_CAPS_LOCK    : constant := PRETERUNICODEBASE + 150;
   NCKEY_SCROLL_LOCK  : constant := PRETERUNICODEBASE + 151;
   NCKEY_NUM_LOCK     : constant := PRETERUNICODEBASE + 152;
   NCKEY_PRINT_SCREEN : constant := PRETERUNICODEBASE + 153;
   NCKEY_PAUSE        : constant := PRETERUNICODEBASE + 154;
   NCKEY_MENU         : constant := PRETERUNICODEBASE + 155;
   --  media keys, similarly only available through kitty's protocol
   NCKEY_MEDIA_PLAY   : constant := PRETERUNICODEBASE + 158;
   NCKEY_MEDIA_PAUSE  : constant := PRETERUNICODEBASE + 159;
   NCKEY_MEDIA_PPAUSE : constant := PRETERUNICODEBASE + 160;
   NCKEY_MEDIA_REV    : constant := PRETERUNICODEBASE + 161;
   NCKEY_MEDIA_STOP   : constant := PRETERUNICODEBASE + 162;
   NCKEY_MEDIA_FF     : constant := PRETERUNICODEBASE + 163;
   NCKEY_MEDIA_REWIND : constant := PRETERUNICODEBASE + 164;
   NCKEY_MEDIA_NEXT   : constant := PRETERUNICODEBASE + 165;
   NCKEY_MEDIA_PREV   : constant := PRETERUNICODEBASE + 166;
   NCKEY_MEDIA_RECORD : constant := PRETERUNICODEBASE + 167;
   NCKEY_MEDIA_LVOL   : constant := PRETERUNICODEBASE + 168;
   NCKEY_MEDIA_RVOL   : constant := PRETERUNICODEBASE + 169;
   NCKEY_MEDIA_MUTE   : constant := PRETERUNICODEBASE + 170;
   --  modifiers when pressed by themselves. this ordering comes from the Kitty
   --  keyboard protocol, and mustn't be changed without updating handlers.
   NCKEY_LSHIFT       : constant := PRETERUNICODEBASE + 171;
   NCKEY_LCTRL        : constant := PRETERUNICODEBASE + 172;
   NCKEY_LALT         : constant := PRETERUNICODEBASE + 173;
   NCKEY_LSUPER       : constant := PRETERUNICODEBASE + 174;
   NCKEY_LHYPER       : constant := PRETERUNICODEBASE + 175;
   NCKEY_LMETA        : constant := PRETERUNICODEBASE + 176;
   NCKEY_RSHIFT       : constant := PRETERUNICODEBASE + 177;
   NCKEY_RCTRL        : constant := PRETERUNICODEBASE + 178;
   NCKEY_RALT         : constant := PRETERUNICODEBASE + 179;
   NCKEY_RSUPER       : constant := PRETERUNICODEBASE + 180;
   NCKEY_RHYPER       : constant := PRETERUNICODEBASE + 181;
   NCKEY_RMETA        : constant := PRETERUNICODEBASE + 182;
   NCKEY_L3SHIFT      : constant := PRETERUNICODEBASE + 183;
   NCKEY_L5SHIFT      : constant := PRETERUNICODEBASE + 184;
   --  mouse events. We encode which button was pressed into the char32_t,
   --  but position information is embedded in the larger ncinput event.
   NCKEY_MOTION    : constant := PRETERUNICODEBASE + 200; --  no buttons pressed
   NCKEY_BUTTON1   : constant := PRETERUNICODEBASE + 201;
   NCKEY_BUTTON2   : constant := PRETERUNICODEBASE + 202;
   NCKEY_BUTTON3   : constant := PRETERUNICODEBASE + 203;
   NCKEY_BUTTON4   : constant := PRETERUNICODEBASE + 204; --  scrollwheel up
   NCKEY_BUTTON5   : constant := PRETERUNICODEBASE + 205; --  scrollwheel down
   NCKEY_BUTTON6   : constant := PRETERUNICODEBASE + 206;
   NCKEY_BUTTON7   : constant := PRETERUNICODEBASE + 207;
   NCKEY_BUTTON8   : constant := PRETERUNICODEBASE + 208;
   NCKEY_BUTTON9   : constant := PRETERUNICODEBASE + 209;
   NCKEY_BUTTON10  : constant := PRETERUNICODEBASE + 210;
   NCKEY_BUTTON11  : constant := PRETERUNICODEBASE + 211;

   --  we received SIGCONT
   NCKEY_SIGNAL    : constant := PRETERUNICODEBASE + 400;

   --  indicates that we have reached the end of input. any further calls
   --  will continute to return this immediately.
   NCKEY_EOF       : constant := PRETERUNICODEBASE + 500;

   --  Synonyms and aliases so far as we're concerned
   NCKEY_SCROLL_UP   : constant := NCKEY_BUTTON4;
   NCKEY_SCROLL_DOWN : constant := NCKEY_BUTTON5;
   NCKEY_RETURN      : constant := NCKEY_ENTER;
   NCKEY_TAB         : constant := 16#09#;
   NCKEY_ESC         : constant := 16#1B#;
   NCKEY_SPACE       : constant := 16#20#;

end Notcurses.Keys;
