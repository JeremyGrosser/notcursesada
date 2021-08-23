pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with Interfaces.C_Streams;
with Interfaces;
with System.OS_Interface;
with System;

package Notcurses_Thin is

   --  unsupported macro: NCALIGN_TOP NCALIGN_LEFT
   --  unsupported macro: NCALIGN_BOTTOM NCALIGN_RIGHT
   NCALPHA_HIGHCONTRAST : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:104
   NCALPHA_TRANSPARENT : constant := 16#20000000#;  --  /usr/local/include/notcurses/notcurses.h:105
   NCALPHA_BLEND : constant := 16#10000000#;  --  /usr/local/include/notcurses/notcurses.h:106
   NCALPHA_OPAQUE : constant := 16#00000000#;  --  /usr/local/include/notcurses/notcurses.h:107

   NCPALETTESIZE : constant := 256;  --  /usr/local/include/notcurses/notcurses.h:110

   NC_NOBACKGROUND_MASK : constant := 16#8700000000000000#;  --  /usr/local/include/notcurses/notcurses.h:115

   NC_BGDEFAULT_MASK : constant := 16#0000000040000000#;  --  /usr/local/include/notcurses/notcurses.h:117

   NC_BG_RGB_MASK : constant := 16#0000000000ffffff#;  --  /usr/local/include/notcurses/notcurses.h:119

   NC_BG_PALETTE : constant := 16#0000000008000000#;  --  /usr/local/include/notcurses/notcurses.h:122

   NC_BG_ALPHA_MASK : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:124
   --  arg-macro: function NCCHANNEL_INITIALIZER (r, g, b)
   --    return ((uint32_t)(r) << 16) + ((uint32_t)(g) << 8) + (b) + NC_BGDEFAULT_MASK;
   --  arg-macro: function NCCHANNELS_INITIALIZER (fr, fg, fb, br, bg, bb)
   --    return (NCCHANNEL_INITIALIZER((fr), (fg), (fb)) << 32) + (NCCHANNEL_INITIALIZER((br), (bg), (bb)));

   WCHAR_MAX_UTF8BYTES : constant := 4;  --  /usr/local/include/notcurses/notcurses.h:594
   --  arg-macro: procedure NCCELL_INITIALIZER (c, s, chan)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)((wcwidth(c) < 0  or else  notc) ? 1 : wcwidth(c)), .stylemask := (s), .channels := (chan), }
   --  arg-macro: procedure NCCELL_CHAR_INITIALIZER (c)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)((wcwidth(c) < 0  or else  notc) ? 1 : wcwidth(c)), .stylemask := 0, .channels := 0, }
   --  unsupported macro: NCCELL_TRIVIAL_INITIALIZER { .gcluster = 0, .gcluster_backstop = 0, .width = 1, .stylemask = 0, .channels = 0, }

   NCSTYLE_MASK : constant := 16#ffff#;  --  /usr/local/include/notcurses/notcurses.h:769
   NCSTYLE_ITALIC : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:770
   NCSTYLE_UNDERLINE : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:771
   NCSTYLE_UNDERCURL : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:772
   NCSTYLE_BOLD : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:773
   NCSTYLE_STRUCK : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:774
   NCSTYLE_NONE : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:775

   NCOPTION_INHIBIT_SETLOCALE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:990

   NCOPTION_NO_CLEAR_BITMAPS : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:996

   NCOPTION_NO_WINCH_SIGHANDLER : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:1000

   NCOPTION_NO_QUIT_SIGHANDLERS : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:1005

   NCOPTION_PRESERVE_CURSOR : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:1011

   NCOPTION_SUPPRESS_BANNERS : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:1015

   NCOPTION_NO_ALTERNATE_SCREEN : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:1019

   NCOPTION_NO_FONT_CHANGES : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:1025

   NCOPTION_DRAIN_INPUT : constant := 16#0100#;  --  /usr/local/include/notcurses/notcurses.h:1030

   NCOPTION_SCROLLING : constant := 16#0200#;  --  /usr/local/include/notcurses/notcurses.h:1034
   --  unsupported macro: NCOPTION_CLI_MODE (NCOPTION_NO_ALTERNATE_SCREEN |NCOPTION_NO_CLEAR_BITMAPS |NCOPTION_PRESERVE_CURSOR |NCOPTION_SCROLLING)

   NCMICE_NO_EVENTS : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:1332
   NCMICE_MOVE_EVENT : constant := 16#1#;  --  /usr/local/include/notcurses/notcurses.h:1333
   NCMICE_BUTTON_EVENT : constant := 16#2#;  --  /usr/local/include/notcurses/notcurses.h:1334
   NCMICE_DRAG_EVENT : constant := 16#4#;  --  /usr/local/include/notcurses/notcurses.h:1335
   NCMICE_ALL_EVENTS : constant := 16#7#;  --  /usr/local/include/notcurses/notcurses.h:1336

   NCPLANE_OPTION_HORALIGNED : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:1439

   NCPLANE_OPTION_VERALIGNED : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:1441

   NCPLANE_OPTION_MARGINALIZED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:1448

   NCPLANE_OPTION_FIXED : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:1452

   NCPLANE_OPTION_AUTOGROW : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:1456

   NCPLANE_OPTION_VSCROLL : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:1460

   NCBOXMASK_TOP : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:2592
   NCBOXMASK_RIGHT : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2593
   NCBOXMASK_BOTTOM : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:2594
   NCBOXMASK_LEFT : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:2595
   NCBOXGRAD_TOP : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:2596
   NCBOXGRAD_RIGHT : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:2597
   NCBOXGRAD_BOTTOM : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:2598
   NCBOXGRAD_LEFT : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:2599
   NCBOXCORNER_MASK : constant := 16#0300#;  --  /usr/local/include/notcurses/notcurses.h:2600
   NCBOXCORNER_SHIFT : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:2601

   NCVISUAL_OPTION_NODEGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3316
   NCVISUAL_OPTION_BLEND : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3317
   NCVISUAL_OPTION_HORALIGNED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:3318
   NCVISUAL_OPTION_VERALIGNED : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:3319
   NCVISUAL_OPTION_ADDALPHA : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:3320
   NCVISUAL_OPTION_CHILDPLANE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:3321
   NCVISUAL_OPTION_NOINTERPOLATE : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:3322

   NCREEL_OPTION_INFINITESCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3672

   NCREEL_OPTION_CIRCULAR : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3676

   NCPREFIXCOLUMNS : constant := 7;  --  /usr/local/include/notcurses/notcurses.h:3804
   NCIPREFIXCOLUMNS : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:3805
   NCBPREFIXCOLUMNS : constant := 9;  --  /usr/local/include/notcurses/notcurses.h:3806
   --  unsupported macro: NCPREFIXSTRLEN (NCPREFIXCOLUMNS + 1)
   --  unsupported macro: NCIPREFIXSTRLEN (NCIPREFIXCOLUMNS + 1)
   --  unsupported macro: NCBPREFIXSTRLEN (NCBPREFIXCOLUMNS + 1)
   --  arg-macro: function NCMETRICFWIDTH (x, cols)
   --    return (int)(strlen(x) - ncstrwidth(x, NULL, NULL) + (cols));
   --  arg-macro: procedure NCPREFIXFMT (x)
   --    NCMETRICFWIDTH((x), NCPREFIXCOLUMNS), (x)
   --  arg-macro: procedure NCIPREFIXFMT (x)
   --    NCMETRIXFWIDTH((x), NCIPREFIXCOLUMNS), (x)
   --  arg-macro: procedure NCBPREFIXFMT (x)
   --    NCMETRICFWIDTH((x), NCBPREFIXCOLUMNS), (x)

   NCMENU_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:4106
   NCMENU_OPTION_HIDING : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:4107

   NCPROGBAR_OPTION_RETROGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:4182

   NCTABBED_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:4216

   NCPLOT_OPTION_LABELTICKSD : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:4459
   NCPLOT_OPTION_EXPONENTIALD : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:4460
   NCPLOT_OPTION_VERTICALI : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:4461
   NCPLOT_OPTION_NODEGRADE : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:4462
   NCPLOT_OPTION_DETECTMAXONLY : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:4463
   NCPLOT_OPTION_PRINTSAMPLE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:4464

   NCREADER_OPTION_HORSCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:4588

   NCREADER_OPTION_VERSCROLL : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:4590

   NCREADER_OPTION_NOCMDKEYS : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:4592

   NCREADER_OPTION_CURSOR : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:4595

   NCSTYLE_PROTECT : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4476
   NCSTYLE_STANDOUT : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4477
   NCSTYLE_REVERSE : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4478
   NCSTYLE_INVIS : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4479
   NCSTYLE_DIM : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4480
   NCSTYLE_BLINK : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4481

   function notcurses_version return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:30
   with Import => True,
        Convention => C,
        External_Name => "notcurses_version";

   procedure notcurses_version_components
     (major : access int;
      minor : access int;
      patch : access int;
      tweak : access int)  -- /usr/local/include/notcurses/notcurses.h:33
   with Import => True,
        Convention => C,
        External_Name => "notcurses_version_components";

   type notcurses is null record;   -- incomplete struct

   type ncplane is null record;   -- incomplete struct

   type ncvisual is null record;   -- incomplete struct

   type ncuplot is null record;   -- incomplete struct

   type ncdplot is null record;   -- incomplete struct

   type ncprogbar is null record;   -- incomplete struct

   type ncfdplane is null record;   -- incomplete struct

   type ncsubproc is null record;   -- incomplete struct

   type ncselector is null record;   -- incomplete struct

   type ncmultiselector is null record;   -- incomplete struct

   type ncreader is null record;   -- incomplete struct

   type ncfadectx is null record;   -- incomplete struct

   type nctablet is null record;   -- incomplete struct

   type ncreel is null record;   -- incomplete struct

   type nctab is null record;   -- incomplete struct

   type nctabbed is null record;   -- incomplete struct

   type ncdirect is null record;   -- incomplete struct

   type ncblitter_e is
     (NCBLIT_DEFAULT,
      NCBLIT_1x1,
      NCBLIT_2x1,
      NCBLIT_2x2,
      NCBLIT_3x2,
      NCBLIT_BRAILLE,
      NCBLIT_PIXEL,
      NCBLIT_4x1,
      NCBLIT_8x1)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:76

   type ncalign_e is
     (NCALIGN_UNALIGNED,
      NCALIGN_LEFT,
      NCALIGN_CENTER,
      NCALIGN_RIGHT)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:84

   type ncscale_e is
     (NCSCALE_NONE,
      NCSCALE_SCALE,
      NCSCALE_STRETCH,
      NCSCALE_NONE_HIRES,
      NCSCALE_SCALE_HIRES)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:101

   function ncchannel_alpha (channel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:142
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_alpha";

   function ncchannel_set_alpha (channel : access Interfaces.Unsigned_32; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:150
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_alpha";

   function ncchannel_default_p (channel : Interfaces.Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:163
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_default_p";

   function ncchannel_set_default (channel : access Interfaces.Unsigned_32) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:169
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_default";

   function ncchannel_palindex_p (channel : Interfaces.Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:177
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex_p";

   function ncchannel_palindex (channel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:184
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex";

   function ncchannel_set_palindex (channel : access Interfaces.Unsigned_32; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:191
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_palindex";

   function ncchannel_rgb_p (channel : Interfaces.Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:203
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_rgb_p";

   function ncchannel_r (channel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:211
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_r";

   function ncchannel_g (channel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:218
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_g";

   function ncchannel_b (channel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:225
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_b";

   function ncchannel_rgb (channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:232
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_rgb";

   function ncchannel_rgb8
     (channel : Interfaces.Unsigned_32;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:239
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_rgb8";

   function ncchannel_set_rgb8
     (channel : access Interfaces.Unsigned_32;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:251
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8";

   function ncchannel_set (channel : access Interfaces.Unsigned_32; rgb : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:264
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set";

   procedure ncchannel_set_rgb8_clipped
     (channel : access Interfaces.Unsigned_32;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:276
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8_clipped";

   function ncchannels_bchannel (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:302
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bchannel";

   function ncchannels_fchannel (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:310
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fchannel";

   function ncchannels_channels (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:316
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_channels";

   function ncchannels_bg_rgb_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:322
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb_p";

   function ncchannels_fg_rgb_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:327
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb_p";

   function ncchannels_bg_alpha (channels : Interfaces.Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:333
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_alpha";

   function ncchannels_set_bchannel (channels : access Interfaces.Unsigned_64; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:340
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bchannel";

   function ncchannels_set_fchannel (channels : access Interfaces.Unsigned_64; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:350
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fchannel";

   function ncchannels_set_channels (dst : access Interfaces.Unsigned_64; channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:359
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_channels";

   function ncchannels_set_bg_alpha (channels : access Interfaces.Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:367
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_alpha";

   function ncchannels_fg_alpha (channels : Interfaces.Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:381
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_alpha";

   function ncchannels_set_fg_alpha (channels : access Interfaces.Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:387
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_alpha";

   function ncchannels_reverse (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:401
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_reverse";

   function ncchannels_combine (fchan : Interfaces.Unsigned_32; bchan : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:424
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_combine";

   function ncchannels_fg_palindex (channels : Interfaces.Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:432
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex";

   function ncchannels_bg_palindex (channels : Interfaces.Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:437
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex";

   function ncchannels_fg_rgb (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:443
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb";

   function ncchannels_bg_rgb (channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:449
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb";

   function ncchannels_fg_rgb8
     (channels : Interfaces.Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:455
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb8";

   function ncchannels_bg_rgb8
     (channels : Interfaces.Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:461
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb8";

   function ncchannels_set_fg_rgb8
     (channels : access Interfaces.Unsigned_64;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:468
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8";

   procedure ncchannels_set_fg_rgb8_clipped
     (channels : access Interfaces.Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:479
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8_clipped";

   function ncchannels_set_fg_palindex (channels : access Interfaces.Unsigned_64; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:486
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_palindex";

   function ncchannels_set_fg_rgb (channels : access Interfaces.Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:497
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb";

   function ncchannels_set_bg_rgb8
     (channels : access Interfaces.Unsigned_64;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:509
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8";

   procedure ncchannels_set_bg_rgb8_clipped
     (channels : access Interfaces.Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:520
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8_clipped";

   function ncchannels_set_bg_palindex (channels : access Interfaces.Unsigned_64; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:529
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_palindex";

   function ncchannels_set_bg_rgb (channels : access Interfaces.Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:540
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb";

   function ncchannels_fg_default_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:551
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_default_p";

   function ncchannels_fg_palindex_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:557
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex_p";

   function ncchannels_bg_default_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:565
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_default_p";

   function ncchannels_bg_palindex_p (channels : Interfaces.Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:571
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex_p";

   function ncchannels_set_fg_default (channels : access Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:577
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_default";

   function ncchannels_set_bg_default (channels : access Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:586
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_default";

   function ncstrwidth
     (egcs : Interfaces.C.Strings.chars_ptr;
      validbytes : access int;
      validwidth : access int) return int  -- /usr/local/include/notcurses/notcurses.h:601
   with Import => True,
        Convention => C,
        External_Name => "ncstrwidth";

   function notcurses_ucs32_to_utf8
     (ucs32 : access Interfaces.Unsigned_32;
      ucs32count : unsigned;
      resultbuf : access unsigned_char;
      buflen : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:609
   with Import => True,
        Convention => C,
        External_Name => "notcurses_ucs32_to_utf8";

   type nccell is record
      gcluster : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:693
      gcluster_backstop : aliased Interfaces.Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:694
      width : aliased Interfaces.Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:702
      stylemask : aliased Interfaces.Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:703
      channels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:723
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:663

   procedure nccell_init (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:741
   with Import => True,
        Convention => C,
        External_Name => "nccell_init";

   function nccell_load
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:748
   with Import => True,
        Convention => C,
        External_Name => "nccell_load";

   function nccell_prime
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr;
      stylemask : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:752
   with Import => True,
        Convention => C,
        External_Name => "nccell_prime";

   function nccell_duplicate
     (n : access ncplane;
      targ : access nccell;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:762
   with Import => True,
        Convention => C,
        External_Name => "nccell_duplicate";

   procedure nccell_release (n : access ncplane; c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:765
   with Import => True,
        Convention => C,
        External_Name => "nccell_release";

   procedure nccell_set_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:780
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_styles";

   function nccell_styles (c : access constant nccell) return Interfaces.Unsigned_16  -- /usr/local/include/notcurses/notcurses.h:786
   with Import => True,
        Convention => C,
        External_Name => "nccell_styles";

   procedure nccell_on_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:793
   with Import => True,
        Convention => C,
        External_Name => "nccell_on_styles";

   procedure nccell_off_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:799
   with Import => True,
        Convention => C,
        External_Name => "nccell_off_styles";

   procedure nccell_set_fg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:805
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_default";

   procedure nccell_set_bg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:811
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_default";

   function nccell_set_fg_alpha (c : access nccell; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:816
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_alpha";

   function nccell_set_bg_alpha (c : access nccell; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:821
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_alpha";

   function nccell_set_bchannel (c : access nccell; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:826
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bchannel";

   function nccell_set_fchannel (c : access nccell; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:831
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fchannel";

   function nccell_set_channels (c : access nccell; channels : Interfaces.Unsigned_64) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:836
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_channels";

   function nccell_double_wide_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:842
   with Import => True,
        Convention => C,
        External_Name => "nccell_double_wide_p";

   function nccell_wide_right_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:848
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_right_p";

   function nccell_wide_left_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:854
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_left_p";

   function nccell_extended_gcluster (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:861
   with Import => True,
        Convention => C,
        External_Name => "nccell_extended_gcluster";

   function nccell_channels (c : access constant nccell) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:864
   with Import => True,
        Convention => C,
        External_Name => "nccell_channels";

   function nccell_bchannel (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:871
   with Import => True,
        Convention => C,
        External_Name => "nccell_bchannel";

   function nccell_fchannel (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:878
   with Import => True,
        Convention => C,
        External_Name => "nccell_fchannel";

   function nccell_cols (c : access constant nccell) return unsigned  -- /usr/local/include/notcurses/notcurses.h:885
   with Import => True,
        Convention => C,
        External_Name => "nccell_cols";

   function nccell_strdup (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:892
   with Import => True,
        Convention => C,
        External_Name => "nccell_strdup";

   function nccell_extract
     (n : access constant ncplane;
      c : access constant nccell;
      stylemask : access Interfaces.Unsigned_16;
      channels : access Interfaces.Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:898
   with Import => True,
        Convention => C,
        External_Name => "nccell_extract";

   function nccellcmp
     (n1 : access constant ncplane;
      c1 : access constant nccell;
      n2 : access constant ncplane;
      c2 : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:915
   with Import => True,
        Convention => C,
        External_Name => "nccellcmp";

   function nccell_load_char
     (n : access ncplane;
      c : access nccell;
      ch : char) return int  -- /usr/local/include/notcurses/notcurses.h:929
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_char";

   function nccell_load_egc32
     (n : access ncplane;
      c : access nccell;
      egc : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:939
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_egc32";

   function nccell_load_ucs32
     (n : access ncplane;
      c : access nccell;
      u : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:950
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_ucs32";

   subtype ncloglevel_e is int;
   NCLOGLEVEL_SILENT : constant int := -1;
   NCLOGLEVEL_PANIC : constant int := 0;
   NCLOGLEVEL_FATAL : constant int := 1;
   NCLOGLEVEL_ERROR : constant int := 2;
   NCLOGLEVEL_WARNING : constant int := 3;
   NCLOGLEVEL_INFO : constant int := 4;
   NCLOGLEVEL_VERBOSE : constant int := 5;
   NCLOGLEVEL_DEBUG : constant int := 6;
   NCLOGLEVEL_TRACE : constant int := 7;  -- /usr/local/include/notcurses/notcurses.h:978

   type notcurses_options is record
      termtype : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:1047
      loglevel : aliased ncloglevel_e;  -- /usr/local/include/notcurses/notcurses.h:1050
      margin_t : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1055
      margin_r : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1055
      margin_b : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1055
      margin_l : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1055
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1059
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1043

   function notcurses_lex_margins (op : Interfaces.C.Strings.chars_ptr; opts : access notcurses_options) return int  -- /usr/local/include/notcurses/notcurses.h:1065
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_margins";

   function notcurses_lex_blitter (op : Interfaces.C.Strings.chars_ptr; blitter : access ncblitter_e) return int  -- /usr/local/include/notcurses/notcurses.h:1069
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_blitter";

   function notcurses_str_blitter (blitter : ncblitter_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1073
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_blitter";

   function notcurses_lex_scalemode (op : Interfaces.C.Strings.chars_ptr; scalemode : access ncscale_e) return int  -- /usr/local/include/notcurses/notcurses.h:1077
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_scalemode";

   function notcurses_str_scalemode (scalemode : ncscale_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1081
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_scalemode";

   function notcurses_init (opts : access constant notcurses_options; fp : access Interfaces.C_Streams.FILEs) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:1087
   with Import => True,
        Convention => C,
        External_Name => "notcurses_init";

   function notcurses_core_init (opts : access constant notcurses_options; fp : access Interfaces.C_Streams.FILEs) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:1091
   with Import => True,
        Convention => C,
        External_Name => "notcurses_core_init";

   function notcurses_stop (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1094
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stop";

   function notcurses_enter_alternate_screen (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1100
   with Import => True,
        Convention => C,
        External_Name => "notcurses_enter_alternate_screen";

   function notcurses_leave_alternate_screen (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1105
   with Import => True,
        Convention => C,
        External_Name => "notcurses_leave_alternate_screen";

   function ncpile_top (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1117
   with Import => True,
        Convention => C,
        External_Name => "ncpile_top";

   function ncpile_bottom (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1121
   with Import => True,
        Convention => C,
        External_Name => "ncpile_bottom";

   function notcurses_top (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1126
   with Import => True,
        Convention => C,
        External_Name => "notcurses_top";

   function notcurses_bottom (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1132
   with Import => True,
        Convention => C,
        External_Name => "notcurses_bottom";

   function ncpile_render (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1138
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render";

   function ncpile_rasterize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1144
   with Import => True,
        Convention => C,
        External_Name => "ncpile_rasterize";

   function notcurses_render (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1149
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render";

   function ncpile_render_to_buffer
     (p : access ncplane;
      buf : System.Address;
      buflen : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:1161
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render_to_buffer";

   function ncpile_render_to_file (p : access ncplane; fp : access Interfaces.C_Streams.FILEs) return int  -- /usr/local/include/notcurses/notcurses.h:1166
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render_to_file";

   procedure notcurses_drop_planes (nc : access notcurses)  -- /usr/local/include/notcurses/notcurses.h:1170
   with Import => True,
        Convention => C,
        External_Name => "notcurses_drop_planes";

   function nckey_mouse_p (r : Interfaces.Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1190
   with Import => True,
        Convention => C,
        External_Name => "nckey_mouse_p";

   type ncintype_e is
     (NCTYPE_UNKNOWN,
      NCTYPE_PRESS,
      NCTYPE_REPEAT,
      NCTYPE_RELEASE)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:1199

   subtype ncinput_array2934 is Interfaces.C.char_array (0 .. 4);
   type ncinput is record
      id : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:1206
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1207
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1207
      utf8 : aliased ncinput_array2934;  -- /usr/local/include/notcurses/notcurses.h:1208
      alt : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1210
      shift : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1211
      ctrl : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1212
      evtype : aliased ncintype_e;  -- /usr/local/include/notcurses/notcurses.h:1214
      modifiers : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1215
      ypx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1216
      xpx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1216
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1205

   function ncinput_shift_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1220
   with Import => True,
        Convention => C,
        External_Name => "ncinput_shift_p";

   function ncinput_ctrl_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1225
   with Import => True,
        Convention => C,
        External_Name => "ncinput_ctrl_p";

   function ncinput_alt_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1230
   with Import => True,
        Convention => C,
        External_Name => "ncinput_alt_p";

   function ncinput_meta_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1235
   with Import => True,
        Convention => C,
        External_Name => "ncinput_meta_p";

   function ncinput_super_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1240
   with Import => True,
        Convention => C,
        External_Name => "ncinput_super_p";

   function ncinput_hyper_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1245
   with Import => True,
        Convention => C,
        External_Name => "ncinput_hyper_p";

   function ncinput_capslock_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1250
   with Import => True,
        Convention => C,
        External_Name => "ncinput_capslock_p";

   function ncinput_numlock_p (n : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1255
   with Import => True,
        Convention => C,
        External_Name => "ncinput_numlock_p";

   function ncinput_equal_p (n1 : access constant ncinput; n2 : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1263
   with Import => True,
        Convention => C,
        External_Name => "ncinput_equal_p";

   function notcurses_get
     (n : access notcurses;
      ts : access constant System.OS_Interface.timespec;
      ni : access ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:1294
   with Import => True,
        Convention => C,
        External_Name => "notcurses_get";

   function notcurses_getvec
     (n : access notcurses;
      ts : access constant System.OS_Interface.timespec;
      ni : access ncinput;
      vcount : int) return int  -- /usr/local/include/notcurses/notcurses.h:1300
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getvec";

   function notcurses_inputready_fd (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1308
   with Import => True,
        Convention => C,
        External_Name => "notcurses_inputready_fd";

   function notcurses_get_nblock (n : access notcurses; ni : access ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:1314
   with Import => True,
        Convention => C,
        External_Name => "notcurses_get_nblock";

   function notcurses_get_blocking (n : access notcurses; ni : access ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:1322
   with Import => True,
        Convention => C,
        External_Name => "notcurses_get_blocking";

   function ncinput_nomod_p (ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1328
   with Import => True,
        Convention => C,
        External_Name => "ncinput_nomod_p";

   function notcurses_mice_enable (n : access notcurses; eventmask : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1341
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mice_enable";

   function notcurses_mice_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1346
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mice_disable";

   function notcurses_linesigs_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1352
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_disable";

   function notcurses_linesigs_enable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1357
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_enable";

   function notcurses_refresh
     (n : access notcurses;
      y : access unsigned;
      x : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1365
   with Import => True,
        Convention => C,
        External_Name => "notcurses_refresh";

   function ncplane_notcurses (n : access constant ncplane) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:1369
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses";

   function ncplane_notcurses_const (n : access constant ncplane) return access constant notcurses  -- /usr/local/include/notcurses/notcurses.h:1372
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses_const";

   procedure ncplane_dim_yx
     (n : access constant ncplane;
      y : access unsigned;
      x : access unsigned)  -- /usr/local/include/notcurses/notcurses.h:1376
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_yx";

   function notcurses_stdplane (nc : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1382
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane";

   function notcurses_stdplane_const (nc : access constant notcurses) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1383
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane_const";

   function notcurses_stddim_yx
     (nc : access notcurses;
      y : access unsigned;
      x : access unsigned) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1387
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx";

   function notcurses_stddim_yx_const
     (nc : access constant notcurses;
      y : access unsigned;
      x : access unsigned) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1394
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx_const";

   function ncplane_dim_y (n : access constant ncplane) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1401
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_y";

   function ncplane_dim_x (n : access constant ncplane) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1408
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_x";

   procedure ncplane_pixel_geom
     (n : access constant ncplane;
      pxy : access unsigned;
      pxx : access unsigned;
      celldimy : access unsigned;
      celldimx : access unsigned;
      maxbmapy : access unsigned;
      maxbmapx : access unsigned)  -- /usr/local/include/notcurses/notcurses.h:1419
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pixel_geom";

   procedure notcurses_term_dim_yx
     (n : access constant notcurses;
      rows : access unsigned;
      cols : access unsigned)  -- /usr/local/include/notcurses/notcurses.h:1427
   with Import => True,
        Convention => C,
        External_Name => "notcurses_term_dim_yx";

   function notcurses_at_yx
     (nc : access notcurses;
      yoff : unsigned;
      xoff : unsigned;
      stylemask : access Interfaces.Unsigned_16;
      channels : access Interfaces.Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1434
   with Import => True,
        Convention => C,
        External_Name => "notcurses_at_yx";

   type ncplane_options is record
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1463
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1464
      rows : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1465
      cols : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1466
      userptr : System.Address;  -- /usr/local/include/notcurses/notcurses.h:1467
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:1468
      resizecb : access function (arg1 : access ncplane) return int;  -- /usr/local/include/notcurses/notcurses.h:1469
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1470
      margin_b : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1471
      margin_r : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1471
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1462

   function ncplane_create (n : access ncplane; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1479
   with Import => True,
        Convention => C,
        External_Name => "ncplane_create";

   function ncpile_create (nc : access notcurses; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1484
   with Import => True,
        Convention => C,
        External_Name => "ncpile_create";

   function ncplane_resize_maximize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1492
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_maximize";

   function ncplane_resize_marginalized (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1496
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_marginalized";

   function ncplane_resize_realign (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1500
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_realign";

   function ncplane_resize_placewithin (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1504
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_placewithin";

   procedure ncplane_set_resizecb (n : access ncplane; resizecb : access function (arg1 : access ncplane) return int)  -- /usr/local/include/notcurses/notcurses.h:1508
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_resizecb";

   function ncplane_resizecb (n : access constant ncplane) return access function (arg1 : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1511
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resizecb";

   function ncplane_set_name (n : access ncplane; name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1514
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_name";

   function ncplane_name (n : access constant ncplane) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1518
   with Import => True,
        Convention => C,
        External_Name => "ncplane_name";

   function ncplane_reparent (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1527
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent";

   function ncplane_reparent_family (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1533
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent_family";

   function ncplane_dup (n : access constant ncplane; opaque : System.Address) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1542
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dup";

   procedure ncplane_translate
     (src : access constant ncplane;
      dst : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1548
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate";

   function ncplane_translate_abs
     (n : access constant ncplane;
      y : access int;
      x : access int) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1556
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate_abs";

   function ncplane_set_scrolling (n : access ncplane; scrollp : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1562
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_scrolling";

   function ncplane_scrolling_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1565
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrolling_p";

   function ncplane_set_autogrow (n : access ncplane; growp : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1571
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_autogrow";

   function ncplane_autogrow_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1574
   with Import => True,
        Convention => C,
        External_Name => "ncplane_autogrow_p";

   type ncpalette_array3100 is array (0 .. 255) of aliased Interfaces.Unsigned_32;
   type ncpalette is record
      chans : aliased ncpalette_array3100;  -- /usr/local/include/notcurses/notcurses.h:1583
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1582

   function ncpalette_new (nc : access notcurses) return access ncpalette  -- /usr/local/include/notcurses/notcurses.h:1588
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_new";

   function ncpalette_use (nc : access notcurses; p : access constant ncpalette) return int  -- /usr/local/include/notcurses/notcurses.h:1593
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_use";

   function ncpalette_set_rgb8
     (p : access ncpalette;
      idx : int;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1598
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set_rgb8";

   function ncpalette_set
     (p : access ncpalette;
      idx : int;
      rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1606
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set";

   function ncpalette_get
     (p : access constant ncpalette;
      idx : int;
      palent : access Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:1614
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_get";

   function ncpalette_get_rgb8
     (p : access constant ncpalette;
      idx : int;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1623
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_get_rgb8";

   procedure ncpalette_free (p : access ncpalette)  -- /usr/local/include/notcurses/notcurses.h:1631
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_free";

   type nccapabilities is record
      colors : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1635
      utf8 : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1636
      rgb : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1637
      can_change_colors : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1638
      halfblocks : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1640
      quadrants : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1641
      sextants : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1642
      braille : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1643
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1634

   function notcurses_supported_styles (nc : access constant notcurses) return Interfaces.Unsigned_16  -- /usr/local/include/notcurses/notcurses.h:1650
   with Import => True,
        Convention => C,
        External_Name => "notcurses_supported_styles";

   function notcurses_palette_size (nc : access constant notcurses) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1656
   with Import => True,
        Convention => C,
        External_Name => "notcurses_palette_size";

   function notcurses_detected_terminal (nc : access constant notcurses) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1661
   with Import => True,
        Convention => C,
        External_Name => "notcurses_detected_terminal";

   function notcurses_capabilities (n : access constant notcurses) return access constant nccapabilities  -- /usr/local/include/notcurses/notcurses.h:1664
   with Import => True,
        Convention => C,
        External_Name => "notcurses_capabilities";

   type ncpixelimpl_e is
     (NCPIXEL_NONE,
      NCPIXEL_SIXEL,
      NCPIXEL_LINUXFB,
      NCPIXEL_ITERM2,
      NCPIXEL_KITTY_STATIC,
      NCPIXEL_KITTY_ANIMATED,
      NCPIXEL_KITTY_SELFREF)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:1687

   function notcurses_check_pixel_support (nc : access constant notcurses) return ncpixelimpl_e  -- /usr/local/include/notcurses/notcurses.h:1690
   with Import => True,
        Convention => C,
        External_Name => "notcurses_check_pixel_support";

   function nccapability_canchangecolor (caps : access constant nccapabilities) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1697
   with Import => True,
        Convention => C,
        External_Name => "nccapability_canchangecolor";

   function notcurses_cantruecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1710
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cantruecolor";

   function notcurses_canchangecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1716
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canchangecolor";

   function notcurses_canfade (n : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1722
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canfade";

   function notcurses_canopen_images (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1727
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_images";

   function notcurses_canopen_videos (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1731
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_videos";

   function notcurses_canutf8 (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1736
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canutf8";

   function notcurses_canhalfblock (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1742
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canhalfblock";

   function notcurses_canquadrant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1748
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canquadrant";

   function notcurses_cansextant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1754
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cansextant";

   function notcurses_canbraille (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1760
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canbraille";

   function notcurses_canpixel (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1766
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canpixel";

   type ncstats is record
      renders : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1774
      writeouts : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1775
      failed_renders : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1776
      failed_writeouts : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1777
      raster_bytes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1778
      raster_max_bytes : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1779
      raster_min_bytes : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1780
      render_ns : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1781
      render_max_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1782
      render_min_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1783
      raster_ns : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1784
      raster_max_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1785
      raster_min_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1786
      writeout_ns : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1787
      writeout_max_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1788
      writeout_min_ns : aliased Interfaces.Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1789
      cellelisions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1790
      cellemissions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1791
      fgelisions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1792
      fgemissions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1793
      bgelisions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1794
      bgemissions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1795
      defaultelisions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1796
      defaultemissions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1797
      refreshes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1798
      sprixelemissions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1799
      sprixelelisions : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1800
      sprixelbytes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1801
      appsync_updates : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1802
      input_errors : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1803
      input_events : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1804
      hpa_gratuitous : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1805
      cell_geo_changes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1806
      pixel_geo_changes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1807
      fbbytes : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1810
      planes : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1811
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1772

   function notcurses_stats_alloc (nc : access constant notcurses) return access ncstats  -- /usr/local/include/notcurses/notcurses.h:1816
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats_alloc";

   procedure notcurses_stats (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1821
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats";

   procedure notcurses_stats_reset (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1826
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats_reset";

   function ncplane_resize
     (n : access ncplane;
      keepy : int;
      keepx : int;
      keepleny : unsigned;
      keeplenx : unsigned;
      yoff : int;
      xoff : int;
      ylen : unsigned;
      xlen : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1843
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize";

   function ncplane_resize_simple
     (n : access ncplane;
      ylen : unsigned;
      xlen : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1851
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_simple";

   function ncplane_destroy (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1862
   with Import => True,
        Convention => C,
        External_Name => "ncplane_destroy";

   function ncplane_set_base_cell (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1868
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base_cell";

   function ncplane_set_base
     (n : access ncplane;
      egc : Interfaces.C.Strings.chars_ptr;
      stylemask : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1874
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base";

   function ncplane_base (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1879
   with Import => True,
        Convention => C,
        External_Name => "ncplane_base";

   procedure ncplane_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1883
   with Import => True,
        Convention => C,
        External_Name => "ncplane_yx";

   function ncplane_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1885
   with Import => True,
        Convention => C,
        External_Name => "ncplane_y";

   function ncplane_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1886
   with Import => True,
        Convention => C,
        External_Name => "ncplane_x";

   function ncplane_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1891
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_yx";

   function ncplane_move_rel
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1896
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_rel";

   procedure ncplane_abs_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1904
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_yx";

   function ncplane_abs_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1906
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_y";

   function ncplane_abs_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1907
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_x";

   function ncplane_parent (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1910
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent";

   function ncplane_parent_const (n : access constant ncplane) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1912
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent_const";

   function ncplane_descendant_p (n : access constant ncplane; ancestor : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1917
   with Import => True,
        Convention => C,
        External_Name => "ncplane_descendant_p";

   function ncplane_move_above (n : access ncplane; above : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1930
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_above";

   function ncplane_move_below (n : access ncplane; below : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1938
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_below";

   procedure ncplane_move_top (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1945
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_top";

   procedure ncplane_move_bottom (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1951
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_bottom";

   function ncplane_move_family_above (n : access ncplane; targ : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1960
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_family_above";

   function ncplane_move_family_below (n : access ncplane; targ : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1963
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_family_below";

   procedure ncplane_move_family_top (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1968
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_family_top";

   procedure ncplane_move_family_bottom (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1974
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_family_bottom";

   function ncplane_below (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1979
   with Import => True,
        Convention => C,
        External_Name => "ncplane_below";

   function ncplane_above (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1983
   with Import => True,
        Convention => C,
        External_Name => "ncplane_above";

   function ncplane_scrollup (n : access ncplane; r : int) return int  -- /usr/local/include/notcurses/notcurses.h:1988
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrollup";

   function ncplane_scrollup_child (n : access ncplane; child : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1995
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrollup_child";

   function ncplane_rotate_cw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:2004
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_cw";

   function ncplane_rotate_ccw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:2006
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_ccw";

   function ncplane_at_cursor
     (n : access constant ncplane;
      stylemask : access Interfaces.Unsigned_16;
      channels : access Interfaces.Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2012
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor";

   function ncplane_at_cursor_cell (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2018
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor_cell";

   function ncplane_at_yx
     (n : access constant ncplane;
      y : int;
      x : int;
      stylemask : access Interfaces.Unsigned_16;
      channels : access Interfaces.Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2029
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx";

   function ncplane_at_yx_cell
     (n : access ncplane;
      y : int;
      x : int;
      c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2039
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx_cell";

   function ncplane_contents
     (n : access ncplane;
      begy : int;
      begx : int;
      leny : unsigned;
      lenx : unsigned) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2047
   with Import => True,
        Convention => C,
        External_Name => "ncplane_contents";

   function ncplane_set_userptr (n : access ncplane; opaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:2054
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_userptr";

   function ncplane_userptr (n : access ncplane) return System.Address  -- /usr/local/include/notcurses/notcurses.h:2056
   with Import => True,
        Convention => C,
        External_Name => "ncplane_userptr";

   procedure ncplane_center_abs
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:2063
   with Import => True,
        Convention => C,
        External_Name => "ncplane_center_abs";

   function ncplane_as_rgba
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : unsigned;
      lenx : unsigned;
      pxdimy : access unsigned;
      pxdimx : access unsigned) return access Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2073
   with Import => True,
        Convention => C,
        External_Name => "ncplane_as_rgba";

   function notcurses_align
     (availu : int;
      align : ncalign_e;
      u : int) return int  -- /usr/local/include/notcurses/notcurses.h:2083
   with Import => True,
        Convention => C,
        External_Name => "notcurses_align";

   function ncplane_halign
     (n : access constant ncplane;
      align : ncalign_e;
      c : int) return int  -- /usr/local/include/notcurses/notcurses.h:2100
   with Import => True,
        Convention => C,
        External_Name => "ncplane_halign";

   function ncplane_valign
     (n : access constant ncplane;
      align : ncalign_e;
      r : int) return int  -- /usr/local/include/notcurses/notcurses.h:2108
   with Import => True,
        Convention => C,
        External_Name => "ncplane_valign";

   function ncplane_cursor_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:2115
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_move_yx";

   function ncplane_cursor_move_rel
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:2121
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_move_rel";

   procedure ncplane_home (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2125
   with Import => True,
        Convention => C,
        External_Name => "ncplane_home";

   procedure ncplane_cursor_yx
     (n : access constant ncplane;
      y : access unsigned;
      x : access unsigned)  -- /usr/local/include/notcurses/notcurses.h:2129
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_yx";

   function ncplane_cursor_y (n : access constant ncplane) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2133
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_y";

   function ncplane_cursor_x (n : access constant ncplane) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2140
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_x";

   function ncplane_channels (n : access constant ncplane) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2147
   with Import => True,
        Convention => C,
        External_Name => "ncplane_channels";

   function ncplane_styles (n : access constant ncplane) return Interfaces.Unsigned_16  -- /usr/local/include/notcurses/notcurses.h:2151
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles";

   function ncplane_putc_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2158
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc_yx";

   function ncplane_putc (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2163
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc";

   function ncplane_putchar_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : char) return int  -- /usr/local/include/notcurses/notcurses.h:2172
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_yx";

   function ncplane_putchar (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:2179
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar";

   function ncplane_putchar_stained (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:2185
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_stained";

   function ncplane_putegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2193
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_yx";

   function ncplane_putegc
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2199
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc";

   function ncplane_putegc_stained
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2205
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_stained";

   function ncwcsrtombs (src : access wchar_t) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2210
   with Import => True,
        Convention => C,
        External_Name => "ncwcsrtombs";

   function ncplane_putwegc
     (n : access ncplane;
      gclust : access wchar_t;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2232
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc";

   function ncplane_putwegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : access wchar_t;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2244
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_yx";

   function ncplane_putwegc_stained
     (n : access ncplane;
      gclust : access wchar_t;
      sbytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2254
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_stained";

   function ncplane_putstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2264
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_yx";

   function ncplane_putstr (n : access ncplane; gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2287
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr";

   function ncplane_putstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2292
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_aligned";

   function ncplane_putstr_stained (n : access ncplane; gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2306
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_stained";

   function ncplane_putnstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.size_t;
      str : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2323
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_aligned";

   function ncplane_putnstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      s : Interfaces.C.size_t;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2333
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_yx";

   function ncplane_putnstr
     (n : access ncplane;
      s : Interfaces.C.size_t;
      gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2357
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr";

   function ncplane_putwstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclustarr : access wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2364
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_yx";

   function ncplane_putwstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      gclustarr : access wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2385
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_aligned";

   function ncplane_putwstr_stained (n : access ncplane; gclustarr : access wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2395
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_stained";

   function ncplane_putwstr (n : access ncplane; gclustarr : access wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2399
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr";

   function ncplane_pututf32_yx
     (n : access ncplane;
      y : int;
      x : int;
      u : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2407
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pututf32_yx";

   function ncplane_putwc_yx
     (n : access ncplane;
      y : int;
      x : int;
      w : wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2427
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_yx";

   function ncplane_putwc (n : access ncplane; w : wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2433
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc";

   function ncplane_putwc_utf32
     (n : access ncplane;
      w : access wchar_t;
      wchars : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2447
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_utf32";

   function ncplane_putwc_stained (n : access ncplane; w : wchar_t) return int  -- /usr/local/include/notcurses/notcurses.h:2466
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_stained";

   function ncplane_vprintf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2472
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_aligned";

   function ncplane_vprintf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2477
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_yx";

   function ncplane_vprintf
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2483
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf";

   function ncplane_vprintf_stained
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2487
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_stained";

   function ncplane_printf (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:2497
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf";

   function ncplane_printf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:2510
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_yx";

   function ncplane_printf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:2524
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_aligned";

   function ncplane_printf_stained (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:2537
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_stained";

   function ncplane_puttext
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      text : Interfaces.C.Strings.chars_ptr;
      bytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:2564
   with Import => True,
        Convention => C,
        External_Name => "ncplane_puttext";

   function ncplane_hline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : unsigned;
      c1 : Interfaces.Unsigned_64;
      c2 : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:2574
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline_interp";

   function ncplane_hline
     (n : access ncplane;
      c : access constant nccell;
      len : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2579
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline";

   function ncplane_vline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : unsigned;
      c1 : Interfaces.Unsigned_64;
      c2 : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:2583
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vline_interp";

   function ncplane_vline
     (n : access ncplane;
      c : access constant nccell;
      len : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2588
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vline";

   function ncplane_box
     (n : access ncplane;
      ul : access constant nccell;
      ur : access constant nccell;
      ll : access constant nccell;
      lr : access constant nccell;
      hline : access constant nccell;
      vline : access constant nccell;
      ystop : unsigned;
      xstop : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2621
   with Import => True,
        Convention => C,
        External_Name => "ncplane_box";

   function ncplane_box_sized
     (n : access ncplane;
      ul : access constant nccell;
      ur : access constant nccell;
      ll : access constant nccell;
      lr : access constant nccell;
      hline : access constant nccell;
      vline : access constant nccell;
      ystop : unsigned;
      xstop : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2630
   with Import => True,
        Convention => C,
        External_Name => "ncplane_box_sized";

   function ncplane_perimeter
     (n : access ncplane;
      ul : access constant nccell;
      ur : access constant nccell;
      ll : access constant nccell;
      lr : access constant nccell;
      hline : access constant nccell;
      vline : access constant nccell;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2641
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter";

   function ncplane_polyfill_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2657
   with Import => True,
        Convention => C,
        External_Name => "ncplane_polyfill_yx";

   function ncplane_gradient
     (n : access ncplane;
      y : int;
      x : int;
      ylen : unsigned;
      xlen : unsigned;
      egc : Interfaces.C.Strings.chars_ptr;
      styles : Interfaces.Unsigned_16;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:2680
   with Import => True,
        Convention => C,
        External_Name => "ncplane_gradient";

   function ncplane_gradient2x1
     (n : access ncplane;
      y : int;
      x : int;
      ylen : unsigned;
      xlen : unsigned;
      ul : Interfaces.Unsigned_32;
      ur : Interfaces.Unsigned_32;
      ll : Interfaces.Unsigned_32;
      lr : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2689
   with Import => True,
        Convention => C,
        External_Name => "ncplane_gradient2x1";

   function ncplane_format
     (n : access ncplane;
      y : int;
      x : int;
      ylen : unsigned;
      xlen : unsigned;
      stylemask : Interfaces.Unsigned_16) return int  -- /usr/local/include/notcurses/notcurses.h:2701
   with Import => True,
        Convention => C,
        External_Name => "ncplane_format";

   function ncplane_stain
     (n : access ncplane;
      y : int;
      x : int;
      ylen : unsigned;
      xlen : unsigned;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:2712
   with Import => True,
        Convention => C,
        External_Name => "ncplane_stain";

   function ncplane_mergedown_simple (src : access ncplane; dst : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:2719
   with Import => True,
        Convention => C,
        External_Name => "ncplane_mergedown_simple";

   function ncplane_mergedown
     (src : access ncplane;
      dst : access ncplane;
      begsrcy : int;
      begsrcx : int;
      leny : unsigned;
      lenx : unsigned;
      dsty : int;
      dstx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2734
   with Import => True,
        Convention => C,
        External_Name => "ncplane_mergedown";

   procedure ncplane_erase (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2745
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase";

   function ncplane_erase_region
     (n : access ncplane;
      ystart : int;
      xstart : int;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:2769
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase_region";

   function nccell_fg_rgb (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2775
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb";

   function nccell_bg_rgb (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2781
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb";

   function nccell_fg_alpha (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2787
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_alpha";

   function nccell_bg_alpha (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2793
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_alpha";

   function nccell_fg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2799
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb8";

   function nccell_bg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2805
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb8";

   function nccell_set_fg_rgb8
     (cl : access nccell;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2812
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8";

   procedure nccell_set_fg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2818
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8_clipped";

   function nccell_set_fg_rgb (c : access nccell; channel : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2824
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb";

   function nccell_set_fg_palindex (cl : access nccell; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2831
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_palindex";

   function nccell_fg_palindex (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2836
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex";

   function nccell_set_bg_rgb8
     (cl : access nccell;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2843
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8";

   procedure nccell_set_bg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2849
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8_clipped";

   function nccell_set_bg_rgb (c : access nccell; channel : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2856
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb";

   function nccell_set_bg_palindex (cl : access nccell; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2863
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_palindex";

   function nccell_bg_palindex (cl : access constant nccell) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2868
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex";

   function nccell_fg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2874
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_default_p";

   function nccell_fg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2879
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex_p";

   function nccell_bg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2887
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_default_p";

   function nccell_bg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2892
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex_p";

   function ncplane_bchannel (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2899
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bchannel";

   function ncplane_fchannel (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2906
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fchannel";

   procedure ncplane_set_channels (n : access ncplane; channels : Interfaces.Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:2912
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_channels";

   function ncplane_set_bchannel (n : access ncplane; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2917
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bchannel";

   function ncplane_set_fchannel (n : access ncplane; channel : Interfaces.Unsigned_32) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2922
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fchannel";

   procedure ncplane_set_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2927
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_styles";

   procedure ncplane_on_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2931
   with Import => True,
        Convention => C,
        External_Name => "ncplane_on_styles";

   procedure ncplane_off_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2935
   with Import => True,
        Convention => C,
        External_Name => "ncplane_off_styles";

   function ncplane_fg_rgb (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2940
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb";

   function ncplane_bg_rgb (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2946
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb";

   function ncplane_fg_alpha (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2952
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_alpha";

   function ncplane_fg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2958
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_default_p";

   function ncplane_bg_alpha (n : access constant ncplane) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2964
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_alpha";

   function ncplane_bg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2970
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_default_p";

   function ncplane_fg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2976
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb8";

   function ncplane_bg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2982
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb8";

   function ncplane_set_fg_rgb8
     (n : access ncplane;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2992
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8";

   function ncplane_set_bg_rgb8
     (n : access ncplane;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2993
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8";

   procedure ncplane_set_bg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2996
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8_clipped";

   procedure ncplane_set_fg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2997
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8_clipped";

   function ncplane_set_fg_rgb (n : access ncplane; channel : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3000
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb";

   function ncplane_set_bg_rgb (n : access ncplane; channel : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3001
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb";

   procedure ncplane_set_fg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:3004
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_default";

   procedure ncplane_set_bg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:3005
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_default";

   function ncplane_set_fg_palindex (n : access ncplane; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3009
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_palindex";

   function ncplane_set_bg_palindex (n : access ncplane; idx : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3010
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_palindex";

   function ncplane_set_fg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3013
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_alpha";

   function ncplane_set_bg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3014
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_alpha";

   type fadecb is access function
        (arg1 : access notcurses;
         arg2 : access ncplane;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3019

   function ncplane_fadeout
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3026
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout";

   function ncplane_fadein
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3033
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein";

   function ncfadectx_setup (n : access ncplane) return access ncfadectx  -- /usr/local/include/notcurses/notcurses.h:3039
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_setup";

   function ncfadectx_iterations (nctx : access constant ncfadectx) return int  -- /usr/local/include/notcurses/notcurses.h:3043
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_iterations";

   function ncplane_fadeout_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3048
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout_iteration";

   function ncplane_fadein_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3054
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein_iteration";

   function ncplane_pulse
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3063
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pulse";

   procedure ncfadectx_free (nctx : access ncfadectx)  -- /usr/local/include/notcurses/notcurses.h:3067
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_free";

   function nccells_load_box
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3074
   with Import => True,
        Convention => C,
        External_Name => "nccells_load_box";

   function nccells_ascii_box
     (n : access ncplane;
      attr : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3100
   with Import => True,
        Convention => C,
        External_Name => "nccells_ascii_box";

   function nccells_double_box
     (n : access ncplane;
      attr : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3106
   with Import => True,
        Convention => C,
        External_Name => "nccells_double_box";

   function nccells_rounded_box
     (n : access ncplane;
      attr : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3115
   with Import => True,
        Convention => C,
        External_Name => "nccells_rounded_box";

   function nccells_light_box
     (n : access ncplane;
      attr : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3124
   with Import => True,
        Convention => C,
        External_Name => "nccells_light_box";

   function nccells_heavy_box
     (n : access ncplane;
      attr : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3133
   with Import => True,
        Convention => C,
        External_Name => "nccells_heavy_box";

   function ncplane_rounded_box
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ystop : unsigned;
      xstop : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3142
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rounded_box";

   function ncplane_perimeter_rounded
     (n : access ncplane;
      stylemask : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3158
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_rounded";

   function ncplane_rounded_box_sized
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3182
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rounded_box_sized";

   function ncplane_double_box
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3191
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box";

   function ncplane_ascii_box
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3207
   with Import => True,
        Convention => C,
        External_Name => "ncplane_ascii_box";

   function ncplane_perimeter_double
     (n : access ncplane;
      stylemask : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3223
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_double";

   function ncplane_double_box_sized
     (n : access ncplane;
      styles : Interfaces.Unsigned_16;
      channels : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3247
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box_sized";

   function ncvisual_from_file (file : Interfaces.C.Strings.chars_ptr) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3257
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_file";

   function ncvisual_from_rgba
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3266
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgba";

   function ncvisual_from_rgb_packed
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int;
      alpha : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3272
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgb_packed";

   function ncvisual_from_rgb_loose
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int;
      alpha : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3279
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgb_loose";

   function ncvisual_from_bgra
     (bgra : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3287
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_bgra";

   function ncvisual_from_palidx
     (data : System.Address;
      rows : int;
      rowstride : int;
      cols : int;
      palsize : int;
      pstride : int;
      palette : access Interfaces.Unsigned_32) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3294
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_palidx";

   function ncvisual_from_plane
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : unsigned;
      lenx : unsigned) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3306
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_plane";

   function ncvisual_from_sixel
     (s : Interfaces.C.Strings.chars_ptr;
      leny : unsigned;
      lenx : unsigned) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:3313
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_sixel";

   type ncvisual_options is record
      n : access ncplane;  -- /usr/local/include/notcurses/notcurses.h:3329
      scaling : aliased ncscale_e;  -- /usr/local/include/notcurses/notcurses.h:3333
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3339
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3339
      begy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3344
      begx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3344
      leny : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3345
      lenx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3345
      blitter : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:3349
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3350
      transcolor : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3351
      pxoffy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3357
      pxoffx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3357
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3324

   type ncvgeom is record
      pixy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3377
      pixx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3377
      cdimy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3378
      cdimx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3378
      rpixy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3379
      rpixx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3379
      rcelly : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3380
      rcellx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3380
      scaley : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3381
      scalex : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3381
      begy : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3382
      begx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3382
      leny : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3383
      lenx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3383
      maxpixely : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3384
      maxpixelx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3384
      blitter : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:3385
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3376

   function ncvisual_geom
     (nc : access constant notcurses;
      n : access constant ncvisual;
      vopts : access constant ncvisual_options;
      geom : access ncvgeom) return int  -- /usr/local/include/notcurses/notcurses.h:3393
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_geom";

   procedure ncvisual_destroy (ncv : access ncvisual)  -- /usr/local/include/notcurses/notcurses.h:3399
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_destroy";

   function ncvisual_decode (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:3403
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode";

   function ncvisual_decode_loop (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:3410
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode_loop";

   function ncvisual_rotate (n : access ncvisual; rads : double) return int  -- /usr/local/include/notcurses/notcurses.h:3415
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_rotate";

   function ncvisual_resize
     (n : access ncvisual;
      rows : int;
      cols : int) return int  -- /usr/local/include/notcurses/notcurses.h:3420
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_resize";

   function ncvisual_resize_noninterpolative
     (n : access ncvisual;
      rows : int;
      cols : int) return int  -- /usr/local/include/notcurses/notcurses.h:3425
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_resize_noninterpolative";

   function ncvisual_polyfill_yx
     (n : access ncvisual;
      y : unsigned;
      x : unsigned;
      rgba : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3429
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_polyfill_yx";

   function ncvisual_at_yx
     (n : access constant ncvisual;
      y : unsigned;
      x : unsigned;
      pixel : access Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3433
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_at_yx";

   function ncvisual_set_yx
     (n : access constant ncvisual;
      y : unsigned;
      x : unsigned;
      pixel : Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3438
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_set_yx";

   function ncvisual_blit
     (nc : access notcurses;
      ncv : access ncvisual;
      vopts : access constant ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3453
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_blit";

   function ncvisualplane_create
     (nc : access notcurses;
      opts : access constant ncplane_options;
      ncv : access ncvisual;
      vopts : access ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3462
   with Import => True,
        Convention => C,
        External_Name => "ncvisualplane_create";

   function ncvisual_subtitle_plane (parent : access ncplane; ncv : access constant ncvisual) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3493
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_subtitle_plane";

   function ncvisual_media_defblitter (nc : access constant notcurses; scale : ncscale_e) return ncblitter_e  -- /usr/local/include/notcurses/notcurses.h:3505
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_media_defblitter";

   type ncstreamcb is access function
        (arg1 : access ncvisual;
         arg2 : access ncvisual_options;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3511

   function ncvisual_simple_streamer
     (ncv : access ncvisual;
      vopts : access ncvisual_options;
      tspec : access constant System.OS_Interface.timespec;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3517
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_simple_streamer";

   function ncvisual_stream
     (nc : access notcurses;
      ncv : access ncvisual;
      timescale : float;
      streamer : ncstreamcb;
      vopts : access constant ncvisual_options;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:3530
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_stream";

   function ncblit_rgba
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:3542
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgba";

   function ncblit_bgrx
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:3547
   with Import => True,
        Convention => C,
        External_Name => "ncblit_bgrx";

   function ncblit_rgb_packed
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3552
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_packed";

   function ncblit_rgb_loose
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3558
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_loose";

   function ncpixel_a (pixel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3574
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_a";

   function ncpixel_r (pixel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3580
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_r";

   function ncpixel_g (pixel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3586
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_g";

   function ncpixel_b (pixel : Interfaces.Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3592
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_b";

   function ncpixel_set_a (pixel : access Interfaces.Unsigned_32; a : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3598
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_a";

   function ncpixel_set_r (pixel : access Interfaces.Unsigned_32; r : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3608
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_r";

   function ncpixel_set_g (pixel : access Interfaces.Unsigned_32; g : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3618
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_g";

   function ncpixel_set_b (pixel : access Interfaces.Unsigned_32; b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3628
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_b";

   function ncpixel
     (r : unsigned;
      g : unsigned;
      b : unsigned) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3638
   with Import => True,
        Convention => C,
        External_Name => "ncpixel";

   function ncpixel_set_rgb8
     (pixel : access Interfaces.Unsigned_32;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3652
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_rgb8";

   type ncreel_options is record
      bordermask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3685
      borderchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3686
      tabletmask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3687
      tabletchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3688
      focusedchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3689
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3690
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3678

   function ncreel_create (n : access ncplane; popts : access constant ncreel_options) return access ncreel  -- /usr/local/include/notcurses/notcurses.h:3695
   with Import => True,
        Convention => C,
        External_Name => "ncreel_create";

   function ncreel_plane (nr : access ncreel) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3699
   with Import => True,
        Convention => C,
        External_Name => "ncreel_plane";

   type tabletcb is access function (arg1 : access nctablet; arg2 : Extensions.bool) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3706

   function ncreel_add
     (nr : access ncreel;
      after : access nctablet;
      before : access nctablet;
      cb : tabletcb;
      opaque : System.Address) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3715
   with Import => True,
        Convention => C,
        External_Name => "ncreel_add";

   function ncreel_tabletcount (nr : access constant ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:3721
   with Import => True,
        Convention => C,
        External_Name => "ncreel_tabletcount";

   function ncreel_del (nr : access ncreel; t : access nctablet) return int  -- /usr/local/include/notcurses/notcurses.h:3726
   with Import => True,
        Convention => C,
        External_Name => "ncreel_del";

   function ncreel_redraw (nr : access ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:3732
   with Import => True,
        Convention => C,
        External_Name => "ncreel_redraw";

   function ncreel_offer_input (nr : access ncreel; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3741
   with Import => True,
        Convention => C,
        External_Name => "ncreel_offer_input";

   function ncreel_focused (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3746
   with Import => True,
        Convention => C,
        External_Name => "ncreel_focused";

   function ncreel_next (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3750
   with Import => True,
        Convention => C,
        External_Name => "ncreel_next";

   function ncreel_prev (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3754
   with Import => True,
        Convention => C,
        External_Name => "ncreel_prev";

   procedure ncreel_destroy (nr : access ncreel)  -- /usr/local/include/notcurses/notcurses.h:3758
   with Import => True,
        Convention => C,
        External_Name => "ncreel_destroy";

   function nctablet_userptr (t : access nctablet) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3761
   with Import => True,
        Convention => C,
        External_Name => "nctablet_userptr";

   function nctablet_plane (t : access nctablet) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3764
   with Import => True,
        Convention => C,
        External_Name => "nctablet_plane";

   --  function ncnmetric
   --    (val : stdint_h.uintmax_t;
   --     s : Interfaces.C.size_t;
   --     decimal : stdint_h.uintmax_t;
   --     buf : Interfaces.C.Strings.chars_ptr;
   --     omitdec : int;
   --     mult : stdint_h.uintmax_t;
   --     uprefix : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3791
   --  with Import => True,
   --       Convention => C,
   --       External_Name => "ncnmetric";

   --  function ncqprefix
   --    (val : stdint_h.uintmax_t;
   --     decimal : stdint_h.uintmax_t;
   --     buf : Interfaces.C.Strings.chars_ptr;
   --     omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3820
   --  with Import => True,
   --       Convention => C,
   --       External_Name => "ncqprefix";

   --  function nciprefix
   --    (val : stdint_h.uintmax_t;
   --     decimal : stdint_h.uintmax_t;
   --     buf : Interfaces.C.Strings.chars_ptr;
   --     omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3826
   --  with Import => True,
   --       Convention => C,
   --       External_Name => "nciprefix";

   --  function ncbprefix
   --    (val : stdint_h.uintmax_t;
   --     decimal : stdint_h.uintmax_t;
   --     buf : Interfaces.C.Strings.chars_ptr;
   --     omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3832
   --  with Import => True,
   --       Convention => C,
   --       External_Name => "ncbprefix";

   function notcurses_default_foreground (nc : access constant notcurses; fg : access Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3839
   with Import => True,
        Convention => C,
        External_Name => "notcurses_default_foreground";

   function notcurses_default_background (nc : access constant notcurses; bg : access Interfaces.Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3846
   with Import => True,
        Convention => C,
        External_Name => "notcurses_default_background";

   function notcurses_cursor_enable
     (nc : access notcurses;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:3853
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_enable";

   function notcurses_cursor_disable (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:3858
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_disable";

   function notcurses_cursor_yx
     (nc : access constant notcurses;
      y : access int;
      x : access int) return int  -- /usr/local/include/notcurses/notcurses.h:3862
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_yx";

   procedure ncplane_greyscale (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:3866
   with Import => True,
        Convention => C,
        External_Name => "ncplane_greyscale";

   type ncselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3887
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3888
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3886

   type ncselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3892
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3893
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3894
      items : access constant ncselector_item;  -- /usr/local/include/notcurses/notcurses.h:3895
      defidx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3898
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3900
      opchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3902
      descchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3903
      titlechannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3904
      footchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3905
      boxchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3906
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3907
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3891

   function ncselector_create (n : access ncplane; opts : access constant ncselector_options) return access ncselector  -- /usr/local/include/notcurses/notcurses.h:3910
   with Import => True,
        Convention => C,
        External_Name => "ncselector_create";

   function ncselector_additem (n : access ncselector; item : access constant ncselector_item) return int  -- /usr/local/include/notcurses/notcurses.h:3915
   with Import => True,
        Convention => C,
        External_Name => "ncselector_additem";

   function ncselector_delitem (n : access ncselector; item : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3916
   with Import => True,
        Convention => C,
        External_Name => "ncselector_delitem";

   function ncselector_selected (n : access constant ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3919
   with Import => True,
        Convention => C,
        External_Name => "ncselector_selected";

   function ncselector_plane (n : access ncselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3923
   with Import => True,
        Convention => C,
        External_Name => "ncselector_plane";

   function ncselector_previtem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3928
   with Import => True,
        Convention => C,
        External_Name => "ncselector_previtem";

   function ncselector_nextitem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3930
   with Import => True,
        Convention => C,
        External_Name => "ncselector_nextitem";

   function ncselector_offer_input (n : access ncselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3940
   with Import => True,
        Convention => C,
        External_Name => "ncselector_offer_input";

   procedure ncselector_destroy (n : access ncselector; item : System.Address)  -- /usr/local/include/notcurses/notcurses.h:3944
   with Import => True,
        Convention => C,
        External_Name => "ncselector_destroy";

   type ncmselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3947
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3948
      selected : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:3949
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3946

   type ncmultiselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3974
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3975
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3976
      items : access constant ncmselector_item;  -- /usr/local/include/notcurses/notcurses.h:3977
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3979
      opchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3981
      descchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3982
      titlechannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3983
      footchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3984
      boxchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3985
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3986
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3973

   function ncmultiselector_create (n : access ncplane; opts : access constant ncmultiselector_options) return access ncmultiselector  -- /usr/local/include/notcurses/notcurses.h:3989
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_create";

   function ncmultiselector_selected
     (n : access ncmultiselector;
      selected : access Extensions.bool;
      count : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3994
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_selected";

   function ncmultiselector_plane (n : access ncmultiselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3997
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_plane";

   function ncmultiselector_offer_input (n : access ncmultiselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4006
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_offer_input";

   procedure ncmultiselector_destroy (n : access ncmultiselector)  -- /usr/local/include/notcurses/notcurses.h:4010
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_destroy";

   type nctree_item;
   type nctree_item is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:4021
      subs : access nctree_item;  -- /usr/local/include/notcurses/notcurses.h:4022
      subcount : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:4023
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4020

   type nctree_options is record
      items : access constant nctree_item;  -- /usr/local/include/notcurses/notcurses.h:4027
      count : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:4028
      nctreecb : access function
           (arg1 : access ncplane;
            arg2 : System.Address;
            arg3 : int) return int;  -- /usr/local/include/notcurses/notcurses.h:4029
      indentcols : aliased int;  -- /usr/local/include/notcurses/notcurses.h:4030
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4031
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4026

   type nctree is null record;   -- incomplete struct

   function nctree_create (n : access ncplane; opts : access constant nctree_options) return access nctree  -- /usr/local/include/notcurses/notcurses.h:4036
   with Import => True,
        Convention => C,
        External_Name => "nctree_create";

   function nctree_plane (n : access nctree) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4040
   with Import => True,
        Convention => C,
        External_Name => "nctree_plane";

   function nctree_redraw (n : access nctree) return int  -- /usr/local/include/notcurses/notcurses.h:4046
   with Import => True,
        Convention => C,
        External_Name => "nctree_redraw";

   function nctree_offer_input (n : access nctree; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4055
   with Import => True,
        Convention => C,
        External_Name => "nctree_offer_input";

   function nctree_focused (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4060
   with Import => True,
        Convention => C,
        External_Name => "nctree_focused";

   function nctree_next (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4063
   with Import => True,
        Convention => C,
        External_Name => "nctree_next";

   function nctree_prev (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4066
   with Import => True,
        Convention => C,
        External_Name => "nctree_prev";

   function nctree_goto
     (n : access nctree;
      spec : access unsigned;
      failspec : access int) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4074
   with Import => True,
        Convention => C,
        External_Name => "nctree_goto";

   function nctree_add
     (n : access nctree;
      spec : access unsigned;
      add : access constant nctree_item) return int  -- /usr/local/include/notcurses/notcurses.h:4079
   with Import => True,
        Convention => C,
        External_Name => "nctree_add";

   function nctree_del (n : access nctree; spec : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4083
   with Import => True,
        Convention => C,
        External_Name => "nctree_del";

   procedure nctree_destroy (n : access nctree)  -- /usr/local/include/notcurses/notcurses.h:4087
   with Import => True,
        Convention => C,
        External_Name => "nctree_destroy";

   type ncmenu_item is record
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:4095
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:4096
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4094

   type ncmenu_section is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:4100
      itemcount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:4101
      items : access ncmenu_item;  -- /usr/local/include/notcurses/notcurses.h:4102
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:4103
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4099

   type ncmenu_options is record
      sections : access ncmenu_section;  -- /usr/local/include/notcurses/notcurses.h:4110
      sectioncount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:4111
      headerchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4112
      sectionchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4113
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4114
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4109

   type ncmenu is null record;   -- incomplete struct

   function ncmenu_create (n : access ncplane; opts : access constant ncmenu_options) return access ncmenu  -- /usr/local/include/notcurses/notcurses.h:4118
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_create";

   function ncmenu_unroll (n : access ncmenu; sectionidx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4123
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_unroll";

   function ncmenu_rollup (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:4126
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_rollup";

   function ncmenu_nextsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:4130
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextsection";

   function ncmenu_prevsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:4131
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_prevsection";

   function ncmenu_nextitem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:4135
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextitem";

   function ncmenu_previtem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:4136
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_previtem";

   function ncmenu_item_set_status
     (n : access ncmenu;
      section : Interfaces.C.Strings.chars_ptr;
      item : Interfaces.C.Strings.chars_ptr;
      enabled : Extensions.bool) return int  -- /usr/local/include/notcurses/notcurses.h:4139
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_item_set_status";

   function ncmenu_selected (n : access constant ncmenu; ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4145
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_selected";

   function ncmenu_mouse_selected
     (n : access constant ncmenu;
      click : access constant ncinput;
      ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4151
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_mouse_selected";

   function ncmenu_plane (n : access ncmenu) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4155
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_plane";

   function ncmenu_offer_input (n : access ncmenu; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4166
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_offer_input";

   procedure ncmenu_destroy (n : access ncmenu)  -- /usr/local/include/notcurses/notcurses.h:4170
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_destroy";

   type ncprogbar_options is record
      ulchannel : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:4185
      urchannel : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:4186
      blchannel : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:4187
      brchannel : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:4188
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4189
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4184

   function ncprogbar_create (n : access ncplane; opts : access constant ncprogbar_options) return access ncprogbar  -- /usr/local/include/notcurses/notcurses.h:4194
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_create";

   function ncprogbar_plane (n : access ncprogbar) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4198
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_plane";

   function ncprogbar_set_progress (n : access ncprogbar; p : double) return int  -- /usr/local/include/notcurses/notcurses.h:4202
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_set_progress";

   function ncprogbar_progress (n : access constant ncprogbar) return double  -- /usr/local/include/notcurses/notcurses.h:4206
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_progress";

   procedure ncprogbar_destroy (n : access ncprogbar)  -- /usr/local/include/notcurses/notcurses.h:4210
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_destroy";

   type nctabbed_options is record
      selchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4219
      hdrchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4220
      sepchan : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4221
      separator : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:4222
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4223
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4218

   type tabcb is access procedure
        (arg1 : access nctab;
         arg2 : access ncplane;
         arg3 : System.Address)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:4229

   function nctabbed_create (n : access ncplane; opts : access constant nctabbed_options) return access nctabbed  -- /usr/local/include/notcurses/notcurses.h:4236
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_create";

   procedure nctabbed_destroy (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:4242
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_destroy";

   procedure nctabbed_redraw (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:4247
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_redraw";

   procedure nctabbed_ensure_selected_header_visible (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:4253
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_ensure_selected_header_visible";

   function nctabbed_selected (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4257
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selected";

   function nctabbed_leftmost (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4261
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_leftmost";

   function nctabbed_tabcount (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:4265
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_tabcount";

   function nctabbed_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4269
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_plane";

   function nctabbed_content_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4273
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_content_plane";

   function nctab_cb (t : access nctab) return tabcb  -- /usr/local/include/notcurses/notcurses.h:4277
   with Import => True,
        Convention => C,
        External_Name => "nctab_cb";

   function nctab_name (t : access nctab) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4281
   with Import => True,
        Convention => C,
        External_Name => "nctab_name";

   function nctab_name_width (t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:4285
   with Import => True,
        Convention => C,
        External_Name => "nctab_name_width";

   function nctab_userptr (t : access nctab) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4289
   with Import => True,
        Convention => C,
        External_Name => "nctab_userptr";

   function nctab_next (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4293
   with Import => True,
        Convention => C,
        External_Name => "nctab_next";

   function nctab_prev (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4297
   with Import => True,
        Convention => C,
        External_Name => "nctab_prev";

   function nctabbed_add
     (nt : access nctabbed;
      after : access nctab;
      before : access nctab;
      tcb : tabcb;
      name : Interfaces.C.Strings.chars_ptr;
      opaque : System.Address) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4309
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_add";

   function nctabbed_del (nt : access nctabbed; t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:4319
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_del";

   function nctab_move
     (nt : access nctabbed;
      t : access nctab;
      after : access nctab;
      before : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:4325
   with Import => True,
        Convention => C,
        External_Name => "nctab_move";

   procedure nctab_move_right (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:4330
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_right";

   procedure nctab_move_left (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:4334
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_left";

   procedure nctabbed_rotate (nt : access nctabbed; amt : int)  -- /usr/local/include/notcurses/notcurses.h:4340
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_rotate";

   function nctabbed_next (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4345
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_next";

   function nctabbed_prev (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4350
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_prev";

   function nctabbed_select (nt : access nctabbed; t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:4354
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_select";

   procedure nctabbed_channels
     (nt : access nctabbed;
      hdrchan : access Interfaces.Unsigned_64;
      selchan : access Interfaces.Unsigned_64;
      sepchan : access Interfaces.Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:4359
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_channels";

   function nctabbed_hdrchan (nt : access nctabbed) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4364
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_hdrchan";

   function nctabbed_selchan (nt : access nctabbed) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4371
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selchan";

   function nctabbed_sepchan (nt : access nctabbed) return Interfaces.Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4378
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_sepchan";

   function nctabbed_separator (nt : access nctabbed) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4387
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator";

   function nctabbed_separator_width (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:4391
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator_width";

   procedure nctabbed_set_hdrchan (nt : access nctabbed; chan : Interfaces.Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:4395
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_hdrchan";

   procedure nctabbed_set_selchan (nt : access nctabbed; chan : Interfaces.Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:4399
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_selchan";

   procedure nctabbed_set_sepchan (nt : access nctabbed; chan : Interfaces.Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:4403
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_sepchan";

   function nctab_set_cb (t : access nctab; newcb : tabcb) return tabcb  -- /usr/local/include/notcurses/notcurses.h:4407
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_cb";

   function nctab_set_name (t : access nctab; newname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:4411
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_name";

   function nctab_set_userptr (t : access nctab; newopaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:4415
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_userptr";

   function nctabbed_set_separator (nt : access nctabbed; separator : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:4420
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_separator";

   type ncplot_options is record
      maxchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4469
      minchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4470
      legendstyle : aliased Interfaces.Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:4472
      gridtype : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:4475
      rangex : aliased int;  -- /usr/local/include/notcurses/notcurses.h:4480
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:4481
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4482
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4466

   function ncuplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : Interfaces.Unsigned_64;
      maxy : Interfaces.Unsigned_64) return access ncuplot  -- /usr/local/include/notcurses/notcurses.h:4489
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_create";

   function ncdplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : double;
      maxy : double) return access ncdplot  -- /usr/local/include/notcurses/notcurses.h:4493
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_create";

   function ncuplot_plane (n : access ncuplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4498
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_plane";

   function ncdplot_plane (n : access ncdplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4501
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_plane";

   function ncuplot_add_sample
     (n : access ncuplot;
      x : Interfaces.Unsigned_64;
      y : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:4508
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_add_sample";

   function ncdplot_add_sample
     (n : access ncdplot;
      x : Interfaces.Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:4510
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_add_sample";

   function ncuplot_set_sample
     (n : access ncuplot;
      x : Interfaces.Unsigned_64;
      y : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:4512
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_set_sample";

   function ncdplot_set_sample
     (n : access ncdplot;
      x : Interfaces.Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:4514
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_set_sample";

   function ncuplot_sample
     (n : access constant ncuplot;
      x : Interfaces.Unsigned_64;
      y : access Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:4517
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_sample";

   function ncdplot_sample
     (n : access constant ncdplot;
      x : Interfaces.Unsigned_64;
      y : access double) return int  -- /usr/local/include/notcurses/notcurses.h:4519
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_sample";

   procedure ncuplot_destroy (n : access ncuplot)  -- /usr/local/include/notcurses/notcurses.h:4522
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_destroy";

   procedure ncdplot_destroy (n : access ncdplot)  -- /usr/local/include/notcurses/notcurses.h:4523
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_destroy";

   type ncfdplane_callback is access function
        (arg1 : access ncfdplane;
         arg2 : System.Address;
         arg3 : Interfaces.C.size_t;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:4525

   type ncfdplane_done_cb is access function
        (arg1 : access ncfdplane;
         arg2 : int;
         arg3 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:4526

   type ncfdplane_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:4534
      follow : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:4535
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4536
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4533

   function ncfdplane_create
     (n : access ncplane;
      opts : access constant ncfdplane_options;
      fd : int;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncfdplane  -- /usr/local/include/notcurses/notcurses.h:4541
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_create";

   function ncfdplane_plane (n : access ncfdplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4545
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_plane";

   function ncfdplane_destroy (n : access ncfdplane) return int  -- /usr/local/include/notcurses/notcurses.h:4548
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_destroy";

   type ncsubproc_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:4551
      restart_period : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4552
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4553
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4550

   function ncsubproc_createv
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:4557
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createv";

   function ncsubproc_createvp
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:4562
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createvp";

   function ncsubproc_createvpe
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      env : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:4567
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createvpe";

   function ncsubproc_plane (n : access ncsubproc) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4573
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_plane";

   function ncsubproc_destroy (n : access ncsubproc) return int  -- /usr/local/include/notcurses/notcurses.h:4576
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_destroy";

   function ncplane_qrcode
     (n : access ncplane;
      ymax : access unsigned;
      xmax : access unsigned;
      data : System.Address;
      len : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:4583
   with Import => True,
        Convention => C,
        External_Name => "ncplane_qrcode";

   type ncreader_options is record
      tchannels : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4598
      tattrword : aliased Interfaces.Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:4599
      flags : aliased Interfaces.Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:4600
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:4597

   function ncreader_create (n : access ncplane; opts : access constant ncreader_options) return access ncreader  -- /usr/local/include/notcurses/notcurses.h:4606
   with Import => True,
        Convention => C,
        External_Name => "ncreader_create";

   function ncreader_clear (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:4610
   with Import => True,
        Convention => C,
        External_Name => "ncreader_clear";

   function ncreader_plane (n : access ncreader) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4613
   with Import => True,
        Convention => C,
        External_Name => "ncreader_plane";

   function ncreader_offer_input (n : access ncreader; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4619
   with Import => True,
        Convention => C,
        External_Name => "ncreader_offer_input";

   function ncreader_move_left (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:4624
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_left";

   function ncreader_move_right (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:4626
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_right";

   function ncreader_move_up (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:4628
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_up";

   function ncreader_move_down (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:4630
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_down";

   function ncreader_write_egc (n : access ncreader; egc : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:4635
   with Import => True,
        Convention => C,
        External_Name => "ncreader_write_egc";

   function ncreader_contents (n : access constant ncreader) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4639
   with Import => True,
        Convention => C,
        External_Name => "ncreader_contents";

   procedure ncreader_destroy (n : access ncreader; contents : System.Address)  -- /usr/local/include/notcurses/notcurses.h:4644
   with Import => True,
        Convention => C,
        External_Name => "ncreader_destroy";

   function notcurses_accountname return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4647
   with Import => True,
        Convention => C,
        External_Name => "notcurses_accountname";

   function notcurses_hostname return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4650
   with Import => True,
        Convention => C,
        External_Name => "notcurses_hostname";

   function notcurses_osversion return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4653
   with Import => True,
        Convention => C,
        External_Name => "notcurses_osversion";

   procedure notcurses_debug (nc : access constant notcurses; debugfp : access Interfaces.C_Streams.FILEs)  -- /usr/local/include/notcurses/notcurses.h:4658
   with Import => True,
        Convention => C,
        External_Name => "notcurses_debug";

end Notcurses_Thin;
