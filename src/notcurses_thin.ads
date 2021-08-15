--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System.OS_Interface;
with Interfaces.C_Streams;
with System;

package Notcurses_Thin is

   type Unsigned_Max is mod 2 ** Long_Long_Integer'Size;

   --  unsupported macro: RESTRICT restrict
   --  unsupported macro: NCALIGN_TOP NCALIGN_LEFT
   --  unsupported macro: NCALIGN_BOTTOM NCALIGN_RIGHT
   NCALPHA_HIGHCONTRAST : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:112
   NCALPHA_TRANSPARENT : constant := 16#20000000#;  --  /usr/local/include/notcurses/notcurses.h:113
   NCALPHA_BLEND : constant := 16#10000000#;  --  /usr/local/include/notcurses/notcurses.h:114
   NCALPHA_OPAQUE : constant := 16#00000000#;  --  /usr/local/include/notcurses/notcurses.h:115

   NCPALETTESIZE : constant := 256;  --  /usr/local/include/notcurses/notcurses.h:118

   NC_NOBACKGROUND_MASK : constant := 16#8700000000000000#;  --  /usr/local/include/notcurses/notcurses.h:123

   NC_BGDEFAULT_MASK : constant := 16#0000000040000000#;  --  /usr/local/include/notcurses/notcurses.h:125
   --  unsupported macro: NC_FGDEFAULT_MASK (NC_BGDEFAULT_MASK << 32u)

   NC_BG_RGB_MASK : constant := 16#0000000000ffffff#;  --  /usr/local/include/notcurses/notcurses.h:129
   --  unsupported macro: NC_FG_RGB_MASK (NC_BG_RGB_MASK << 32u)

   NC_BG_PALETTE : constant := 16#0000000008000000#;  --  /usr/local/include/notcurses/notcurses.h:134
   --  unsupported macro: NC_FG_PALETTE (NC_BG_PALETTE << 32u)

   NC_BG_ALPHA_MASK : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:139
   --  unsupported macro: NC_FG_ALPHA_MASK (NC_BG_ALPHA_MASK << 32u)
   --  arg-macro: function NCCHANNEL_INITIALIZER (r, g, b)
   --    return ((uint32_t)r << 16) + ((uint32_t)g << 8) + (b) + NC_BGDEFAULT_MASK;
   --  arg-macro: function NCCHANNELS_INITIALIZER (fr, fg, fb, br, bg, bb)
   --    return (NCCHANNEL_INITIALIZER(fr, fg, fb) << 32) + (NCCHANNEL_INITIALIZER(br, bg, bb));
   --  arg-macro: procedure CELL_INITIALIZER (c, s, chan)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)((wcwidth(c) < 0  or else  notc) ? 1 : wcwidth(c)), .stylemask := (s), .channels := (chan), }
   --  arg-macro: procedure CELL_CHAR_INITIALIZER (c)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)((wcwidth(c) < 0  or else  notc) ? 1 : wcwidth(c)), .stylemask := 0, .channels := 0, }
   --  unsupported macro: CELL_TRIVIAL_INITIALIZER { .gcluster = 0, .gcluster_backstop = 0, .width = 1, .stylemask = 0, .channels = 0, }

   NCSTYLE_MASK : constant := 16#ffff#;  --  /usr/local/include/notcurses/notcurses.h:687
   NCSTYLE_ITALIC : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:688
   NCSTYLE_UNDERLINE : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:689
   NCSTYLE_UNDERCURL : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:690
   NCSTYLE_BOLD : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:691
   NCSTYLE_STRUCK : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:692
   NCSTYLE_NONE : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:693

   NCOPTION_INHIBIT_SETLOCALE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:858

   NCOPTION_NO_CLEAR_BITMAPS : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:864

   NCOPTION_NO_WINCH_SIGHANDLER : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:868

   NCOPTION_NO_QUIT_SIGHANDLERS : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:873

   NCOPTION_PRESERVE_CURSOR : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:879

   NCOPTION_SUPPRESS_BANNERS : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:883

   NCOPTION_NO_ALTERNATE_SCREEN : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:887

   NCOPTION_NO_FONT_CHANGES : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:893

   NCPLANE_OPTION_HORALIGNED : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:1165

   NCPLANE_OPTION_VERALIGNED : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:1167

   NCPLANE_OPTION_MARGINALIZED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:1174

   NCPLANE_OPTION_FIXED : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:1178

   WCHAR_MAX_UTF8BYTES : constant := 6;  --  /usr/local/include/notcurses/notcurses.h:1756

   NCBOXMASK_TOP : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:1999
   NCBOXMASK_RIGHT : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2000
   NCBOXMASK_BOTTOM : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:2001
   NCBOXMASK_LEFT : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:2002
   NCBOXGRAD_TOP : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:2003
   NCBOXGRAD_RIGHT : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:2004
   NCBOXGRAD_BOTTOM : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:2005
   NCBOXGRAD_LEFT : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:2006
   NCBOXCORNER_MASK : constant := 16#0300#;  --  /usr/local/include/notcurses/notcurses.h:2007
   NCBOXCORNER_SHIFT : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:2008

   NCVISUAL_OPTION_NODEGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:2648
   NCVISUAL_OPTION_BLEND : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2649
   NCVISUAL_OPTION_HORALIGNED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:2650
   NCVISUAL_OPTION_VERALIGNED : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:2651
   NCVISUAL_OPTION_ADDALPHA : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:2652
   NCVISUAL_OPTION_CHILDPLANE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:2653
   NCVISUAL_OPTION_NOINTERPOLATE : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:2654

   NCREEL_OPTION_INFINITESCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:2964

   NCREEL_OPTION_CIRCULAR : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2968

   PREFIXCOLUMNS : constant := 7;  --  /usr/local/include/notcurses/notcurses.h:3091
   IPREFIXCOLUMNS : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:3092
   BPREFIXCOLUMNS : constant := 9;  --  /usr/local/include/notcurses/notcurses.h:3093
   --  unsupported macro: PREFIXSTRLEN (PREFIXCOLUMNS + 1)
   --  unsupported macro: IPREFIXSTRLEN (IPREFIXCOLUMNS + 1)
   --  unsupported macro: BPREFIXSTRLEN (BPREFIXCOLUMNS + 1)
   --  arg-macro: function NCMETRICFWIDTH (x, cols)
   --    return (int)(strlen(x) - ncstrwidth(x) + (cols));
   --  arg-macro: procedure PREFIXFMT (x)
   --    NCMETRICFWIDTH((x), PREFIXCOLUMNS), (x)
   --  arg-macro: procedure IPREFIXFMT (x)
   --    NCMETRIXFWIDTH((x), IPREFIXCOLUMNS), (x)
   --  arg-macro: procedure BPREFIXFMT (x)
   --    NCMETRICFWIDTH((x), BPREFIXCOLUMNS), (x)

   NCMENU_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3363
   NCMENU_OPTION_HIDING : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3364

   NCPROGBAR_OPTION_RETROGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3439

   NCTABBED_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3473

   NCPLOT_OPTION_LABELTICKSD : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3716
   NCPLOT_OPTION_EXPONENTIALD : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3717
   NCPLOT_OPTION_VERTICALI : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:3718
   NCPLOT_OPTION_NODEGRADE : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:3719
   NCPLOT_OPTION_DETECTMAXONLY : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:3720
   NCPLOT_OPTION_PRINTSAMPLE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:3721

   NCREADER_OPTION_HORSCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3844

   NCREADER_OPTION_VERSCROLL : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3846

   NCREADER_OPTION_NOCMDKEYS : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:3848

   NCREADER_OPTION_CURSOR : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:3851
   --  unsupported macro: CELL_ALPHA_HIGHCONTRAST NCALPHA_HIGHCONTRAST
   --  unsupported macro: CELL_ALPHA_TRANSPARENT NCALPHA_TRANSPARENT
   --  unsupported macro: CELL_ALPHA_BLEND NCALPHA_BLEND
   --  unsupported macro: CELL_ALPHA_OPAQUE NCALPHA_OPAQUE

   NCSTYLE_PROTECT : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4460
   NCSTYLE_STANDOUT : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4461
   NCSTYLE_REVERSE : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4462
   NCSTYLE_INVIS : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4463
   NCSTYLE_DIM : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4464
   NCSTYLE_BLINK : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:4465

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
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:66

   type ncalign_e is
     (NCALIGN_UNALIGNED,
      NCALIGN_LEFT,
      NCALIGN_CENTER,
      NCALIGN_RIGHT)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:74

   type ncscale_e is
     (NCSCALE_NONE,
      NCSCALE_SCALE,
      NCSCALE_STRETCH,
      NCSCALE_NONE_HIRES,
      NCSCALE_SCALE_HIRES)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:91

   function ncstrwidth (mbs : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:95
   with Import => True,
        Convention => C,
        External_Name => "ncstrwidth";

   function notcurses_accountname return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:98
   with Import => True,
        Convention => C,
        External_Name => "notcurses_accountname";

   function notcurses_hostname return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:101
   with Import => True,
        Convention => C,
        External_Name => "notcurses_hostname";

   function notcurses_ucs32_to_utf8
     (ucs32 : access Unsigned_32;
      ucs32count : unsigned;
      resultbuf : access unsigned_char;
      buflen : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:108
   with Import => True,
        Convention => C,
        External_Name => "notcurses_ucs32_to_utf8";

   function ncchannel_r (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:158
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_r";

   function ncchannel_g (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:164
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_g";

   function ncchannel_b (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:170
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_b";

   function ncchannel_alpha (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:176
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_alpha";

   function ncchannel_rgb8
     (channel : Unsigned_32;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:182
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_rgb8";

   function ncchannel_set_rgb8
     (channel : access Unsigned_32;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:193
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8";

   procedure ncchannel_set_rgb8_clipped
     (channel : access Unsigned_32;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:206
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8_clipped";

   function ncchannel_set (channel : access Unsigned_32; rgb : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:231
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set";

   function ncchannel_palindex (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:240
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex";

   function ncchannel_set_alpha (channel : access Unsigned_32; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:246
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_alpha";

   function ncchannel_set_palindex (channel : access Unsigned_32; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:258
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_palindex";

   function ncchannel_default_p (channel : Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:272
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_default_p";

   function ncchannel_palindex_p (channel : Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:278
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex_p";

   function ncchannel_set_default (channel : access Unsigned_32) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:284
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_default";

   function ncchannels_bchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:290
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bchannel";

   function ncchannels_fchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:296
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fchannel";

   function ncchannels_reverse (channels : Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:303
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_reverse";

   function ncchannels_set_bchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:315
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bchannel";

   function ncchannels_set_fchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:321
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fchannel";

   function ncchannels_combine (fchan : Unsigned_32; bchan : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:326
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_combine";

   function ncchannels_fg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:334
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex";

   function ncchannels_bg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:339
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex";

   function ncchannels_fg_rgb (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:345
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb";

   function ncchannels_bg_rgb (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:351
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb";

   function ncchannels_fg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:357
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_alpha";

   function ncchannels_bg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:363
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_alpha";

   function ncchannels_fg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:369
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb8";

   function ncchannels_bg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:375
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb8";

   function ncchannels_set_fg_rgb8
     (channels : access Unsigned_64;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:382
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8";

   procedure ncchannels_set_fg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:393
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8_clipped";

   function ncchannels_set_fg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:401
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_alpha";

   function ncchannels_set_fg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:411
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_palindex";

   function ncchannels_set_fg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:422
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb";

   function ncchannels_set_bg_rgb8
     (channels : access Unsigned_64;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:434
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8";

   procedure ncchannels_set_bg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:445
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8_clipped";

   function ncchannels_set_bg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:453
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_alpha";

   function ncchannels_set_bg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:468
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_palindex";

   function ncchannels_set_bg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:479
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb";

   function ncchannels_fg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:490
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_default_p";

   function ncchannels_fg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:496
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex_p";

   function ncchannels_bg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:504
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_default_p";

   function ncchannels_bg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:510
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex_p";

   function ncchannels_set_fg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:516
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_default";

   function ncchannels_set_bg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:525
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_default";

   type nccell is record
      gcluster : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:612
      gcluster_backstop : aliased Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:613
      width : aliased Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:621
      stylemask : aliased Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:622
      channels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:642
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:582

   procedure nccell_init (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:659
   with Import => True,
        Convention => C,
        External_Name => "nccell_init";

   function nccell_load
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:666
   with Import => True,
        Convention => C,
        External_Name => "nccell_load";

   function nccell_prime
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:670
   with Import => True,
        Convention => C,
        External_Name => "nccell_prime";

   function nccell_duplicate
     (n : access ncplane;
      targ : access nccell;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:680
   with Import => True,
        Convention => C,
        External_Name => "nccell_duplicate";

   procedure nccell_release (n : access ncplane; c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:683
   with Import => True,
        Convention => C,
        External_Name => "nccell_release";

   procedure nccell_set_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:698
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_styles";

   function nccell_styles (c : access constant nccell) return unsigned  -- /usr/local/include/notcurses/notcurses.h:704
   with Import => True,
        Convention => C,
        External_Name => "nccell_styles";

   procedure nccell_on_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:711
   with Import => True,
        Convention => C,
        External_Name => "nccell_on_styles";

   procedure nccell_off_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:717
   with Import => True,
        Convention => C,
        External_Name => "nccell_off_styles";

   procedure nccell_set_fg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:723
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_default";

   procedure nccell_set_bg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:729
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_default";

   function nccell_set_fg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:734
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_alpha";

   function nccell_set_bg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:739
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_alpha";

   function nccell_double_wide_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:745
   with Import => True,
        Convention => C,
        External_Name => "nccell_double_wide_p";

   function nccell_wide_right_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:751
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_right_p";

   function nccell_wide_left_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:757
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_left_p";

   function nccell_extended_gcluster (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:763
   with Import => True,
        Convention => C,
        External_Name => "nccell_extended_gcluster";

   function nccell_cols (c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:768
   with Import => True,
        Convention => C,
        External_Name => "nccell_cols";

   function nccell_strdup (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:775
   with Import => True,
        Convention => C,
        External_Name => "nccell_strdup";

   function nccell_extract
     (n : access constant ncplane;
      c : access constant nccell;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:781
   with Import => True,
        Convention => C,
        External_Name => "nccell_extract";

   function nccellcmp
     (n1 : access constant ncplane;
      c1 : access constant nccell;
      n2 : access constant ncplane;
      c2 : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:798
   with Import => True,
        Convention => C,
        External_Name => "nccellcmp";

   function nccell_load_char
     (n : access ncplane;
      c : access nccell;
      ch : char) return int  -- /usr/local/include/notcurses/notcurses.h:812
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_char";

   function nccell_load_egc32
     (n : access ncplane;
      c : access nccell;
      egc : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:822
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_egc32";

   subtype ncloglevel_e is int;
   NCLOGLEVEL_SILENT : constant int := -1;
   NCLOGLEVEL_PANIC : constant int := 0;
   NCLOGLEVEL_FATAL : constant int := 1;
   NCLOGLEVEL_ERROR : constant int := 2;
   NCLOGLEVEL_WARNING : constant int := 3;
   NCLOGLEVEL_INFO : constant int := 4;
   NCLOGLEVEL_VERBOSE : constant int := 5;
   NCLOGLEVEL_DEBUG : constant int := 6;
   NCLOGLEVEL_TRACE : constant int := 7;  -- /usr/local/include/notcurses/notcurses.h:846

   type notcurses_options is record
      termtype : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:900
      renderfp : Interfaces.C_Streams.FILEs;  -- /usr/local/include/notcurses/notcurses.h:903
      loglevel : aliased ncloglevel_e;  -- /usr/local/include/notcurses/notcurses.h:906
      margin_t : aliased int;  -- /usr/local/include/notcurses/notcurses.h:911
      margin_r : aliased int;  -- /usr/local/include/notcurses/notcurses.h:911
      margin_b : aliased int;  -- /usr/local/include/notcurses/notcurses.h:911
      margin_l : aliased int;  -- /usr/local/include/notcurses/notcurses.h:911
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:915
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:896

   function notcurses_lex_margins (op : Interfaces.C.Strings.chars_ptr; opts : access notcurses_options) return int  -- /usr/local/include/notcurses/notcurses.h:921
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_margins";

   function notcurses_lex_blitter (op : Interfaces.C.Strings.chars_ptr; blitter : access ncblitter_e) return int  -- /usr/local/include/notcurses/notcurses.h:924
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_blitter";

   function notcurses_str_blitter (blitter : ncblitter_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:927
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_blitter";

   function notcurses_lex_scalemode (op : Interfaces.C.Strings.chars_ptr; scalemode : access ncscale_e) return int  -- /usr/local/include/notcurses/notcurses.h:931
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_scalemode";

   function notcurses_str_scalemode (scalemode : ncscale_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:934
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_scalemode";

   function notcurses_init (opts : access constant notcurses_options; fp : Interfaces.C_Streams.FILEs) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:940
   with Import => True,
        Convention => C,
        External_Name => "notcurses_init";

   function notcurses_core_init (opts : access constant notcurses_options; fp : Interfaces.C_Streams.FILEs) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:944
   with Import => True,
        Convention => C,
        External_Name => "notcurses_core_init";

   function notcurses_stop (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:947
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stop";

   function ncpile_top (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:950
   with Import => True,
        Convention => C,
        External_Name => "ncpile_top";

   function ncpile_bottom (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:953
   with Import => True,
        Convention => C,
        External_Name => "ncpile_bottom";

   function ncpile_render (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:957
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render";

   function ncpile_rasterize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:962
   with Import => True,
        Convention => C,
        External_Name => "ncpile_rasterize";

   function notcurses_render (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:965
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render";

   function ncpile_render_to_buffer
     (p : access ncplane;
      buf : System.Address;
      buflen : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:971
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render_to_buffer";

   function ncpile_render_to_file (p : access ncplane; fp : Interfaces.C_Streams.FILEs) return int  -- /usr/local/include/notcurses/notcurses.h:975
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render_to_file";

   function notcurses_top (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:978
   with Import => True,
        Convention => C,
        External_Name => "notcurses_top";

   function notcurses_bottom (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:981
   with Import => True,
        Convention => C,
        External_Name => "notcurses_bottom";

   procedure notcurses_drop_planes (nc : access notcurses)  -- /usr/local/include/notcurses/notcurses.h:984
   with Import => True,
        Convention => C,
        External_Name => "notcurses_drop_planes";

   function nckey_supppuab_p (w : Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1003
   with Import => True,
        Convention => C,
        External_Name => "nckey_supppuab_p";

   function nckey_mouse_p (r : Unsigned_32) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1009
   with Import => True,
        Convention => C,
        External_Name => "nckey_mouse_p";

   type ncinput is record
      id : aliased Wide_Wide_Character;  -- /usr/local/include/notcurses/notcurses.h:1015
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1016
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1017
      alt : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1018
      shift : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1019
      ctrl : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1020
      seqnum : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1021
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1014

   function ncinput_equal_p (n1 : access constant ncinput; n2 : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1027
   with Import => True,
        Convention => C,
        External_Name => "ncinput_equal_p";

   function notcurses_get
     (n : access notcurses;
      ts : access constant System.OS_Interface.timespec;
      ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1047
   with Import => True,
        Convention => C,
        External_Name => "notcurses_get";

   function notcurses_inputready_fd (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1055
   with Import => True,
        Convention => C,
        External_Name => "notcurses_inputready_fd";

   function notcurses_getc_nblock (n : access notcurses; ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1061
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc_nblock";

   function notcurses_getc_blocking (n : access notcurses; ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1069
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc_blocking";

   function ncinput_nomod_p (ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1074
   with Import => True,
        Convention => C,
        External_Name => "ncinput_nomod_p";

   function notcurses_mouse_enable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1081
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mouse_enable";

   function notcurses_mouse_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1084
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mouse_disable";

   function notcurses_linesigs_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1088
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_disable";

   function notcurses_linesigs_enable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1092
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_enable";

   function notcurses_refresh
     (n : access notcurses;
      y : access int;
      x : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1099
   with Import => True,
        Convention => C,
        External_Name => "notcurses_refresh";

   function ncplane_notcurses (n : access constant ncplane) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:1102
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses";

   function ncplane_notcurses_const (n : access constant ncplane) return access constant notcurses  -- /usr/local/include/notcurses/notcurses.h:1103
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses_const";

   procedure ncplane_dim_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1106
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_yx";

   function notcurses_stdplane (nc : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1111
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane";

   function notcurses_stdplane_const (nc : access constant notcurses) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1112
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane_const";

   function notcurses_stddim_yx
     (nc : access notcurses;
      y : access int;
      x : access int) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1116
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx";

   function notcurses_stddim_yx_const
     (nc : access constant notcurses;
      y : access int;
      x : access int) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1123
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx_const";

   function ncplane_dim_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1130
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_y";

   function ncplane_dim_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1137
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_x";

   procedure ncplane_pixelgeom
     (n : access constant ncplane;
      pxy : access int;
      pxx : access int;
      celldimy : access int;
      celldimx : access int;
      maxbmapy : access int;
      maxbmapx : access int)  -- /usr/local/include/notcurses/notcurses.h:1147
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pixelgeom";

   procedure notcurses_term_dim_yx
     (n : access constant notcurses;
      rows : access int;
      cols : access int)  -- /usr/local/include/notcurses/notcurses.h:1154
   with Import => True,
        Convention => C,
        External_Name => "notcurses_term_dim_yx";

   function notcurses_at_yx
     (nc : access notcurses;
      yoff : int;
      xoff : int;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1161
   with Import => True,
        Convention => C,
        External_Name => "notcurses_at_yx";

   type ncplane_options is record
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1181
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1182
      rows : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1183
      cols : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1184
      userptr : System.Address;  -- /usr/local/include/notcurses/notcurses.h:1185
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:1186
      resizecb : access function (arg1 : access ncplane) return int;  -- /usr/local/include/notcurses/notcurses.h:1187
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1188
      margin_b : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1189
      margin_r : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1189
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1180

   function ncplane_create (n : access ncplane; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1197
   with Import => True,
        Convention => C,
        External_Name => "ncplane_create";

   function ncpile_create (nc : access notcurses; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1201
   with Import => True,
        Convention => C,
        External_Name => "ncpile_create";

   function ncplane_resize_maximize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1205
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_maximize";

   function ncplane_resize_marginalized (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1210
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_marginalized";

   function ncplane_resize_realign (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1214
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_realign";

   procedure ncplane_set_resizecb (n : access ncplane; resizecb : access function (arg1 : access ncplane) return int)  -- /usr/local/include/notcurses/notcurses.h:1218
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_resizecb";

   function ncplane_resizecb (n : access constant ncplane) return access function (arg1 : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1221
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resizecb";

   function ncplane_reparent (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1229
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent";

   function ncplane_reparent_family (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1235
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent_family";

   function ncplane_dup (n : access constant ncplane; opaque : System.Address) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1243
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dup";

   procedure ncplane_translate
     (src : access constant ncplane;
      dst : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1249
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate";

   function ncplane_translate_abs
     (n : access constant ncplane;
      y : access int;
      x : access int) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1256
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate_abs";

   function ncplane_set_scrolling (n : access ncplane; scrollp : Extensions.bool) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1262
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_scrolling";

   function ncplane_scrolling_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1265
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrolling_p";

   type anon2243_array2245 is array (0 .. 255) of aliased Unsigned_32;
   type ncpalette is record
      chans : aliased anon2243_array2245;  -- /usr/local/include/notcurses/notcurses.h:1274
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1273

   function ncpalette_new (nc : access notcurses) return access ncpalette  -- /usr/local/include/notcurses/notcurses.h:1280
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_new";

   function ncpalette_use (nc : access notcurses; p : access constant ncpalette) return int  -- /usr/local/include/notcurses/notcurses.h:1284
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_use";

   function ncpalette_set_rgb8
     (p : access ncpalette;
      idx : int;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1288
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set_rgb8";

   function ncpalette_set
     (p : access ncpalette;
      idx : int;
      rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1296
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set";

   function ncpalette_get_rgb8
     (p : access constant ncpalette;
      idx : int;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1304
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_get_rgb8";

   procedure ncpalette_free (p : access ncpalette)  -- /usr/local/include/notcurses/notcurses.h:1312
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_free";

   type nccapabilities is record
      colors : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1316
      utf8 : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1317
      rgb : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1318
      can_change_colors : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1319
      quadrants : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1321
      sextants : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1322
      braille : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:1323
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1315

   function notcurses_supported_styles (nc : access constant notcurses) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1330
   with Import => True,
        Convention => C,
        External_Name => "notcurses_supported_styles";

   function notcurses_palette_size (nc : access constant notcurses) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1336
   with Import => True,
        Convention => C,
        External_Name => "notcurses_palette_size";

   function notcurses_detected_terminal (nc : access constant notcurses) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1341
   with Import => True,
        Convention => C,
        External_Name => "notcurses_detected_terminal";

   function notcurses_cantruecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1345
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cantruecolor";

   function notcurses_canfade (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1349
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canfade";

   function nccapability_canchangecolor (caps : access constant nccapabilities) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1356
   with Import => True,
        Convention => C,
        External_Name => "nccapability_canchangecolor";

   function notcurses_canchangecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1367
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canchangecolor";

   function notcurses_canopen_images (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1371
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_images";

   function notcurses_canopen_videos (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1375
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_videos";

   function notcurses_canutf8 (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1379
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canutf8";

   function notcurses_canhalfblock (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1383
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canhalfblock";

   function notcurses_canquadrant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1387
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canquadrant";

   function notcurses_cansextant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1391
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cansextant";

   function notcurses_canbraille (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1395
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canbraille";

   function notcurses_check_pixel_support (nc : access constant notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1399
   with Import => True,
        Convention => C,
        External_Name => "notcurses_check_pixel_support";

   type ncstats is record
      renders : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1406
      writeouts : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1407
      failed_renders : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1408
      failed_writeouts : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1409
      render_bytes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1411
      render_max_bytes : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1412
      render_min_bytes : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1413
      render_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1414
      render_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1415
      render_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1416
      writeout_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1417
      writeout_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1418
      writeout_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1419
      cellelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1420
      cellemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1421
      fgelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1422
      fgemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1423
      bgelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1424
      bgemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1425
      defaultelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1426
      defaultemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1427
      refreshes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1428
      appsync_updates : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1429
      fbbytes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1432
      planes : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1433
      raster_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1436
      raster_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1437
      raster_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1438
      sprixelemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1439
      sprixelelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1440
      sprixelbytes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1441
      input_errors : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1442
      input_events : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1443
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1404

   function notcurses_stats_alloc (nc : access constant notcurses) return access ncstats  -- /usr/local/include/notcurses/notcurses.h:1448
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats_alloc";

   procedure notcurses_stats (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1453
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats";

   procedure notcurses_stats_reset (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1458
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats_reset";

   function ncplane_resize
     (n : access ncplane;
      keepy : int;
      keepx : int;
      keepleny : int;
      keeplenx : int;
      yoff : int;
      xoff : int;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1475
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize";

   function ncplane_resize_simple
     (n : access ncplane;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1481
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_simple";

   function ncplane_destroy (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1495
   with Import => True,
        Convention => C,
        External_Name => "ncplane_destroy";

   function ncplane_set_base_cell (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1501
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base_cell";

   function ncplane_set_base
     (n : access ncplane;
      egc : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1507
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base";

   function ncplane_base (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1512
   with Import => True,
        Convention => C,
        External_Name => "ncplane_base";

   procedure ncplane_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1516
   with Import => True,
        Convention => C,
        External_Name => "ncplane_yx";

   function ncplane_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1518
   with Import => True,
        Convention => C,
        External_Name => "ncplane_y";

   function ncplane_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1519
   with Import => True,
        Convention => C,
        External_Name => "ncplane_x";

   function ncplane_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1524
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_yx";

   function ncplane_moverel
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1529
   with Import => True,
        Convention => C,
        External_Name => "ncplane_moverel";

   procedure ncplane_abs_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1537
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_yx";

   function ncplane_abs_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1539
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_y";

   function ncplane_abs_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1540
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_x";

   function ncplane_parent (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1543
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent";

   function ncplane_parent_const (n : access constant ncplane) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1544
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent_const";

   function ncplane_descendant_p (n : access constant ncplane; ancestor : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1548
   with Import => True,
        Convention => C,
        External_Name => "ncplane_descendant_p";

   procedure ncplane_move_top (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1558
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_top";

   procedure ncplane_move_bottom (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1559
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_bottom";

   function ncplane_move_above (n : access ncplane; above : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1564
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_above";

   function ncplane_move_below (n : access ncplane; below : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1570
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_below";

   function ncplane_below (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1574
   with Import => True,
        Convention => C,
        External_Name => "ncplane_below";

   function ncplane_above (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1578
   with Import => True,
        Convention => C,
        External_Name => "ncplane_above";

   function ncplane_scrollup (n : access ncplane; r : int) return int  -- /usr/local/include/notcurses/notcurses.h:1583
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrollup";

   function ncplane_scrollup_child (n : access ncplane; child : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1589
   with Import => True,
        Convention => C,
        External_Name => "ncplane_scrollup_child";

   function ncplane_rotate_cw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1598
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_cw";

   function ncplane_rotate_ccw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1599
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_ccw";

   function ncplane_at_cursor
     (n : access ncplane;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1604
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor";

   function ncplane_at_cursor_cell (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1609
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor_cell";

   function ncplane_at_yx
     (n : access constant ncplane;
      y : int;
      x : int;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1618
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx";

   function ncplane_at_yx_cell
     (n : access ncplane;
      y : int;
      x : int;
      c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1626
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx_cell";

   function ncplane_contents
     (n : access ncplane;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1632
   with Import => True,
        Convention => C,
        External_Name => "ncplane_contents";

   function ncplane_set_userptr (n : access ncplane; opaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:1638
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_userptr";

   function ncplane_userptr (n : access ncplane) return System.Address  -- /usr/local/include/notcurses/notcurses.h:1639
   with Import => True,
        Convention => C,
        External_Name => "ncplane_userptr";

   procedure ncplane_center_abs
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1641
   with Import => True,
        Convention => C,
        External_Name => "ncplane_center_abs";

   function notcurses_align
     (availu : int;
      align : ncalign_e;
      u : int) return int  -- /usr/local/include/notcurses/notcurses.h:1648
   with Import => True,
        Convention => C,
        External_Name => "notcurses_align";

   function ncplane_halign
     (n : access constant ncplane;
      align : ncalign_e;
      c : int) return int  -- /usr/local/include/notcurses/notcurses.h:1665
   with Import => True,
        Convention => C,
        External_Name => "ncplane_halign";

   function ncplane_valign
     (n : access constant ncplane;
      align : ncalign_e;
      r : int) return int  -- /usr/local/include/notcurses/notcurses.h:1673
   with Import => True,
        Convention => C,
        External_Name => "ncplane_valign";

   function ncplane_cursor_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1680
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_move_yx";

   function ncplane_cursor_move_rel
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1686
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_move_rel";

   procedure ncplane_home (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1690
   with Import => True,
        Convention => C,
        External_Name => "ncplane_home";

   procedure ncplane_cursor_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1694
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_yx";

   function ncplane_channels (n : access constant ncplane) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:1698
   with Import => True,
        Convention => C,
        External_Name => "ncplane_channels";

   function ncplane_styles (n : access constant ncplane) return Unsigned_16  -- /usr/local/include/notcurses/notcurses.h:1702
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles";

   function ncplane_putc_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1709
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc_yx";

   function ncplane_putc (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1714
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc";

   function ncplane_putchar_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1722
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_yx";

   function wcwidth return int  -- /usr/local/include/notcurses/notcurses.h:1723
   with Import => True,
        Convention => C,
        External_Name => "wcwidth";

   function ncplane_putchar (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1729
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar";

   function ncplane_putchar_stained (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1735
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_stained";

   function ncplane_putegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1742
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_yx";

   function ncplane_putegc
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1746
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc";

   function ncplane_putegc_stained
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1752
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_stained";

   function ncplane_putwegc
     (n : access ncplane;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1760
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc";

   function ncplane_putwegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1779
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_yx";

   function ncplane_putwegc_stained
     (n : access ncplane;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1789
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_stained";

   function ncplane_putstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1797
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_yx";

   function ncplane_putstr (n : access ncplane; gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1800
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr";

   function ncplane_putstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1804
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_aligned";

   function ncplane_putstr_stained (n : access ncplane; s : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1809
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_stained";

   function ncplane_putnstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      s : Interfaces.C.size_t;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1817
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_yx";

   function ncplane_putnstr
     (n : access ncplane;
      s : Interfaces.C.size_t;
      gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1820
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr";

   function ncplane_putnstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.size_t;
      gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1824
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_aligned";

   function ncplane_putwstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1830
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_yx";

   function ncplane_putwstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1848
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_aligned";

   function wcswidth return int  -- /usr/local/include/notcurses/notcurses.h:1850
   with Import => True,
        Convention => C,
        External_Name => "wcswidth";

   function ncplane_putwstr_stained (n : access ncplane; gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1858
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_stained";

   function ncplane_putwstr (n : access ncplane; gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1861
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr";

   function ncplane_putwc_yx
     (n : access ncplane;
      y : int;
      x : int;
      w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1869
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_yx";

   function ncplane_putwc (n : access ncplane; w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1876
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc";

   function ncplane_putwc_stained (n : access ncplane; w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1883
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_stained";

   function ncplane_vprintf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1889
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_aligned";

   function ncplane_vprintf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1892
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_yx";

   function ncplane_vprintf
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1896
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf";

   function ncplane_vprintf_stained
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1900
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_stained";

   function ncplane_printf (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1907
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf";

   function ncplane_printf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1920
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_yx";

   function ncplane_printf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1934
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_aligned";

   function ncplane_printf_stained (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1947
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_stained";

   function ncplane_puttext
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      text : Interfaces.C.Strings.chars_ptr;
      bytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:1974
   with Import => True,
        Convention => C,
        External_Name => "ncplane_puttext";

   function ncplane_hline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : int;
      c1 : Unsigned_64;
      c2 : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1983
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline_interp";

   function ncplane_hline
     (n : access ncplane;
      c : access constant nccell;
      len : int) return int  -- /usr/local/include/notcurses/notcurses.h:1987
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline";

   function ncplane_vline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : int;
      c1 : Unsigned_64;
      c2 : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1991
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vline_interp";

   function ncplane_vline
     (n : access ncplane;
      c : access constant nccell;
      len : int) return int  -- /usr/local/include/notcurses/notcurses.h:1995
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
      ystop : int;
      xstop : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2028
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
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2037
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
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2047
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter";

   function ncplane_polyfill_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2063
   with Import => True,
        Convention => C,
        External_Name => "ncplane_polyfill_yx";

   function ncplane_gradient
     (n : access ncplane;
      egc : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64;
      ystop : int;
      xstop : int) return int  -- /usr/local/include/notcurses/notcurses.h:2084
   with Import => True,
        Convention => C,
        External_Name => "ncplane_gradient";

   function ncplane_highgradient
     (n : access ncplane;
      ul : Unsigned_32;
      ur : Unsigned_32;
      ll : Unsigned_32;
      lr : Unsigned_32;
      ystop : int;
      xstop : int) return int  -- /usr/local/include/notcurses/notcurses.h:2092
   with Import => True,
        Convention => C,
        External_Name => "ncplane_highgradient";

   function ncplane_gradient_sized
     (n : access ncplane;
      egc : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:2098
   with Import => True,
        Convention => C,
        External_Name => "ncplane_gradient_sized";

   function ncplane_highgradient_sized
     (n : access ncplane;
      ul : Unsigned_32;
      ur : Unsigned_32;
      ll : Unsigned_32;
      lr : Unsigned_32;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:2111
   with Import => True,
        Convention => C,
        External_Name => "ncplane_highgradient_sized";

   function ncplane_format
     (n : access ncplane;
      ystop : int;
      xstop : int;
      stylemask : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2116
   with Import => True,
        Convention => C,
        External_Name => "ncplane_format";

   function ncplane_stain
     (n : access ncplane;
      ystop : int;
      xstop : int;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:2120
   with Import => True,
        Convention => C,
        External_Name => "ncplane_stain";

   function ncplane_mergedown_simple (src : access ncplane; dst : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:2125
   with Import => True,
        Convention => C,
        External_Name => "ncplane_mergedown_simple";

   function ncplane_mergedown
     (src : access ncplane;
      dst : access ncplane;
      begsrcy : int;
      begsrcx : int;
      leny : int;
      lenx : int;
      dsty : int;
      dstx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2138
   with Import => True,
        Convention => C,
        External_Name => "ncplane_mergedown";

   procedure ncplane_erase (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2147
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase";

   function ncplane_erase_region
     (n : access ncplane;
      ystart : int;
      xstart : int;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:2154
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase_region";

   function nccell_fg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2160
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb";

   function nccell_bg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2166
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb";

   function nccell_fg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2172
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_alpha";

   function nccell_bg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2178
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_alpha";

   function nccell_fg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2184
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb8";

   function nccell_bg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2190
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb8";

   function nccell_set_fg_rgb8
     (cl : access nccell;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2197
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8";

   procedure nccell_set_fg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2203
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8_clipped";

   function nccell_set_fg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2209
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb";

   function nccell_set_fg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2216
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_palindex";

   function nccell_fg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2221
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex";

   function nccell_set_bg_rgb8
     (cl : access nccell;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2228
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8";

   procedure nccell_set_bg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2234
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8_clipped";

   function nccell_set_bg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2241
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb";

   function nccell_set_bg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2248
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_palindex";

   function nccell_bg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2253
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex";

   function nccell_fg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2259
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_default_p";

   function nccell_fg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2264
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex_p";

   function nccell_bg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2272
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_default_p";

   function nccell_bg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2277
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex_p";

   function ncplane_bchannel (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2283
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bchannel";

   function ncplane_fchannel (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2289
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fchannel";

   procedure ncplane_set_channels (n : access ncplane; channels : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:2293
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_channels";

   procedure ncplane_set_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2297
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_styles";

   procedure ncplane_on_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2300
   with Import => True,
        Convention => C,
        External_Name => "ncplane_on_styles";

   procedure ncplane_off_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2303
   with Import => True,
        Convention => C,
        External_Name => "ncplane_off_styles";

   function ncplane_fg_rgb (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2307
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb";

   function ncplane_bg_rgb (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2313
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb";

   function ncplane_fg_alpha (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2319
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_alpha";

   function ncplane_fg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2325
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_default_p";

   function ncplane_bg_alpha (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2331
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_alpha";

   function ncplane_bg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2337
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_default_p";

   function ncplane_fg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2343
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb8";

   function ncplane_bg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2349
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb8";

   function ncplane_set_fchannel (n : access ncplane; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2354
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fchannel";

   function ncplane_set_bchannel (n : access ncplane; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2355
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bchannel";

   function ncplane_set_fg_rgb8
     (n : access ncplane;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2363
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8";

   function ncplane_set_bg_rgb8
     (n : access ncplane;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2364
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8";

   procedure ncplane_set_bg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2367
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8_clipped";

   procedure ncplane_set_fg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2368
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8_clipped";

   function ncplane_set_fg_rgb (n : access ncplane; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2371
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb";

   function ncplane_set_bg_rgb (n : access ncplane; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2372
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb";

   procedure ncplane_set_fg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2375
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_default";

   procedure ncplane_set_bg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2376
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_default";

   function ncplane_set_fg_palindex (n : access ncplane; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2380
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_palindex";

   function ncplane_set_bg_palindex (n : access ncplane; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2381
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_palindex";

   function ncplane_set_fg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2384
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_alpha";

   function ncplane_set_bg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2385
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_alpha";

   type fadecb is access function
        (arg1 : access notcurses;
         arg2 : access ncplane;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2390

   function ncplane_fadeout
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2397
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout";

   function ncplane_fadein
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2403
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein";

   function ncfadectx_setup (n : access ncplane) return access ncfadectx  -- /usr/local/include/notcurses/notcurses.h:2408
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_setup";

   function ncfadectx_iterations (nctx : access constant ncfadectx) return int  -- /usr/local/include/notcurses/notcurses.h:2411
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_iterations";

   function ncplane_fadeout_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2415
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout_iteration";

   function ncplane_fadein_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2420
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein_iteration";

   function ncplane_pulse
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2428
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pulse";

   procedure ncfadectx_free (nctx : access ncfadectx)  -- /usr/local/include/notcurses/notcurses.h:2431
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_free";

   function nccells_load_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2438
   with Import => True,
        Convention => C,
        External_Name => "nccells_load_box";

   function nccells_rounded_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2463
   with Import => True,
        Convention => C,
        External_Name => "nccells_rounded_box";

   function nccells_ascii_box
     (n : access ncplane;
      attr : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2468
   with Import => True,
        Convention => C,
        External_Name => "nccells_ascii_box";

   function nccells_light_box
     (n : access ncplane;
      attr : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2474
   with Import => True,
        Convention => C,
        External_Name => "nccells_light_box";

   function nccells_heavy_box
     (n : access ncplane;
      attr : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2483
   with Import => True,
        Convention => C,
        External_Name => "nccells_heavy_box";

   function ncplane_rounded_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ystop : int;
      xstop : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2492
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rounded_box";

   function ncplane_perimeter_rounded
     (n : access ncplane;
      stylemask : Unsigned_32;
      channels : Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2508
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_rounded";

   function ncplane_rounded_box_sized
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2532
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rounded_box_sized";

   function nccells_double_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2540
   with Import => True,
        Convention => C,
        External_Name => "nccells_double_box";

   function ncplane_double_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ystop : int;
      xstop : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2545
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box";

   function ncplane_perimeter_double
     (n : access ncplane;
      stylemask : Unsigned_32;
      channels : Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2561
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_double";

   function ncplane_double_box_sized
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2585
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box_sized";

   function ncvisual_from_file (file : Interfaces.C.Strings.chars_ptr) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2595
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_file";

   function ncvisual_from_rgba
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2604
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgba";

   function ncvisual_from_rgb_packed
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int;
      alpha : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2610
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgb_packed";

   function ncvisual_from_rgb_loose
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int;
      alpha : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2617
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgb_loose";

   function ncvisual_from_bgra
     (bgra : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2625
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
      palette : access Unsigned_32) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2632
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_palidx";

   function ncvisual_from_plane
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2643
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_plane";

   type ncvisual_options is record
      n : access ncplane;  -- /usr/local/include/notcurses/notcurses.h:2662
      scaling : aliased ncscale_e;  -- /usr/local/include/notcurses/notcurses.h:2666
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2672
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2672
      begy : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2677
      begx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2677
      leny : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2678
      lenx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2678
      blitter : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:2682
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2683
      transcolor : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:2684
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2657

   function ncplane_as_rgba
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int;
      pxdimy : access int;
      pxdimx : access int) return access Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2693
   with Import => True,
        Convention => C,
        External_Name => "ncplane_as_rgba";

   function ncvisual_blitter_geom
     (nc : access constant notcurses;
      n : access constant ncvisual;
      vopts : access constant ncvisual_options;
      y : access int;
      x : access int;
      scaley : access int;
      scalex : access int;
      blitter : access ncblitter_e) return int  -- /usr/local/include/notcurses/notcurses.h:2705
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_blitter_geom";

   procedure ncvisual_destroy (ncv : access ncvisual)  -- /usr/local/include/notcurses/notcurses.h:2712
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_destroy";

   function ncvisual_decode (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:2716
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode";

   function ncvisual_decode_loop (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:2723
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode_loop";

   function ncvisual_rotate (n : access ncvisual; rads : double) return int  -- /usr/local/include/notcurses/notcurses.h:2728
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_rotate";

   function ncvisual_resize
     (n : access ncvisual;
      rows : int;
      cols : int) return int  -- /usr/local/include/notcurses/notcurses.h:2733
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_resize";

   function ncvisual_resize_noninterpolative
     (n : access ncvisual;
      rows : int;
      cols : int) return int  -- /usr/local/include/notcurses/notcurses.h:2738
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_resize_noninterpolative";

   function ncvisual_polyfill_yx
     (n : access ncvisual;
      y : int;
      x : int;
      rgba : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2742
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_polyfill_yx";

   function ncvisual_at_yx
     (n : access constant ncvisual;
      y : int;
      x : int;
      pixel : access Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2746
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_at_yx";

   function ncvisual_set_yx
     (n : access constant ncvisual;
      y : int;
      x : int;
      pixel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2750
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_set_yx";

   function ncvisual_render
     (nc : access notcurses;
      ncv : access ncvisual;
      vopts : access constant ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2759
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_render";

   function ncvisualplane_create
     (n : access ncplane;
      opts : access constant ncplane_options;
      ncv : access ncvisual;
      vopts : access ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2764
   with Import => True,
        Convention => C,
        External_Name => "ncvisualplane_create";

   function ncvisual_subtitle_plane (parent : access ncplane; ncv : access constant ncvisual) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2789
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_subtitle_plane";

   function ncvisual_media_defblitter (nc : access constant notcurses; scale : ncscale_e) return ncblitter_e  -- /usr/local/include/notcurses/notcurses.h:2801
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_media_defblitter";

   type ncstreamcb is access function
        (arg1 : access ncvisual;
         arg2 : access ncvisual_options;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2806

   function ncvisual_simple_streamer
     (ncv : access ncvisual;
      vopts : access ncvisual_options;
      tspec : access constant System.OS_Interface.timespec;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2812
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_simple_streamer";

   function ncvisual_stream
     (nc : access notcurses;
      ncv : access ncvisual;
      timescale : float;
      streamer : ncstreamcb;
      vopts : access constant ncvisual_options;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2824
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_stream";

   function ncblit_rgba
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:2835
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgba";

   function ncblit_bgrx
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:2839
   with Import => True,
        Convention => C,
        External_Name => "ncblit_bgrx";

   function ncblit_rgb_packed
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2843
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_packed";

   function ncblit_rgb_loose
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2848
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_loose";

   function ncpixel_a (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2863
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_a";

   function ncpixel_r (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2869
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_r";

   function ncpixel_g (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2875
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_g";

   function ncpixel_b (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2881
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_b";

   function ncpixel_set_a (pixel : access Unsigned_32; a : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2887
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_a";

   function ncpixel_set_r (pixel : access Unsigned_32; r : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2897
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_r";

   function ncpixel_set_g (pixel : access Unsigned_32; g : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2907
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_g";

   function ncpixel_set_b (pixel : access Unsigned_32; b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2917
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_b";

   function ncpixel
     (r : int;
      g : int;
      b : int) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2927
   with Import => True,
        Convention => C,
        External_Name => "ncpixel";

   function ncpixel_set_rgb8
     (pixel : access Unsigned_32;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2944
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_rgb8";

   type ncreel_options is record
      bordermask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2977
      borderchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2978
      tabletmask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2979
      tabletchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2980
      focusedchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2981
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2982
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2970

   function ncreel_create (n : access ncplane; popts : access constant ncreel_options) return access ncreel  -- /usr/local/include/notcurses/notcurses.h:2987
   with Import => True,
        Convention => C,
        External_Name => "ncreel_create";

   function ncreel_plane (nr : access ncreel) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2991
   with Import => True,
        Convention => C,
        External_Name => "ncreel_plane";

   type tabletcb is access function (arg1 : access nctablet; arg2 : Extensions.bool) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2998

   function ncreel_add
     (nr : access ncreel;
      after : access nctablet;
      before : access nctablet;
      cb : tabletcb;
      opaque : System.Address) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3007
   with Import => True,
        Convention => C,
        External_Name => "ncreel_add";

   function ncreel_tabletcount (nr : access constant ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:3013
   with Import => True,
        Convention => C,
        External_Name => "ncreel_tabletcount";

   function ncreel_del (nr : access ncreel; t : access nctablet) return int  -- /usr/local/include/notcurses/notcurses.h:3018
   with Import => True,
        Convention => C,
        External_Name => "ncreel_del";

   function ncreel_redraw (nr : access ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:3024
   with Import => True,
        Convention => C,
        External_Name => "ncreel_redraw";

   function ncreel_offer_input (nr : access ncreel; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3033
   with Import => True,
        Convention => C,
        External_Name => "ncreel_offer_input";

   function ncreel_focused (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3038
   with Import => True,
        Convention => C,
        External_Name => "ncreel_focused";

   function ncreel_next (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3042
   with Import => True,
        Convention => C,
        External_Name => "ncreel_next";

   function ncreel_prev (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:3046
   with Import => True,
        Convention => C,
        External_Name => "ncreel_prev";

   procedure ncreel_destroy (nr : access ncreel)  -- /usr/local/include/notcurses/notcurses.h:3050
   with Import => True,
        Convention => C,
        External_Name => "ncreel_destroy";

   function nctablet_userptr (t : access nctablet) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3053
   with Import => True,
        Convention => C,
        External_Name => "nctablet_userptr";

   function nctablet_plane (t : access nctablet) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3056
   with Import => True,
        Convention => C,
        External_Name => "nctablet_plane";

   function ncmetric
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int;
      mult : Unsigned_Max;
      uprefix : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3079
   with Import => True,
        Convention => C,
        External_Name => "ncmetric";

   function qprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3106
   with Import => True,
        Convention => C,
        External_Name => "qprefix";

   function iprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3112
   with Import => True,
        Convention => C,
        External_Name => "iprefix";

   function bprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3118
   with Import => True,
        Convention => C,
        External_Name => "bprefix";

   function notcurses_cursor_enable
     (nc : access notcurses;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:3126
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_enable";

   function notcurses_cursor_yx
     (nc : access notcurses;
      y : access int;
      x : access int) return int  -- /usr/local/include/notcurses/notcurses.h:3129
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_yx";

   function notcurses_cursor_disable (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:3133
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_disable";

   procedure ncplane_greyscale (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:3136
   with Import => True,
        Convention => C,
        External_Name => "ncplane_greyscale";

   type ncselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3156
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3157
      opcolumns : aliased Interfaces.C.size_t;  -- /usr/local/include/notcurses/notcurses.h:3158
      desccolumns : aliased Interfaces.C.size_t;  -- /usr/local/include/notcurses/notcurses.h:3159
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3155

   type ncselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3163
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3164
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3165
      items : access ncselector_item;  -- /usr/local/include/notcurses/notcurses.h:3166
      defidx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3169
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3171
      opchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3173
      descchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3174
      titlechannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3175
      footchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3176
      boxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3177
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3178
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3162

   function ncselector_create (n : access ncplane; opts : access constant ncselector_options) return access ncselector  -- /usr/local/include/notcurses/notcurses.h:3181
   with Import => True,
        Convention => C,
        External_Name => "ncselector_create";

   function ncselector_additem (n : access ncselector; item : access constant ncselector_item) return int  -- /usr/local/include/notcurses/notcurses.h:3186
   with Import => True,
        Convention => C,
        External_Name => "ncselector_additem";

   function ncselector_delitem (n : access ncselector; item : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3187
   with Import => True,
        Convention => C,
        External_Name => "ncselector_delitem";

   function ncselector_selected (n : access constant ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3190
   with Import => True,
        Convention => C,
        External_Name => "ncselector_selected";

   function ncselector_plane (n : access ncselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3193
   with Import => True,
        Convention => C,
        External_Name => "ncselector_plane";

   function ncselector_previtem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3197
   with Import => True,
        Convention => C,
        External_Name => "ncselector_previtem";

   function ncselector_nextitem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3198
   with Import => True,
        Convention => C,
        External_Name => "ncselector_nextitem";

   function ncselector_offer_input (n : access ncselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3207
   with Import => True,
        Convention => C,
        External_Name => "ncselector_offer_input";

   procedure ncselector_destroy (n : access ncselector; item : System.Address)  -- /usr/local/include/notcurses/notcurses.h:3212
   with Import => True,
        Convention => C,
        External_Name => "ncselector_destroy";

   type ncmselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3215
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3216
      selected : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:3217
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3214

   type ncmultiselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3242
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3243
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3244
      items : access ncmselector_item;  -- /usr/local/include/notcurses/notcurses.h:3245
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3247
      opchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3249
      descchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3250
      titlechannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3251
      footchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3252
      boxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3253
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3254
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3241

   function ncmultiselector_create (n : access ncplane; opts : access constant ncmultiselector_options) return access ncmultiselector  -- /usr/local/include/notcurses/notcurses.h:3257
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_create";

   function ncmultiselector_selected
     (n : access ncmultiselector;
      selected : access Extensions.bool;
      count : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3262
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_selected";

   function ncmultiselector_plane (n : access ncmultiselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3265
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_plane";

   function ncmultiselector_offer_input (n : access ncmultiselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3274
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_offer_input";

   procedure ncmultiselector_destroy (n : access ncmultiselector)  -- /usr/local/include/notcurses/notcurses.h:3278
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_destroy";

   type nctree_item;
   type nctree_item is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3289
      subs : access nctree_item;  -- /usr/local/include/notcurses/notcurses.h:3290
      subcount : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3291
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3288

   type nctree_options is record
      items : access constant nctree_item;  -- /usr/local/include/notcurses/notcurses.h:3295
      count : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3296
      nctreecb : access function
           (arg1 : access ncplane;
            arg2 : System.Address;
            arg3 : int) return int;  -- /usr/local/include/notcurses/notcurses.h:3297
      indentcols : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3298
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3299
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3294

   type nctree is null record;   -- incomplete struct

   function nctree_create (n : access ncplane; opts : access constant nctree_options) return access nctree  -- /usr/local/include/notcurses/notcurses.h:3304
   with Import => True,
        Convention => C,
        External_Name => "nctree_create";

   function nctree_plane (n : access nctree) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3308
   with Import => True,
        Convention => C,
        External_Name => "nctree_plane";

   function nctree_redraw (n : access nctree) return int  -- /usr/local/include/notcurses/notcurses.h:3314
   with Import => True,
        Convention => C,
        External_Name => "nctree_redraw";

   function nctree_offer_input (n : access nctree; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3323
   with Import => True,
        Convention => C,
        External_Name => "nctree_offer_input";

   function nctree_focused (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3328
   with Import => True,
        Convention => C,
        External_Name => "nctree_focused";

   function nctree_next (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3331
   with Import => True,
        Convention => C,
        External_Name => "nctree_next";

   function nctree_prev (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3334
   with Import => True,
        Convention => C,
        External_Name => "nctree_prev";

   function nctree_goto
     (n : access nctree;
      spec : access unsigned;
      failspec : access int) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3341
   with Import => True,
        Convention => C,
        External_Name => "nctree_goto";

   procedure nctree_destroy (n : access nctree)  -- /usr/local/include/notcurses/notcurses.h:3344
   with Import => True,
        Convention => C,
        External_Name => "nctree_destroy";

   type ncmenu_item is record
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3352
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:3353
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3351

   type ncmenu_section is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3357
      itemcount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3358
      items : access ncmenu_item;  -- /usr/local/include/notcurses/notcurses.h:3359
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:3360
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3356

   type ncmenu_options is record
      sections : access ncmenu_section;  -- /usr/local/include/notcurses/notcurses.h:3367
      sectioncount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3368
      headerchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3369
      sectionchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3370
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3371
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3366

   type ncmenu is null record;   -- incomplete struct

   function ncmenu_create (n : access ncplane; opts : access constant ncmenu_options) return access ncmenu  -- /usr/local/include/notcurses/notcurses.h:3375
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_create";

   function ncmenu_unroll (n : access ncmenu; sectionidx : int) return int  -- /usr/local/include/notcurses/notcurses.h:3380
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_unroll";

   function ncmenu_rollup (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3383
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_rollup";

   function ncmenu_nextsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3387
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextsection";

   function ncmenu_prevsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3388
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_prevsection";

   function ncmenu_nextitem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3392
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextitem";

   function ncmenu_previtem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3393
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_previtem";

   function ncmenu_item_set_status
     (n : access ncmenu;
      section : Interfaces.C.Strings.chars_ptr;
      item : Interfaces.C.Strings.chars_ptr;
      enabled : Extensions.bool) return int  -- /usr/local/include/notcurses/notcurses.h:3396
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_item_set_status";

   function ncmenu_selected (n : access constant ncmenu; ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3402
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_selected";

   function ncmenu_mouse_selected
     (n : access constant ncmenu;
      click : access constant ncinput;
      ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3408
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_mouse_selected";

   function ncmenu_plane (n : access ncmenu) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3412
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_plane";

   function ncmenu_offer_input (n : access ncmenu; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3423
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_offer_input";

   function ncmenu_destroy (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3427
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_destroy";

   type ncprogbar_options is record
      ulchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3442
      urchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3443
      blchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3444
      brchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3445
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3446
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3441

   function ncprogbar_create (n : access ncplane; opts : access constant ncprogbar_options) return access ncprogbar  -- /usr/local/include/notcurses/notcurses.h:3451
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_create";

   function ncprogbar_plane (n : access ncprogbar) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3455
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_plane";

   function ncprogbar_set_progress (n : access ncprogbar; p : double) return int  -- /usr/local/include/notcurses/notcurses.h:3459
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_set_progress";

   function ncprogbar_progress (n : access constant ncprogbar) return double  -- /usr/local/include/notcurses/notcurses.h:3463
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_progress";

   procedure ncprogbar_destroy (n : access ncprogbar)  -- /usr/local/include/notcurses/notcurses.h:3467
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_destroy";

   type nctabbed_options is record
      selchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3476
      hdrchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3477
      sepchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3478
      separator : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3479
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3480
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3475

   type tabcb is access procedure
        (arg1 : access nctab;
         arg2 : access ncplane;
         arg3 : System.Address)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3486

   function nctabbed_create (n : access ncplane; opts : access constant nctabbed_options) return access nctabbed  -- /usr/local/include/notcurses/notcurses.h:3493
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_create";

   procedure nctabbed_destroy (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3499
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_destroy";

   procedure nctabbed_redraw (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3504
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_redraw";

   procedure nctabbed_ensure_selected_header_visible (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3510
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_ensure_selected_header_visible";

   function nctabbed_selected (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3514
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selected";

   function nctabbed_leftmost (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3518
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_leftmost";

   function nctabbed_tabcount (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:3522
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_tabcount";

   function nctabbed_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3526
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_plane";

   function nctabbed_content_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3530
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_content_plane";

   function nctab_cb (t : access nctab) return tabcb  -- /usr/local/include/notcurses/notcurses.h:3534
   with Import => True,
        Convention => C,
        External_Name => "nctab_cb";

   function nctab_name (t : access nctab) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3538
   with Import => True,
        Convention => C,
        External_Name => "nctab_name";

   function nctab_name_width (t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3542
   with Import => True,
        Convention => C,
        External_Name => "nctab_name_width";

   function nctab_userptr (t : access nctab) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3546
   with Import => True,
        Convention => C,
        External_Name => "nctab_userptr";

   function nctab_next (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3550
   with Import => True,
        Convention => C,
        External_Name => "nctab_next";

   function nctab_prev (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3554
   with Import => True,
        Convention => C,
        External_Name => "nctab_prev";

   function nctabbed_add
     (nt : access nctabbed;
      after : access nctab;
      before : access nctab;
      tcb : tabcb;
      name : Interfaces.C.Strings.chars_ptr;
      opaque : System.Address) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3566
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_add";

   function nctabbed_del (nt : access nctabbed; t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3576
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_del";

   function nctab_move
     (nt : access nctabbed;
      t : access nctab;
      after : access nctab;
      before : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3582
   with Import => True,
        Convention => C,
        External_Name => "nctab_move";

   procedure nctab_move_right (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:3587
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_right";

   procedure nctab_move_left (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:3591
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_left";

   procedure nctabbed_rotate (nt : access nctabbed; amt : int)  -- /usr/local/include/notcurses/notcurses.h:3597
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_rotate";

   function nctabbed_next (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3602
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_next";

   function nctabbed_prev (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3607
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_prev";

   function nctabbed_select (nt : access nctabbed; t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3611
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_select";

   procedure nctabbed_channels
     (nt : access nctabbed;
      hdrchan : access Unsigned_64;
      selchan : access Unsigned_64;
      sepchan : access Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3616
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_channels";

   function nctabbed_hdrchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3621
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_hdrchan";

   function nctabbed_selchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3628
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selchan";

   function nctabbed_sepchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3635
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_sepchan";

   function nctabbed_separator (nt : access nctabbed) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3644
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator";

   function nctabbed_separator_width (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:3648
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator_width";

   procedure nctabbed_set_hdrchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3652
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_hdrchan";

   procedure nctabbed_set_selchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3656
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_selchan";

   procedure nctabbed_set_sepchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3660
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_sepchan";

   function nctab_set_cb (t : access nctab; newcb : tabcb) return tabcb  -- /usr/local/include/notcurses/notcurses.h:3664
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_cb";

   function nctab_set_name (t : access nctab; newname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3668
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_name";

   function nctab_set_userptr (t : access nctab; newopaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3672
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_userptr";

   function nctabbed_set_separator (nt : access nctabbed; separator : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3677
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_separator";

   type ncplot_options is record
      maxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3726
      minchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3727
      legendstyle : aliased Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:3729
      gridtype : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:3732
      rangex : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3737
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3738
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3739
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3723

   function ncuplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : Unsigned_64;
      maxy : Unsigned_64) return access ncuplot  -- /usr/local/include/notcurses/notcurses.h:3746
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_create";

   function ncdplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : double;
      maxy : double) return access ncdplot  -- /usr/local/include/notcurses/notcurses.h:3750
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_create";

   function ncuplot_plane (n : access ncuplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3755
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_plane";

   function ncdplot_plane (n : access ncdplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3758
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_plane";

   function ncuplot_add_sample
     (n : access ncuplot;
      x : Unsigned_64;
      y : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3765
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_add_sample";

   function ncdplot_add_sample
     (n : access ncdplot;
      x : Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:3767
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_add_sample";

   function ncuplot_set_sample
     (n : access ncuplot;
      x : Unsigned_64;
      y : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3769
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_set_sample";

   function ncdplot_set_sample
     (n : access ncdplot;
      x : Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:3771
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_set_sample";

   function ncuplot_sample
     (n : access constant ncuplot;
      x : Unsigned_64;
      y : access Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3774
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_sample";

   function ncdplot_sample
     (n : access constant ncdplot;
      x : Unsigned_64;
      y : access double) return int  -- /usr/local/include/notcurses/notcurses.h:3776
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_sample";

   procedure ncuplot_destroy (n : access ncuplot)  -- /usr/local/include/notcurses/notcurses.h:3779
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_destroy";

   procedure ncdplot_destroy (n : access ncdplot)  -- /usr/local/include/notcurses/notcurses.h:3780
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_destroy";

   type ncfdplane_callback is access function
        (arg1 : access ncfdplane;
         arg2 : System.Address;
         arg3 : Interfaces.C.size_t;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3782

   type ncfdplane_done_cb is access function
        (arg1 : access ncfdplane;
         arg2 : int;
         arg3 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3783

   type ncfdplane_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3791
      follow : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:3792
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3793
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3790

   function ncfdplane_create
     (n : access ncplane;
      opts : access constant ncfdplane_options;
      fd : int;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncfdplane  -- /usr/local/include/notcurses/notcurses.h:3798
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_create";

   function ncfdplane_plane (n : access ncfdplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3802
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_plane";

   function ncfdplane_destroy (n : access ncfdplane) return int  -- /usr/local/include/notcurses/notcurses.h:3805
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_destroy";

   type ncsubproc_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3808
      restart_period : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3809
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3810
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3807

   function ncsubproc_createv
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3814
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createv";

   function ncsubproc_createvp
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3819
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
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3824
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createvpe";

   function ncsubproc_plane (n : access ncsubproc) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3829
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_plane";

   function ncsubproc_destroy (n : access ncsubproc) return int  -- /usr/local/include/notcurses/notcurses.h:3832
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_destroy";

   function ncplane_qrcode
     (n : access ncplane;
      ymax : access int;
      xmax : access int;
      data : System.Address;
      len : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:3839
   with Import => True,
        Convention => C,
        External_Name => "ncplane_qrcode";

   type ncreader_options is record
      tchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3854
      tattrword : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3855
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3856
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3853

   function ncreader_create (n : access ncplane; opts : access constant ncreader_options) return access ncreader  -- /usr/local/include/notcurses/notcurses.h:3862
   with Import => True,
        Convention => C,
        External_Name => "ncreader_create";

   function ncreader_clear (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3866
   with Import => True,
        Convention => C,
        External_Name => "ncreader_clear";

   function ncreader_plane (n : access ncreader) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3869
   with Import => True,
        Convention => C,
        External_Name => "ncreader_plane";

   function ncreader_offer_input (n : access ncreader; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3875
   with Import => True,
        Convention => C,
        External_Name => "ncreader_offer_input";

   function ncreader_move_left (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3880
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_left";

   function ncreader_move_right (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3882
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_right";

   function ncreader_move_up (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3884
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_up";

   function ncreader_move_down (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3886
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_down";

   function ncreader_write_egc (n : access ncreader; egc : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3891
   with Import => True,
        Convention => C,
        External_Name => "ncreader_write_egc";

   function ncreader_contents (n : access constant ncreader) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3895
   with Import => True,
        Convention => C,
        External_Name => "ncreader_contents";

   procedure ncreader_destroy (n : access ncreader; contents : System.Address)  -- /usr/local/include/notcurses/notcurses.h:3900
   with Import => True,
        Convention => C,
        External_Name => "ncreader_destroy";

   procedure notcurses_debug (nc : access constant notcurses; debugfp : Interfaces.C_Streams.FILEs)  -- /usr/local/include/notcurses/notcurses.h:3905
   with Import => True,
        Convention => C,
        External_Name => "notcurses_debug";

   function ncplane_align
     (n : access constant ncplane;
      align : ncalign_e;
      c : int) return int  -- /usr/local/include/notcurses/notcurses.h:3911
   with Import => True,
        Convention => C,
        External_Name => "ncplane_align";

   procedure cell_init (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3916
   with Import => True,
        Convention => C,
        External_Name => "cell_init";

   function cell_load
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3920
   with Import => True,
        Convention => C,
        External_Name => "cell_load";

   function cell_prime
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3924
   with Import => True,
        Convention => C,
        External_Name => "cell_prime";

   function cell_duplicate
     (n : access ncplane;
      targ : access nccell;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3929
   with Import => True,
        Convention => C,
        External_Name => "cell_duplicate";

   procedure cell_release (n : access ncplane; c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3931
   with Import => True,
        Convention => C,
        External_Name => "cell_release";

   function ncvisual_default_blitter (utf8 : Extensions.bool; scale : ncscale_e) return ncblitter_e  -- /usr/local/include/notcurses/notcurses.h:3936
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_default_blitter";

   procedure cell_set_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3951
   with Import => True,
        Convention => C,
        External_Name => "cell_set_styles";

   function cell_styles (c : access constant nccell) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3957
   with Import => True,
        Convention => C,
        External_Name => "cell_styles";

   procedure cell_on_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3962
   with Import => True,
        Convention => C,
        External_Name => "cell_on_styles";

   procedure cell_off_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3967
   with Import => True,
        Convention => C,
        External_Name => "cell_off_styles";

   procedure cell_set_fg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3972
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_default";

   procedure cell_set_bg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3977
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_default";

   function cell_set_fg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3982
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_alpha";

   function cell_set_bg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3987
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_alpha";

   function cell_double_wide_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3992
   with Import => True,
        Convention => C,
        External_Name => "cell_double_wide_p";

   function cell_wide_right_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3997
   with Import => True,
        Convention => C,
        External_Name => "cell_wide_right_p";

   function cell_wide_left_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4002
   with Import => True,
        Convention => C,
        External_Name => "cell_wide_left_p";

   function cell_extended_gcluster (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4007
   with Import => True,
        Convention => C,
        External_Name => "cell_extended_gcluster";

   function cell_strdup (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4010
   with Import => True,
        Convention => C,
        External_Name => "cell_strdup";

   function cell_extract
     (n : access constant ncplane;
      c : access constant nccell;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4015
   with Import => True,
        Convention => C,
        External_Name => "cell_extract";

   function cellcmp
     (n1 : access constant ncplane;
      c1 : access constant nccell;
      n2 : access constant ncplane;
      c2 : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4021
   with Import => True,
        Convention => C,
        External_Name => "cellcmp";

   function cell_load_char
     (n : access ncplane;
      c : access nccell;
      ch : char) return int  -- /usr/local/include/notcurses/notcurses.h:4027
   with Import => True,
        Convention => C,
        External_Name => "cell_load_char";

   function cell_load_egc32
     (n : access ncplane;
      c : access nccell;
      egc : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:4032
   with Import => True,
        Convention => C,
        External_Name => "cell_load_egc32";

   function ncplane_new
     (n : access ncplane;
      rows : int;
      cols : int;
      y : int;
      x : int;
      opaque : System.Address;
      name : Interfaces.C.Strings.chars_ptr) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4038
   with Import => True,
        Convention => C,
        External_Name => "ncplane_new";

   function cell_fg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4042
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_rgb";

   function cell_bg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4047
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_rgb";

   function cell_fg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4052
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_alpha";

   function cell_bg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4057
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_alpha";

   function cell_fg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4062
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_rgb8";

   function cell_bg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4068
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_rgb8";

   function cell_set_fg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4073
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb8";

   procedure cell_set_fg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4078
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb8_clipped";

   function cell_set_fg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:4083
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb";

   function cell_set_fg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4088
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_palindex";

   function cell_fg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4093
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_palindex";

   function cell_set_bg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4098
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb8";

   procedure cell_set_bg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4103
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb8_clipped";

   function cell_set_bg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:4108
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb";

   function cell_set_bg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4113
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_palindex";

   function cell_bg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4118
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_palindex";

   function cell_fg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4123
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_default_p";

   function cell_fg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4128
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_palindex_p";

   function cell_bg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4133
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_default_p";

   function cell_bg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4138
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_palindex_p";

   procedure ncplane_styles_set (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:4142
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles_set";

   procedure ncplane_styles_on (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:4144
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles_on";

   procedure ncplane_styles_off (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:4146
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles_off";

   function cells_rounded_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:4150
   with Import => True,
        Convention => C,
        External_Name => "cells_rounded_box";

   function cells_double_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ul : access nccell;
      ur : access nccell;
      ll : access nccell;
      lr : access nccell;
      hl : access nccell;
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:4155
   with Import => True,
        Convention => C,
        External_Name => "cells_double_box";

   function ncplane_rgba
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return access Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4162
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rgba";

   function ncvisual_geom
     (nc : access constant notcurses;
      n : access constant ncvisual;
      vopts : access constant ncvisual_options;
      y : access int;
      x : access int;
      scaley : access int;
      scalex : access int) return int  -- /usr/local/include/notcurses/notcurses.h:4168
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_geom";

   function nctablet_ncplane (t : access nctablet) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:4175
   with Import => True,
        Convention => C,
        External_Name => "nctablet_ncplane";

   function palette256_new (nc : access notcurses) return access ncpalette  -- /usr/local/include/notcurses/notcurses.h:4178
   with Import => True,
        Convention => C,
        External_Name => "palette256_new";

   function palette256_use (nc : access notcurses; p : access constant ncpalette) return int  -- /usr/local/include/notcurses/notcurses.h:4181
   with Import => True,
        Convention => C,
        External_Name => "palette256_use";

   function palette256_set_rgb8
     (p : access ncpalette;
      idx : int;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4185
   with Import => True,
        Convention => C,
        External_Name => "palette256_set_rgb8";

   function palette256_set
     (p : access ncpalette;
      idx : int;
      rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4190
   with Import => True,
        Convention => C,
        External_Name => "palette256_set";

   function palette256_get_rgb8
     (p : access constant ncpalette;
      idx : int;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4195
   with Import => True,
        Convention => C,
        External_Name => "palette256_get_rgb8";

   procedure palette256_free (p : access ncpalette)  -- /usr/local/include/notcurses/notcurses.h:4199
   with Import => True,
        Convention => C,
        External_Name => "palette256_free";

   function channel_r (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4202
   with Import => True,
        Convention => C,
        External_Name => "channel_r";

   function channel_g (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4208
   with Import => True,
        Convention => C,
        External_Name => "channel_g";

   function channel_b (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4214
   with Import => True,
        Convention => C,
        External_Name => "channel_b";

   function channel_rgb8
     (channel : Unsigned_32;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4220
   with Import => True,
        Convention => C,
        External_Name => "channel_rgb8";

   function channel_set_rgb8
     (channel : access Unsigned_32;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4228
   with Import => True,
        Convention => C,
        External_Name => "channel_set_rgb8";

   procedure channel_set_rgb8_clipped
     (channel : access unsigned;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4236
   with Import => True,
        Convention => C,
        External_Name => "channel_set_rgb8_clipped";

   function channel_set (channel : access unsigned; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4242
   with Import => True,
        Convention => C,
        External_Name => "channel_set";

   function channel_alpha (channel : unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4248
   with Import => True,
        Convention => C,
        External_Name => "channel_alpha";

   function channel_palindex (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4253
   with Import => True,
        Convention => C,
        External_Name => "channel_palindex";

   function channel_set_alpha (channel : access unsigned; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4259
   with Import => True,
        Convention => C,
        External_Name => "channel_set_alpha";

   function channel_set_palindex (channel : access Unsigned_32; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4264
   with Import => True,
        Convention => C,
        External_Name => "channel_set_palindex";

   function channel_default_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4269
   with Import => True,
        Convention => C,
        External_Name => "channel_default_p";

   function channel_palindex_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4274
   with Import => True,
        Convention => C,
        External_Name => "channel_palindex_p";

   function channel_set_default (channel : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4279
   with Import => True,
        Convention => C,
        External_Name => "channel_set_default";

   function channels_bchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4284
   with Import => True,
        Convention => C,
        External_Name => "channels_bchannel";

   function channels_fchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4289
   with Import => True,
        Convention => C,
        External_Name => "channels_fchannel";

   function channels_set_bchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4294
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bchannel";

   function channels_set_fchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4299
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fchannel";

   function channels_combine (fchan : Unsigned_32; bchan : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4304
   with Import => True,
        Convention => C,
        External_Name => "channels_combine";

   function channels_fg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4309
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_palindex";

   function channels_bg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4314
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_palindex";

   function channels_fg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4319
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_rgb";

   function channels_bg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4324
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_rgb";

   function channels_fg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4329
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_alpha";

   function channels_bg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4334
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_alpha";

   function channels_fg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4339
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_rgb8";

   function channels_bg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4344
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_rgb8";

   function channels_set_fg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4349
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb8";

   procedure channels_set_fg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4354
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb8_clipped";

   function channels_set_fg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4359
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_alpha";

   function channels_set_fg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4364
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_palindex";

   function channels_set_fg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4369
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb";

   function channels_set_bg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4374
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb8";

   procedure channels_set_bg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4379
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb8_clipped";

   function channels_set_bg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4384
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_alpha";

   function channels_set_bg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4389
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_palindex";

   function channels_set_bg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4394
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb";

   function channels_fg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4399
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_default_p";

   function channels_fg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4404
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_palindex_p";

   function channels_bg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4409
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_default_p";

   function channels_bg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4414
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_palindex_p";

   function channels_set_fg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4419
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_default";

   function channels_set_bg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4424
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_default";

   function ncvisual_inflate (n : access ncvisual; scale : int) return int  -- /usr/local/include/notcurses/notcurses.h:4431
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_inflate";

   function notcurses_render_to_buffer
     (nc : access notcurses;
      buf : System.Address;
      buflen : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:4434
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render_to_buffer";

   function notcurses_render_to_file (nc : access notcurses; fp : Interfaces.C_Streams.FILEs) return int  -- /usr/local/include/notcurses/notcurses.h:4437
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render_to_file";

   subtype cell is nccell;  -- /usr/local/include/notcurses/notcurses.h:4440

   procedure notcurses_debug_caps (nc : access constant notcurses; debugfp : Interfaces.C_Streams.FILEs)  -- /usr/local/include/notcurses/notcurses.h:4442
   with Import => True,
        Convention => C,
        External_Name => "notcurses_debug_caps";

   function notcurses_getc
     (n : access notcurses;
      ts : access constant System.OS_Interface.timespec;
      unused : System.Address;
      ni : access ncinput) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4447
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc";

   function nccell_width (n : access constant ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:4451
   with Import => True,
        Convention => C,
        External_Name => "nccell_width";

   function ncvisual_subtitle (ncv : access constant ncvisual) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:4453
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_subtitle";

end Notcurses_Thin;
