--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces;   use Interfaces;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;
with System.OS_Interface;

package Notcurses_Thin is

   type File_Pointer is new System.Address;
   Null_File : constant File_Pointer := File_Pointer (System.Null_Address);

   type Unsigned_Max is mod 2 ** Long_Long_Integer'Size;

   --  arg-macro: function htole (x)
   --    return __bswap_32(htonl(x));
   --  unsupported macro: RESTRICT restrict
   --  unsupported macro: NCALIGN_TOP NCALIGN_LEFT
   --  unsupported macro: NCALIGN_BOTTOM NCALIGN_RIGHT
   CELL_ALPHA_HIGHCONTRAST : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:114
   CELL_ALPHA_TRANSPARENT : constant := 16#20000000#;  --  /usr/local/include/notcurses/notcurses.h:115
   CELL_ALPHA_BLEND : constant := 16#10000000#;  --  /usr/local/include/notcurses/notcurses.h:116
   CELL_ALPHA_OPAQUE : constant := 16#00000000#;  --  /usr/local/include/notcurses/notcurses.h:117

   CELL_BGDEFAULT_MASK : constant := 16#0000000040000000#;  --  /usr/local/include/notcurses/notcurses.h:120
   --  unsupported macro: CELL_FGDEFAULT_MASK (CELL_BGDEFAULT_MASK << 32u)

   CELL_BG_RGB_MASK : constant := 16#0000000000ffffff#;  --  /usr/local/include/notcurses/notcurses.h:124
   --  unsupported macro: CELL_FG_RGB_MASK (CELL_BG_RGB_MASK << 32u)

   CELL_BG_PALETTE : constant := 16#0000000008000000#;  --  /usr/local/include/notcurses/notcurses.h:129
   NCPALETTESIZE : constant := 256;  --  /usr/local/include/notcurses/notcurses.h:130
   --  unsupported macro: CELL_FG_PALETTE (CELL_BG_PALETTE << 32u)

   CELL_BG_ALPHA_MASK : constant := 16#30000000#;  --  /usr/local/include/notcurses/notcurses.h:135
   --  unsupported macro: CELL_FG_ALPHA_MASK (CELL_BG_ALPHA_MASK << 32u)
   --  arg-macro: function CHANNELS_RGB_INITIALIZER (fr, fg, fb, br, bg, bb)
   --    return ((((uint64_t)(fr) << 16) + ((uint64_t)(fg) << 8) + (uint64_t)(fb)) << 32) + (((br) << 16) + ((bg) << 8) + (bb)) + CELL_BGDEFAULT_MASK + CELL_FGDEFAULT_MASK;
   --  arg-macro: function CHANNEL_RGB_INITIALIZER (r, g, b)
   --    return ((uint32_t)r << 16) + ((uint32_t)g << 8) + (b) + CELL_BGDEFAULT_MASK;
   --  unsupported macro: CELL_TRIVIAL_INITIALIZER { .gcluster = 0, .gcluster_backstop = 0, .width = 0, .stylemask = 0, .channels = 0, }
   --  arg-macro: procedure CELL_CHAR_INITIALIZER (c)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)wcwidth(c), .stylemask := 0, .channels := 0, }
   --  arg-macro: procedure CELL_INITIALIZER (c, s, chan)
   --    { .gcluster := (htole(c)), .gcluster_backstop := 0, .width := (uint8_t)wcwidth(c), .stylemask := (s), .channels := (chan), }

   NCSTYLE_MASK : constant := 16#03ff#;  --  /usr/local/include/notcurses/notcurses.h:668
   NCSTYLE_STANDOUT : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:669
   NCSTYLE_UNDERLINE : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:670
   NCSTYLE_REVERSE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:671
   NCSTYLE_BLINK : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:672
   NCSTYLE_DIM : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:673
   NCSTYLE_BOLD : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:674
   NCSTYLE_INVIS : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:675
   NCSTYLE_PROTECT : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:676
   NCSTYLE_ITALIC : constant := 16#0100#;  --  /usr/local/include/notcurses/notcurses.h:677
   NCSTYLE_STRUCK : constant := 16#0200#;  --  /usr/local/include/notcurses/notcurses.h:678
   NCSTYLE_NONE : constant := 0;  --  /usr/local/include/notcurses/notcurses.h:679

   NCOPTION_INHIBIT_SETLOCALE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:841

   NCOPTION_NO_CLEAR_BITMAPS : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:847

   NCOPTION_NO_WINCH_SIGHANDLER : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:851

   NCOPTION_NO_QUIT_SIGHANDLERS : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:856

   NCOPTION_SUPPRESS_BANNERS : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:862

   NCOPTION_NO_ALTERNATE_SCREEN : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:866

   NCOPTION_NO_FONT_CHANGES : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:872

   NCPLANE_OPTION_HORALIGNED : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:1144

   NCPLANE_OPTION_VERALIGNED : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:1146

   NCPLANE_OPTION_MARGINALIZED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:1153

   WCHAR_MAX_UTF8BYTES : constant := 6;  --  /usr/local/include/notcurses/notcurses.h:1593

   NCBOXMASK_TOP : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:1835
   NCBOXMASK_RIGHT : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:1836
   NCBOXMASK_BOTTOM : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:1837
   NCBOXMASK_LEFT : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:1838
   NCBOXGRAD_TOP : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:1839
   NCBOXGRAD_RIGHT : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:1840
   NCBOXGRAD_BOTTOM : constant := 16#0040#;  --  /usr/local/include/notcurses/notcurses.h:1841
   NCBOXGRAD_LEFT : constant := 16#0080#;  --  /usr/local/include/notcurses/notcurses.h:1842
   NCBOXCORNER_MASK : constant := 16#0300#;  --  /usr/local/include/notcurses/notcurses.h:1843
   NCBOXCORNER_SHIFT : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:1844

   NCVISUAL_OPTION_NODEGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:2432
   NCVISUAL_OPTION_BLEND : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2433
   NCVISUAL_OPTION_HORALIGNED : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:2434
   NCVISUAL_OPTION_VERALIGNED : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:2435
   NCVISUAL_OPTION_ADDALPHA : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:2436
   NCVISUAL_OPTION_CHILDPLANE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:2437

   NCREEL_OPTION_INFINITESCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:2745

   NCREEL_OPTION_CIRCULAR : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:2749

   PREFIXCOLUMNS : constant := 7;  --  /usr/local/include/notcurses/notcurses.h:2872
   IPREFIXCOLUMNS : constant := 8;  --  /usr/local/include/notcurses/notcurses.h:2873
   BPREFIXCOLUMNS : constant := 9;  --  /usr/local/include/notcurses/notcurses.h:2874
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

   NCMENU_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3183
   NCMENU_OPTION_HIDING : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3184

   NCPROGBAR_OPTION_RETROGRADE : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3261

   NCTABBED_OPTION_BOTTOM : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3295

   NCPLOT_OPTION_LABELTICKSD : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3538
   NCPLOT_OPTION_EXPONENTIALD : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3539
   NCPLOT_OPTION_VERTICALI : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:3540
   NCPLOT_OPTION_NODEGRADE : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:3541
   NCPLOT_OPTION_DETECTMAXONLY : constant := 16#0010#;  --  /usr/local/include/notcurses/notcurses.h:3542
   NCPLOT_OPTION_PRINTSAMPLE : constant := 16#0020#;  --  /usr/local/include/notcurses/notcurses.h:3543

   NCREADER_OPTION_HORSCROLL : constant := 16#0001#;  --  /usr/local/include/notcurses/notcurses.h:3666

   NCREADER_OPTION_VERSCROLL : constant := 16#0002#;  --  /usr/local/include/notcurses/notcurses.h:3668

   NCREADER_OPTION_NOCMDKEYS : constant := 16#0004#;  --  /usr/local/include/notcurses/notcurses.h:3670

   NCREADER_OPTION_CURSOR : constant := 16#0008#;  --  /usr/local/include/notcurses/notcurses.h:3673

   function notcurses_version return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:39
   with Import => True,
        Convention => C,
        External_Name => "notcurses_version";

   procedure notcurses_version_components
     (major : access int;
      minor : access int;
      patch : access int;
      tweak : access int)  -- /usr/local/include/notcurses/notcurses.h:42
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
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:74

   type ncalign_e is
     (NCALIGN_UNALIGNED,
      NCALIGN_LEFT,
      NCALIGN_CENTER,
      NCALIGN_RIGHT)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:82

   type ncscale_e is
     (NCSCALE_NONE,
      NCSCALE_SCALE,
      NCSCALE_STRETCH,
      NCSCALE_NONE_HIRES,
      NCSCALE_SCALE_HIRES)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:99

   function ncstrwidth (mbs : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:103
   with Import => True,
        Convention => C,
        External_Name => "ncstrwidth";

   function notcurses_ucs32_to_utf8
     (ucs32 : access Wide_Wide_Character;
      ucs32count : unsigned;
      resultbuf : access unsigned_char;
      buflen : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:110
   with Import => True,
        Convention => C,
        External_Name => "notcurses_ucs32_to_utf8";

   function ncchannel_r (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:153
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_r";

   function ncchannel_g (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:159
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_g";

   function ncchannel_b (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:165
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_b";

   function ncchannel_alpha (channel : unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:171
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_alpha";

   function ncchannel_rgb8
     (channel : Unsigned_32;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:177
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_rgb8";

   function ncchannel_set_rgb8
     (channel : access Unsigned_32;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:188
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8";

   procedure ncchannel_set_rgb8_clipped
     (channel : access unsigned;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:204
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_rgb8_clipped";

   function ncchannel_set (channel : access unsigned; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:229
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set";

   function ncchannel_palindex (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:238
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex";

   function ncchannel_set_alpha (channel : access unsigned; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:244
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_alpha";

   function ncchannel_set_palindex (channel : access Unsigned_32; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:256
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_palindex";

   function ncchannel_default_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:270
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_default_p";

   function ncchannel_palindex_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:276
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_palindex_p";

   function ncchannel_set_default (channel : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:282
   with Import => True,
        Convention => C,
        External_Name => "ncchannel_set_default";

   function ncchannels_bchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:288
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bchannel";

   function ncchannels_fchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:294
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fchannel";

   function ncchannels_set_bchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:300
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bchannel";

   function ncchannels_set_fchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:306
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fchannel";

   function ncchannels_combine (fchan : Unsigned_32; bchan : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:311
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_combine";

   function ncchannels_fg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:319
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex";

   function ncchannels_bg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:324
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex";

   function ncchannels_fg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:330
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb";

   function ncchannels_bg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:336
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb";

   function ncchannels_fg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:342
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_alpha";

   function ncchannels_bg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:348
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_alpha";

   function ncchannels_fg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:354
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_rgb8";

   function ncchannels_bg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:360
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_rgb8";

   function ncchannels_set_fg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:367
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8";

   procedure ncchannels_set_fg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:378
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb8_clipped";

   function ncchannels_set_fg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:386
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_alpha";

   function ncchannels_set_fg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:396
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_palindex";

   function ncchannels_set_fg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:407
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_rgb";

   function ncchannels_set_bg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:419
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8";

   procedure ncchannels_set_bg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:430
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb8_clipped";

   function ncchannels_set_bg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:438
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_alpha";

   function ncchannels_set_bg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:453
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_palindex";

   function ncchannels_set_bg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:464
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_rgb";

   function ncchannels_fg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:475
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_default_p";

   function ncchannels_fg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:481
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_fg_palindex_p";

   function ncchannels_bg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:489
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_default_p";

   function ncchannels_bg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:495
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_bg_palindex_p";

   function ncchannels_set_fg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:501
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_fg_default";

   function ncchannels_set_bg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:510
   with Import => True,
        Convention => C,
        External_Name => "ncchannels_set_bg_default";

   type nccell is record
      gcluster : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:601
      gcluster_backstop : aliased Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:602
      width : aliased Unsigned_8;  -- /usr/local/include/notcurses/notcurses.h:610
      stylemask : aliased Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:611
      channels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:631
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:567

   procedure nccell_init (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:642
   with Import => True,
        Convention => C,
        External_Name => "nccell_init";

   function nccell_load
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:649
   with Import => True,
        Convention => C,
        External_Name => "nccell_load";

   function nccell_prime
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:653
   with Import => True,
        Convention => C,
        External_Name => "nccell_prime";

   function nccell_duplicate
     (n : access ncplane;
      targ : access nccell;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:663
   with Import => True,
        Convention => C,
        External_Name => "nccell_duplicate";

   procedure nccell_release (n : access ncplane; c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:666
   with Import => True,
        Convention => C,
        External_Name => "nccell_release";

   procedure nccell_set_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:684
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_styles";

   function nccell_styles (c : access constant nccell) return unsigned  -- /usr/local/include/notcurses/notcurses.h:690
   with Import => True,
        Convention => C,
        External_Name => "nccell_styles";

   procedure nccell_on_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:697
   with Import => True,
        Convention => C,
        External_Name => "nccell_on_styles";

   procedure nccell_off_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:703
   with Import => True,
        Convention => C,
        External_Name => "nccell_off_styles";

   procedure nccell_set_fg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:709
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_default";

   procedure nccell_set_bg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:715
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_default";

   function nccell_set_fg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:720
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_alpha";

   function nccell_set_bg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:725
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_alpha";

   function nccell_double_wide_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:731
   with Import => True,
        Convention => C,
        External_Name => "nccell_double_wide_p";

   function nccell_wide_right_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:737
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_right_p";

   function nccell_wide_left_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:743
   with Import => True,
        Convention => C,
        External_Name => "nccell_wide_left_p";

   function nccell_extended_gcluster (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:750
   with Import => True,
        Convention => C,
        External_Name => "nccell_extended_gcluster";

   function nccell_width (n : access constant ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:754
   with Import => True,
        Convention => C,
        External_Name => "nccell_width";

   function nccell_strdup (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:759
   with Import => True,
        Convention => C,
        External_Name => "nccell_strdup";

   function nccell_extract
     (n : access constant ncplane;
      c : access constant nccell;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:765
   with Import => True,
        Convention => C,
        External_Name => "nccell_extract";

   function nccellcmp
     (n1 : access constant ncplane;
      c1 : access constant nccell;
      n2 : access constant ncplane;
      c2 : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:782
   with Import => True,
        Convention => C,
        External_Name => "nccellcmp";

   function nccell_load_char
     (n : access ncplane;
      c : access nccell;
      ch : char) return int  -- /usr/local/include/notcurses/notcurses.h:796
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_char";

   function nccell_load_egc32
     (n : access ncplane;
      c : access nccell;
      egc : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:806
   with Import => True,
        Convention => C,
        External_Name => "nccell_load_egc32";

   type ncloglevel_e is
     (NCLOGLEVEL_SILENT,
      NCLOGLEVEL_PANIC,
      NCLOGLEVEL_FATAL,
      NCLOGLEVEL_ERROR,
      NCLOGLEVEL_WARNING,
      NCLOGLEVEL_INFO,
      NCLOGLEVEL_VERBOSE,
      NCLOGLEVEL_DEBUG,
      NCLOGLEVEL_TRACE)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:829

   type notcurses_options is record
      termtype : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:879
      renderfp : File_Pointer;  -- /usr/local/include/notcurses/notcurses.h:882
      loglevel : aliased ncloglevel_e;  -- /usr/local/include/notcurses/notcurses.h:885
      margin_t : aliased int;  -- /usr/local/include/notcurses/notcurses.h:890
      margin_r : aliased int;  -- /usr/local/include/notcurses/notcurses.h:890
      margin_b : aliased int;  -- /usr/local/include/notcurses/notcurses.h:890
      margin_l : aliased int;  -- /usr/local/include/notcurses/notcurses.h:890
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:894
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:875

   function notcurses_lex_margins (op : Interfaces.C.Strings.chars_ptr; opts : access notcurses_options) return int  -- /usr/local/include/notcurses/notcurses.h:900
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_margins";

   function notcurses_lex_blitter (op : Interfaces.C.Strings.chars_ptr; blitter : access ncblitter_e) return int  -- /usr/local/include/notcurses/notcurses.h:903
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_blitter";

   function notcurses_str_blitter (blitter : ncblitter_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:906
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_blitter";

   function notcurses_lex_scalemode (op : Interfaces.C.Strings.chars_ptr; scalemode : access ncscale_e) return int  -- /usr/local/include/notcurses/notcurses.h:909
   with Import => True,
        Convention => C,
        External_Name => "notcurses_lex_scalemode";

   function notcurses_str_scalemode (scalemode : ncscale_e) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:912
   with Import => True,
        Convention => C,
        External_Name => "notcurses_str_scalemode";

   function notcurses_init (opts : access constant notcurses_options; fp : File_Pointer) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:918
   with Import => True,
        Convention => C,
        External_Name => "notcurses_init";

   function notcurses_core_init (opts : access constant notcurses_options; fp : File_Pointer) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:922
   with Import => True,
        Convention => C,
        External_Name => "notcurses_core_init";

   function notcurses_stop (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:925
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stop";

   function ncpile_top (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:928
   with Import => True,
        Convention => C,
        External_Name => "ncpile_top";

   function ncpile_bottom (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:931
   with Import => True,
        Convention => C,
        External_Name => "ncpile_bottom";

   function ncpile_render (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:935
   with Import => True,
        Convention => C,
        External_Name => "ncpile_render";

   function ncpile_rasterize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:940
   with Import => True,
        Convention => C,
        External_Name => "ncpile_rasterize";

   function notcurses_render (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:943
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render";

   function notcurses_render_to_buffer
     (nc : access notcurses;
      buf : System.Address;
      buflen : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:949
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render_to_buffer";

   function notcurses_render_to_file (nc : access notcurses; fp : File_Pointer) return int  -- /usr/local/include/notcurses/notcurses.h:953
   with Import => True,
        Convention => C,
        External_Name => "notcurses_render_to_file";

   function notcurses_top (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:956
   with Import => True,
        Convention => C,
        External_Name => "notcurses_top";

   function notcurses_bottom (n : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:959
   with Import => True,
        Convention => C,
        External_Name => "notcurses_bottom";

   procedure notcurses_drop_planes (nc : access notcurses)  -- /usr/local/include/notcurses/notcurses.h:962
   with Import => True,
        Convention => C,
        External_Name => "notcurses_drop_planes";

   function nckey_supppuab_p (w : Wide_Wide_Character) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:981
   with Import => True,
        Convention => C,
        External_Name => "nckey_supppuab_p";

   function nckey_mouse_p (r : Wide_Wide_Character) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:987
   with Import => True,
        Convention => C,
        External_Name => "nckey_mouse_p";

   type ncinput is record
      id : aliased Wide_Wide_Character;  -- /usr/local/include/notcurses/notcurses.h:993
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:994
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:995
      alt : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:996
      shift : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:997
      ctrl : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:998
      seqnum : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:999
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:992

   function ncinput_equal_p (n1 : access constant ncinput; n2 : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1005
   with Import => True,
        Convention => C,
        External_Name => "ncinput_equal_p";

   function notcurses_getc
     (n : access notcurses;
      ts : access constant System.OS_Interface.timespec;
      sigmask : access constant System.OS_Interface.sigset_t;
      ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1026
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc";

   function notcurses_inputready_fd (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1034
   with Import => True,
        Convention => C,
        External_Name => "notcurses_inputready_fd";

   function notcurses_getc_nblock (n : access notcurses; ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1040
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc_nblock";

   function notcurses_getc_blocking (n : access notcurses; ni : access ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/notcurses.h:1050
   with Import => True,
        Convention => C,
        External_Name => "notcurses_getc_blocking";

   function notcurses_mouse_enable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1059
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mouse_enable";

   function notcurses_mouse_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1062
   with Import => True,
        Convention => C,
        External_Name => "notcurses_mouse_disable";

   function notcurses_linesigs_disable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1066
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_disable";

   function notcurses_linesigs_enable (n : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1070
   with Import => True,
        Convention => C,
        External_Name => "notcurses_linesigs_enable";

   function notcurses_refresh
     (n : access notcurses;
      y : access int;
      x : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1076
   with Import => True,
        Convention => C,
        External_Name => "notcurses_refresh";

   function ncplane_notcurses (n : access ncplane) return access notcurses  -- /usr/local/include/notcurses/notcurses.h:1079
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses";

   function ncplane_notcurses_const (n : access constant ncplane) return access constant notcurses  -- /usr/local/include/notcurses/notcurses.h:1080
   with Import => True,
        Convention => C,
        External_Name => "ncplane_notcurses_const";

   procedure ncplane_dim_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1083
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_yx";

   function notcurses_stdplane (nc : access notcurses) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1088
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane";

   function notcurses_stdplane_const (nc : access constant notcurses) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1089
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stdplane_const";

   function notcurses_stddim_yx
     (nc : access notcurses;
      y : access int;
      x : access int) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1093
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx";

   function notcurses_stddim_yx_const
     (nc : access constant notcurses;
      y : access int;
      x : access int) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1100
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stddim_yx_const";

   function ncplane_dim_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1107
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_y";

   function ncplane_dim_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1114
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dim_x";

   procedure ncplane_pixelgeom
     (n : access ncplane;
      pxy : access int;
      pxx : access int;
      celldimy : access int;
      celldimx : access int;
      maxbmapy : access int;
      maxbmapx : access int)  -- /usr/local/include/notcurses/notcurses.h:1126
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pixelgeom";

   procedure notcurses_term_dim_yx
     (n : access constant notcurses;
      rows : access int;
      cols : access int)  -- /usr/local/include/notcurses/notcurses.h:1133
   with Import => True,
        Convention => C,
        External_Name => "notcurses_term_dim_yx";

   function notcurses_at_yx
     (nc : access notcurses;
      yoff : int;
      xoff : int;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1140
   with Import => True,
        Convention => C,
        External_Name => "notcurses_at_yx";

   type ncplane_options is record
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1156
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1157
      rows : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1158
      cols : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1159
      userptr : System.Address;  -- /usr/local/include/notcurses/notcurses.h:1160
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:1161
      resizecb : access function (arg1 : access ncplane) return int;  -- /usr/local/include/notcurses/notcurses.h:1162
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1163
      margin_b : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1164
      margin_r : aliased int;  -- /usr/local/include/notcurses/notcurses.h:1164
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1155

   function ncplane_create (n : access ncplane; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1172
   with Import => True,
        Convention => C,
        External_Name => "ncplane_create";

   function ncpile_create (nc : access notcurses; nopts : access constant ncplane_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1176
   with Import => True,
        Convention => C,
        External_Name => "ncpile_create";

   function ncplane_resize_maximize (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1180
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_maximize";

   function ncplane_resize_marginalized (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1185
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_marginalized";

   function ncplane_resize_realign (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1189
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_realign";

   procedure ncplane_set_resizecb (n : access ncplane; resizecb : access function (arg1 : access ncplane) return int)  -- /usr/local/include/notcurses/notcurses.h:1193
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_resizecb";

   function ncplane_resizecb (n : access constant ncplane) return access function (arg1 : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1196
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resizecb";

   function ncplane_reparent (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1204
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent";

   function ncplane_reparent_family (n : access ncplane; newparent : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1210
   with Import => True,
        Convention => C,
        External_Name => "ncplane_reparent_family";

   function ncplane_dup (n : access constant ncplane; opaque : System.Address) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1217
   with Import => True,
        Convention => C,
        External_Name => "ncplane_dup";

   procedure ncplane_translate
     (src : access constant ncplane;
      dst : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1222
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate";

   function ncplane_translate_abs
     (n : access constant ncplane;
      y : access int;
      x : access int) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1229
   with Import => True,
        Convention => C,
        External_Name => "ncplane_translate_abs";

   function ncplane_set_scrolling (n : access ncplane; scrollp : Extensions.bool) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1234
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_scrolling";

   function notcurses_supported_styles (nc : access constant notcurses) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1242
   with Import => True,
        Convention => C,
        External_Name => "notcurses_supported_styles";

   function notcurses_palette_size (nc : access constant notcurses) return unsigned  -- /usr/local/include/notcurses/notcurses.h:1247
   with Import => True,
        Convention => C,
        External_Name => "notcurses_palette_size";

   function notcurses_cantruecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1250
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cantruecolor";

   function notcurses_canfade (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1253
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canfade";

   function notcurses_canchangecolor (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1256
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canchangecolor";

   function notcurses_canopen_images (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1259
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_images";

   function notcurses_canopen_videos (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1262
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canopen_videos";

   function notcurses_canutf8 (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1265
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canutf8";

   function notcurses_canhalfblock (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1268
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canhalfblock";

   function notcurses_canquadrant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1271
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canquadrant";

   function notcurses_cansextant (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1274
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cansextant";

   function notcurses_canbraille (nc : access constant notcurses) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:1277
   with Import => True,
        Convention => C,
        External_Name => "notcurses_canbraille";

   function notcurses_check_pixel_support (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:1282
   with Import => True,
        Convention => C,
        External_Name => "notcurses_check_pixel_support";

   type ncstats is record
      renders : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1288
      writeouts : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1289
      failed_renders : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1290
      failed_writeouts : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1291
      render_bytes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1293
      render_max_bytes : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1294
      render_min_bytes : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1295
      render_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1296
      render_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1297
      render_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1298
      writeout_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1299
      writeout_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1300
      writeout_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1301
      cellelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1302
      cellemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1303
      fgelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1304
      fgemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1305
      bgelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1306
      bgemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1307
      defaultelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1308
      defaultemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1309
      refreshes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1310
      fbbytes : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1313
      planes : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:1314
      raster_ns : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1317
      raster_max_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1318
      raster_min_ns : aliased Integer_64;  -- /usr/local/include/notcurses/notcurses.h:1319
      sprixelemissions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1320
      sprixelelisions : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:1321
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:1286

   function notcurses_stats_alloc (nc : access constant notcurses) return access ncstats  -- /usr/local/include/notcurses/notcurses.h:1326
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats_alloc";

   procedure notcurses_stats (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1331
   with Import => True,
        Convention => C,
        External_Name => "notcurses_stats";

   procedure notcurses_stats_reset (nc : access notcurses; stats : access ncstats)  -- /usr/local/include/notcurses/notcurses.h:1336
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
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1353
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize";

   function ncplane_resize_simple
     (n : access ncplane;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1359
   with Import => True,
        Convention => C,
        External_Name => "ncplane_resize_simple";

   function ncplane_destroy (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1370
   with Import => True,
        Convention => C,
        External_Name => "ncplane_destroy";

   function ncplane_set_base_cell (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1376
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base_cell";

   function ncplane_set_base
     (n : access ncplane;
      egc : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1381
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_base";

   function ncplane_base (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1386
   with Import => True,
        Convention => C,
        External_Name => "ncplane_base";

   function ncplane_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1391
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_yx";

   procedure ncplane_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1395
   with Import => True,
        Convention => C,
        External_Name => "ncplane_yx";

   function ncplane_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1397
   with Import => True,
        Convention => C,
        External_Name => "ncplane_y";

   function ncplane_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1398
   with Import => True,
        Convention => C,
        External_Name => "ncplane_x";

   procedure ncplane_abs_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1402
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_yx";

   function ncplane_abs_y (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1404
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_y";

   function ncplane_abs_x (n : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1405
   with Import => True,
        Convention => C,
        External_Name => "ncplane_abs_x";

   function ncplane_parent (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1408
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent";

   function ncplane_parent_const (n : access constant ncplane) return access constant ncplane  -- /usr/local/include/notcurses/notcurses.h:1409
   with Import => True,
        Convention => C,
        External_Name => "ncplane_parent_const";

   function ncplane_descendant_p (n : access constant ncplane; ancestor : access constant ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1413
   with Import => True,
        Convention => C,
        External_Name => "ncplane_descendant_p";

   procedure ncplane_move_top (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1423
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_top";

   procedure ncplane_move_bottom (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1424
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_bottom";

   function ncplane_move_above (n : access ncplane; above : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1429
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_above";

   function ncplane_move_below (n : access ncplane; below : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1435
   with Import => True,
        Convention => C,
        External_Name => "ncplane_move_below";

   function ncplane_below (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1439
   with Import => True,
        Convention => C,
        External_Name => "ncplane_below";

   function ncplane_above (n : access ncplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:1440
   with Import => True,
        Convention => C,
        External_Name => "ncplane_above";

   function ncplane_rotate_cw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1448
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_cw";

   function ncplane_rotate_ccw (n : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1449
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rotate_ccw";

   function ncplane_at_cursor
     (n : access ncplane;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1454
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor";

   function ncplane_at_cursor_cell (n : access ncplane; c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1459
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_cursor_cell";

   function ncplane_at_yx
     (n : access constant ncplane;
      y : int;
      x : int;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1464
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx";

   function ncplane_at_yx_cell
     (n : access ncplane;
      y : int;
      x : int;
      c : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1470
   with Import => True,
        Convention => C,
        External_Name => "ncplane_at_yx_cell";

   function ncplane_contents
     (n : access constant ncplane;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:1476
   with Import => True,
        Convention => C,
        External_Name => "ncplane_contents";

   function ncplane_set_userptr (n : access ncplane; opaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:1482
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_userptr";

   function ncplane_userptr (n : access ncplane) return System.Address  -- /usr/local/include/notcurses/notcurses.h:1483
   with Import => True,
        Convention => C,
        External_Name => "ncplane_userptr";

   procedure ncplane_center_abs
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1485
   with Import => True,
        Convention => C,
        External_Name => "ncplane_center_abs";

   function notcurses_align
     (availu : int;
      align : ncalign_e;
      u : int) return int  -- /usr/local/include/notcurses/notcurses.h:1492
   with Import => True,
        Convention => C,
        External_Name => "notcurses_align";

   function ncplane_halign
     (n : access constant ncplane;
      align : ncalign_e;
      c : int) return int  -- /usr/local/include/notcurses/notcurses.h:1509
   with Import => True,
        Convention => C,
        External_Name => "ncplane_halign";

   function ncplane_align
     (n : access constant ncplane;
      align : ncalign_e;
      c : int) return int  -- /usr/local/include/notcurses/notcurses.h:1514
   with Import => True,
        Convention => C,
        External_Name => "ncplane_align";

   function ncplane_valign
     (n : access constant ncplane;
      align : ncalign_e;
      r : int) return int  -- /usr/local/include/notcurses/notcurses.h:1522
   with Import => True,
        Convention => C,
        External_Name => "ncplane_valign";

   function ncplane_cursor_move_yx
     (n : access ncplane;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:1529
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_move_yx";

   procedure ncplane_home (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1532
   with Import => True,
        Convention => C,
        External_Name => "ncplane_home";

   procedure ncplane_cursor_yx
     (n : access constant ncplane;
      y : access int;
      x : access int)  -- /usr/local/include/notcurses/notcurses.h:1535
   with Import => True,
        Convention => C,
        External_Name => "ncplane_cursor_yx";

   function ncplane_channels (n : access constant ncplane) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:1538
   with Import => True,
        Convention => C,
        External_Name => "ncplane_channels";

   function ncplane_styles (n : access constant ncplane) return Unsigned_16  -- /usr/local/include/notcurses/notcurses.h:1541
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles";

   function ncplane_putc_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1547
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc_yx";

   function ncplane_putc (n : access ncplane; c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1551
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putc";

   function ncplane_putchar_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1559
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_yx";

   function wcwidth return int  -- /usr/local/include/notcurses/notcurses.h:1560
   with Import => True,
        Convention => C,
        External_Name => "wcwidth";

   function ncplane_putchar (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1566
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar";

   function ncplane_putchar_stained (n : access ncplane; c : char) return int  -- /usr/local/include/notcurses/notcurses.h:1572
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putchar_stained";

   function ncplane_putegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1579
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_yx";

   function ncplane_putegc
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1583
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc";

   function ncplane_putegc_stained
     (n : access ncplane;
      gclust : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1589
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putegc_stained";

   function ncplane_putwegc
     (n : access ncplane;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1597
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc";

   function ncplane_putwegc_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1616
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_yx";

   function ncplane_putwegc_stained
     (n : access ncplane;
      gclust : access Wide_Character;
      sbytes : access int) return int  -- /usr/local/include/notcurses/notcurses.h:1626
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwegc_stained";

   function ncplane_putstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1634
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_yx";

   function ncplane_putstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1641
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_aligned";

   function ncplane_putstr_stained (n : access ncplane; s : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1646
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putstr_stained";

   function ncplane_putnstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      s : Interfaces.C.size_t;
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1654
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_yx";

   function ncplane_putnstr
     (n : access ncplane;
      s : Interfaces.C.size_t;
      gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1657
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr";

   function ncplane_putnstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      s : Interfaces.C.size_t;
      gclustarr : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:1661
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putnstr_aligned";

   function ncplane_putwstr_yx
     (n : access ncplane;
      y : int;
      x : int;
      gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1666
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_yx";

   function ncplane_putwstr_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1684
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_aligned";

   function wcswidth return int  -- /usr/local/include/notcurses/notcurses.h:1686
   with Import => True,
        Convention => C,
        External_Name => "wcswidth";

   function ncplane_putwstr_stained (n : access ncplane; gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1694
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr_stained";

   function ncplane_putwstr (n : access ncplane; gclustarr : access Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1697
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwstr";

   function ncplane_putwc_yx
     (n : access ncplane;
      y : int;
      x : int;
      w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1705
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_yx";

   function ncplane_putwc (n : access ncplane; w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1712
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc";

   function ncplane_putwc_stained (n : access ncplane; w : Wide_Character) return int  -- /usr/local/include/notcurses/notcurses.h:1719
   with Import => True,
        Convention => C,
        External_Name => "ncplane_putwc_stained";

   function ncplane_vprintf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1725
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_aligned";

   function ncplane_vprintf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1728
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_yx";

   function ncplane_vprintf
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1732
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf";

   function ncplane_vprintf_stained
     (n : access ncplane;
      format : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:1736
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vprintf_stained";

   function ncplane_printf (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1743
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf";

   function ncplane_printf_yx
     (n : access ncplane;
      y : int;
      x : int;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1756
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_yx";

   function ncplane_printf_aligned
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1770
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_aligned";

   function ncplane_printf_stained (n : access ncplane; format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/notcurses.h:1783
   with Import => True,
        Convention => C,
        External_Name => "ncplane_printf_stained";

   function ncplane_puttext
     (n : access ncplane;
      y : int;
      align : ncalign_e;
      text : Interfaces.C.Strings.chars_ptr;
      bytes : access Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:1810
   with Import => True,
        Convention => C,
        External_Name => "ncplane_puttext";

   function ncplane_hline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : int;
      c1 : Unsigned_64;
      c2 : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1819
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline_interp";

   function ncplane_hline
     (n : access ncplane;
      c : access constant nccell;
      len : int) return int  -- /usr/local/include/notcurses/notcurses.h:1823
   with Import => True,
        Convention => C,
        External_Name => "ncplane_hline";

   function ncplane_vline_interp
     (n : access ncplane;
      c : access constant nccell;
      len : int;
      c1 : Unsigned_64;
      c2 : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1827
   with Import => True,
        Convention => C,
        External_Name => "ncplane_vline_interp";

   function ncplane_vline
     (n : access ncplane;
      c : access constant nccell;
      len : int) return int  -- /usr/local/include/notcurses/notcurses.h:1831
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
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1864
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
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1873
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
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:1883
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter";

   function ncplane_polyfill_yx
     (n : access ncplane;
      y : int;
      x : int;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:1899
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
      xstop : int) return int  -- /usr/local/include/notcurses/notcurses.h:1920
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
      xstop : int) return int  -- /usr/local/include/notcurses/notcurses.h:1928
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
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1934
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
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1947
   with Import => True,
        Convention => C,
        External_Name => "ncplane_highgradient_sized";

   function ncplane_format
     (n : access ncplane;
      ystop : int;
      xstop : int;
      stylemask : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:1952
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
      lr : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:1956
   with Import => True,
        Convention => C,
        External_Name => "ncplane_stain";

   function ncplane_mergedown_simple (src : access ncplane; dst : access ncplane) return int  -- /usr/local/include/notcurses/notcurses.h:1961
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
      dstx : int) return int  -- /usr/local/include/notcurses/notcurses.h:1974
   with Import => True,
        Convention => C,
        External_Name => "ncplane_mergedown";

   procedure ncplane_erase (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:1983
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase";

   function ncplane_erase_region
     (n : access ncplane;
      ystart : int;
      xstart : int;
      ylen : int;
      xlen : int) return int  -- /usr/local/include/notcurses/notcurses.h:1990
   with Import => True,
        Convention => C,
        External_Name => "ncplane_erase_region";

   function nccell_fg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:1996
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb";

   function nccell_bg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2002
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb";

   function nccell_fg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2008
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_alpha";

   function nccell_bg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2014
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_alpha";

   function nccell_fg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2020
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_rgb8";

   function nccell_bg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2026
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_rgb8";

   function nccell_set_fg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2033
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8";

   procedure nccell_set_fg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2039
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb8_clipped";

   function nccell_set_fg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2045
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_rgb";

   function nccell_set_fg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2052
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_fg_palindex";

   function nccell_fg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2057
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex";

   function nccell_set_bg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2064
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8";

   procedure nccell_set_bg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2070
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb8_clipped";

   function nccell_set_bg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2077
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_rgb";

   function nccell_set_bg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2084
   with Import => True,
        Convention => C,
        External_Name => "nccell_set_bg_palindex";

   function nccell_bg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2089
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex";

   function nccell_fg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2095
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_default_p";

   function nccell_fg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2100
   with Import => True,
        Convention => C,
        External_Name => "nccell_fg_palindex_p";

   function nccell_bg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2108
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_default_p";

   function nccell_bg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2113
   with Import => True,
        Convention => C,
        External_Name => "nccell_bg_palindex_p";

   function ncplane_bchannel (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2119
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bchannel";

   function ncplane_fchannel (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2125
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fchannel";

   procedure ncplane_set_channels (n : access ncplane; channels : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:2129
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_channels";

   procedure ncplane_set_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2133
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_styles";

   procedure ncplane_on_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2136
   with Import => True,
        Convention => C,
        External_Name => "ncplane_on_styles";

   procedure ncplane_off_styles (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:2139
   with Import => True,
        Convention => C,
        External_Name => "ncplane_off_styles";

   function ncplane_fg_rgb (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2143
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb";

   function ncplane_bg_rgb (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2149
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb";

   function ncplane_fg_alpha (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2155
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_alpha";

   function ncplane_fg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2161
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_default_p";

   function ncplane_bg_alpha (n : access constant ncplane) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2167
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_alpha";

   function ncplane_bg_default_p (n : access constant ncplane) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2173
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_default_p";

   function ncplane_fg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2179
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fg_rgb8";

   function ncplane_bg_rgb8
     (n : access constant ncplane;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2185
   with Import => True,
        Convention => C,
        External_Name => "ncplane_bg_rgb8";

   function ncplane_set_fchannel (n : access ncplane; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2190
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fchannel";

   function ncplane_set_bchannel (n : access ncplane; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:2191
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bchannel";

   function ncplane_set_fg_rgb8
     (n : access ncplane;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2199
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8";

   function ncplane_set_bg_rgb8
     (n : access ncplane;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2200
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8";

   procedure ncplane_set_bg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2203
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb8_clipped";

   procedure ncplane_set_fg_rgb8_clipped
     (n : access ncplane;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:2204
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb8_clipped";

   function ncplane_set_fg_rgb (n : access ncplane; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2207
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_rgb";

   function ncplane_set_bg_rgb (n : access ncplane; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2208
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_rgb";

   procedure ncplane_set_fg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2211
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_default";

   procedure ncplane_set_bg_default (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2212
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_default";

   function ncplane_set_fg_palindex (n : access ncplane; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2216
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_palindex";

   function ncplane_set_bg_palindex (n : access ncplane; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:2217
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_palindex";

   function ncplane_set_fg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2220
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_fg_alpha";

   function ncplane_set_bg_alpha (n : access ncplane; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2221
   with Import => True,
        Convention => C,
        External_Name => "ncplane_set_bg_alpha";

   type fadecb is access function
        (arg1 : access notcurses;
         arg2 : access ncplane;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2226

   function ncplane_fadeout
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2233
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout";

   function ncplane_fadein
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2239
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein";

   function ncfadectx_setup (n : access ncplane) return access ncfadectx  -- /usr/local/include/notcurses/notcurses.h:2244
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_setup";

   function ncfadectx_iterations (nctx : access constant ncfadectx) return int  -- /usr/local/include/notcurses/notcurses.h:2247
   with Import => True,
        Convention => C,
        External_Name => "ncfadectx_iterations";

   function ncplane_fadeout_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2251
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadeout_iteration";

   function ncplane_fadein_iteration
     (n : access ncplane;
      nctx : access ncfadectx;
      iter : int;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2256
   with Import => True,
        Convention => C,
        External_Name => "ncplane_fadein_iteration";

   function ncplane_pulse
     (n : access ncplane;
      ts : access constant System.OS_Interface.timespec;
      fader : fadecb;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2264
   with Import => True,
        Convention => C,
        External_Name => "ncplane_pulse";

   procedure ncfadectx_free (nctx : access ncfadectx)  -- /usr/local/include/notcurses/notcurses.h:2267
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
      gclusters : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:2274
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
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2299
   with Import => True,
        Convention => C,
        External_Name => "nccells_rounded_box";

   function ncplane_rounded_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ystop : int;
      xstop : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2304
   with Import => True,
        Convention => C,
        External_Name => "ncplane_rounded_box";

   function ncplane_perimeter_rounded
     (n : access ncplane;
      stylemask : Unsigned_32;
      channels : Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2320
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_rounded";

   function ncplane_rounded_box_sized
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2344
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
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:2352
   with Import => True,
        Convention => C,
        External_Name => "nccells_double_box";

   function ncplane_double_box
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ystop : int;
      xstop : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2357
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box";

   function ncplane_perimeter_double
     (n : access ncplane;
      stylemask : Unsigned_32;
      channels : Unsigned_64;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2373
   with Import => True,
        Convention => C,
        External_Name => "ncplane_perimeter_double";

   function ncplane_double_box_sized
     (n : access ncplane;
      styles : Unsigned_32;
      channels : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2397
   with Import => True,
        Convention => C,
        External_Name => "ncplane_double_box_sized";

   function ncvisual_from_file (file : Interfaces.C.Strings.chars_ptr) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2407
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_file";

   function ncvisual_from_rgba
     (rgba : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2415
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_rgba";

   function ncvisual_from_bgra
     (bgra : System.Address;
      rows : int;
      rowstride : int;
      cols : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2419
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_bgra";

   function ncvisual_from_plane
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return access ncvisual  -- /usr/local/include/notcurses/notcurses.h:2427
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_from_plane";

   type ncvisual_options is record
      n : access ncplane;  -- /usr/local/include/notcurses/notcurses.h:2444
      scaling : aliased ncscale_e;  -- /usr/local/include/notcurses/notcurses.h:2448
      y : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2454
      x : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2454
      begy : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2459
      begx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2459
      leny : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2460
      lenx : aliased int;  -- /usr/local/include/notcurses/notcurses.h:2460
      blitter : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:2464
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2465
      transcolor : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:2466
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2439

   function ncplane_as_rgba
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int;
      pxdimy : access int;
      pxdimx : access int) return access Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2475
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
      blitter : access ncblitter_e) return int  -- /usr/local/include/notcurses/notcurses.h:2487
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_blitter_geom";

   procedure ncvisual_destroy (ncv : access ncvisual)  -- /usr/local/include/notcurses/notcurses.h:2494
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_destroy";

   function ncvisual_decode (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:2498
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode";

   function ncvisual_decode_loop (nc : access ncvisual) return int  -- /usr/local/include/notcurses/notcurses.h:2505
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_decode_loop";

   function ncvisual_rotate (n : access ncvisual; rads : double) return int  -- /usr/local/include/notcurses/notcurses.h:2510
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_rotate";

   function ncvisual_resize
     (n : access ncvisual;
      rows : int;
      cols : int) return int  -- /usr/local/include/notcurses/notcurses.h:2515
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_resize";

   function ncvisual_inflate (n : access ncvisual; scale : int) return int  -- /usr/local/include/notcurses/notcurses.h:2520
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_inflate";

   function ncvisual_polyfill_yx
     (n : access ncvisual;
      y : int;
      x : int;
      rgba : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2524
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_polyfill_yx";

   function ncvisual_at_yx
     (n : access constant ncvisual;
      y : int;
      x : int;
      pixel : access Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2528
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_at_yx";

   function ncvisual_set_yx
     (n : access constant ncvisual;
      y : int;
      x : int;
      pixel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:2532
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_set_yx";

   function ncvisual_render
     (nc : access notcurses;
      ncv : access ncvisual;
      vopts : access constant ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2541
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_render";

   function ncvisualplane_create
     (n : access ncplane;
      opts : access constant ncplane_options;
      ncv : access ncvisual;
      vopts : access ncvisual_options) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2546
   with Import => True,
        Convention => C,
        External_Name => "ncvisualplane_create";

   function ncvisual_subtitle (ncv : access constant ncvisual) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2570
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_subtitle";

   function ncvisual_media_defblitter (nc : access constant notcurses; scale : ncscale_e) return ncblitter_e  -- /usr/local/include/notcurses/notcurses.h:2581
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_media_defblitter";

   type ncstreamcb is access function
        (arg1 : access ncvisual;
         arg2 : access ncvisual_options;
         arg3 : access constant System.OS_Interface.timespec;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2586

   function ncvisual_simple_streamer
     (ncv : access ncvisual;
      vopts : access ncvisual_options;
      tspec : access constant System.OS_Interface.timespec;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2592
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_simple_streamer";

   function ncvisual_stream
     (nc : access notcurses;
      ncv : access ncvisual;
      timescale : float;
      streamer : ncstreamcb;
      vopts : access constant ncvisual_options;
      curry : System.Address) return int  -- /usr/local/include/notcurses/notcurses.h:2604
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_stream";

   function ncblit_rgba
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:2615
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgba";

   function ncblit_bgrx
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options) return int  -- /usr/local/include/notcurses/notcurses.h:2619
   with Import => True,
        Convention => C,
        External_Name => "ncblit_bgrx";

   function ncblit_rgb_packed
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2624
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_packed";

   function ncblit_rgb_loose
     (data : System.Address;
      linesize : int;
      vopts : access constant ncvisual_options;
      alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:2629
   with Import => True,
        Convention => C,
        External_Name => "ncblit_rgb_loose";

   function ncpixel_a (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2644
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_a";

   function ncpixel_r (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2650
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_r";

   function ncpixel_g (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2656
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_g";

   function ncpixel_b (pixel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:2662
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_b";

   function ncpixel_set_a (pixel : access Unsigned_32; a : int) return int  -- /usr/local/include/notcurses/notcurses.h:2668
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_a";

   function ncpixel_set_r (pixel : access Unsigned_32; r : int) return int  -- /usr/local/include/notcurses/notcurses.h:2678
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_r";

   function ncpixel_set_g (pixel : access Unsigned_32; g : int) return int  -- /usr/local/include/notcurses/notcurses.h:2688
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_g";

   function ncpixel_set_b (pixel : access Unsigned_32; b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2698
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_b";

   function ncpixel
     (r : int;
      g : int;
      b : int) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:2708
   with Import => True,
        Convention => C,
        External_Name => "ncpixel";

   function ncpixel_set_rgb8
     (pixel : access Unsigned_32;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2725
   with Import => True,
        Convention => C,
        External_Name => "ncpixel_set_rgb8";

   type ncreel_options is record
      bordermask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2758
      borderchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2759
      tabletmask : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2760
      tabletchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2761
      focusedchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2762
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2763
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2751

   function ncreel_create (n : access ncplane; popts : access constant ncreel_options) return access ncreel  -- /usr/local/include/notcurses/notcurses.h:2768
   with Import => True,
        Convention => C,
        External_Name => "ncreel_create";

   function ncreel_plane (nr : access ncreel) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2772
   with Import => True,
        Convention => C,
        External_Name => "ncreel_plane";

   type tabletcb is access function (arg1 : access nctablet; arg2 : Extensions.bool) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:2779

   function ncreel_add
     (nr : access ncreel;
      after : access nctablet;
      before : access nctablet;
      cb : tabletcb;
      opaque : System.Address) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:2788
   with Import => True,
        Convention => C,
        External_Name => "ncreel_add";

   function ncreel_tabletcount (nr : access constant ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:2794
   with Import => True,
        Convention => C,
        External_Name => "ncreel_tabletcount";

   function ncreel_del (nr : access ncreel; t : access nctablet) return int  -- /usr/local/include/notcurses/notcurses.h:2799
   with Import => True,
        Convention => C,
        External_Name => "ncreel_del";

   function ncreel_redraw (nr : access ncreel) return int  -- /usr/local/include/notcurses/notcurses.h:2805
   with Import => True,
        Convention => C,
        External_Name => "ncreel_redraw";

   function ncreel_offer_input (nr : access ncreel; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:2814
   with Import => True,
        Convention => C,
        External_Name => "ncreel_offer_input";

   function ncreel_focused (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:2819
   with Import => True,
        Convention => C,
        External_Name => "ncreel_focused";

   function ncreel_next (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:2823
   with Import => True,
        Convention => C,
        External_Name => "ncreel_next";

   function ncreel_prev (nr : access ncreel) return access nctablet  -- /usr/local/include/notcurses/notcurses.h:2827
   with Import => True,
        Convention => C,
        External_Name => "ncreel_prev";

   procedure ncreel_destroy (nr : access ncreel)  -- /usr/local/include/notcurses/notcurses.h:2831
   with Import => True,
        Convention => C,
        External_Name => "ncreel_destroy";

   function nctablet_userptr (t : access nctablet) return System.Address  -- /usr/local/include/notcurses/notcurses.h:2834
   with Import => True,
        Convention => C,
        External_Name => "nctablet_userptr";

   function nctablet_plane (t : access nctablet) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:2837
   with Import => True,
        Convention => C,
        External_Name => "nctablet_plane";

   function ncmetric
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int;
      mult : Unsigned_Max;
      uprefix : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2860
   with Import => True,
        Convention => C,
        External_Name => "ncmetric";

   function qprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2887
   with Import => True,
        Convention => C,
        External_Name => "qprefix";

   function iprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2893
   with Import => True,
        Convention => C,
        External_Name => "iprefix";

   function bprefix
     (val : Unsigned_Max;
      decimal : Unsigned_Max;
      buf : Interfaces.C.Strings.chars_ptr;
      omitdec : int) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:2899
   with Import => True,
        Convention => C,
        External_Name => "bprefix";

   function notcurses_cursor_enable
     (nc : access notcurses;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/notcurses.h:2906
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_enable";

   function notcurses_cursor_disable (nc : access notcurses) return int  -- /usr/local/include/notcurses/notcurses.h:2907
   with Import => True,
        Convention => C,
        External_Name => "notcurses_cursor_disable";

   type anon2548_array2550 is array (0 .. 255) of aliased Unsigned_32;
   type ncpalette is record
      chans : aliased anon2548_array2550;  -- /usr/local/include/notcurses/notcurses.h:2915
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2914

   function ncpalette_new (nc : access notcurses) return access ncpalette  -- /usr/local/include/notcurses/notcurses.h:2921
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_new";

   function ncpalette_use (nc : access notcurses; p : access constant ncpalette) return int  -- /usr/local/include/notcurses/notcurses.h:2925
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_use";

   function ncpalette_set_rgb8
     (p : access ncpalette;
      idx : int;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:2929
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set_rgb8";

   function ncpalette_set
     (p : access ncpalette;
      idx : int;
      rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2937
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_set";

   function ncpalette_get_rgb8
     (p : access constant ncpalette;
      idx : int;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:2945
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_get_rgb8";

   procedure ncpalette_free (p : access ncpalette)  -- /usr/local/include/notcurses/notcurses.h:2953
   with Import => True,
        Convention => C,
        External_Name => "ncpalette_free";

   procedure ncplane_greyscale (n : access ncplane)  -- /usr/local/include/notcurses/notcurses.h:2956
   with Import => True,
        Convention => C,
        External_Name => "ncplane_greyscale";

   type ncselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:2976
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:2977
      opcolumns : aliased Interfaces.C.size_t;  -- /usr/local/include/notcurses/notcurses.h:2978
      desccolumns : aliased Interfaces.C.size_t;  -- /usr/local/include/notcurses/notcurses.h:2979
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2975

   type ncselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:2983
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:2984
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:2985
      items : access ncselector_item;  -- /usr/local/include/notcurses/notcurses.h:2986
      defidx : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2989
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:2991
      opchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2993
      descchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2994
      titlechannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2995
      footchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2996
      boxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2997
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:2998
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:2982

   function ncselector_create (n : access ncplane; opts : access constant ncselector_options) return access ncselector  -- /usr/local/include/notcurses/notcurses.h:3001
   with Import => True,
        Convention => C,
        External_Name => "ncselector_create";

   function ncselector_additem (n : access ncselector; item : access constant ncselector_item) return int  -- /usr/local/include/notcurses/notcurses.h:3006
   with Import => True,
        Convention => C,
        External_Name => "ncselector_additem";

   function ncselector_delitem (n : access ncselector; item : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3007
   with Import => True,
        Convention => C,
        External_Name => "ncselector_delitem";

   function ncselector_selected (n : access constant ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3010
   with Import => True,
        Convention => C,
        External_Name => "ncselector_selected";

   function ncselector_plane (n : access ncselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3013
   with Import => True,
        Convention => C,
        External_Name => "ncselector_plane";

   function ncselector_previtem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3017
   with Import => True,
        Convention => C,
        External_Name => "ncselector_previtem";

   function ncselector_nextitem (n : access ncselector) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3018
   with Import => True,
        Convention => C,
        External_Name => "ncselector_nextitem";

   function ncselector_offer_input (n : access ncselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3027
   with Import => True,
        Convention => C,
        External_Name => "ncselector_offer_input";

   procedure ncselector_destroy (n : access ncselector; item : System.Address)  -- /usr/local/include/notcurses/notcurses.h:3032
   with Import => True,
        Convention => C,
        External_Name => "ncselector_destroy";

   type ncmselector_item is record
      option : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3035
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3036
      selected : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:3037
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3034

   type ncmultiselector_options is record
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3062
      secondary : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3063
      footer : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3064
      items : access ncmselector_item;  -- /usr/local/include/notcurses/notcurses.h:3065
      maxdisplay : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3067
      opchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3069
      descchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3070
      titlechannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3071
      footchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3072
      boxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3073
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3074
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3061

   function ncmultiselector_create (n : access ncplane; opts : access constant ncmultiselector_options) return access ncmultiselector  -- /usr/local/include/notcurses/notcurses.h:3077
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_create";

   function ncmultiselector_selected
     (n : access ncmultiselector;
      selected : access Extensions.bool;
      count : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:3082
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_selected";

   function ncmultiselector_plane (n : access ncmultiselector) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3085
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_plane";

   function ncmultiselector_offer_input (n : access ncmultiselector; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3094
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_offer_input";

   procedure ncmultiselector_destroy (n : access ncmultiselector)  -- /usr/local/include/notcurses/notcurses.h:3098
   with Import => True,
        Convention => C,
        External_Name => "ncmultiselector_destroy";

   type nctree_item;
   type nctree_item is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3109
      subs : access nctree_item;  -- /usr/local/include/notcurses/notcurses.h:3110
      subcount : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3111
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3108

   type nctree_options is record
      items : access constant nctree_item;  -- /usr/local/include/notcurses/notcurses.h:3115
      count : aliased unsigned;  -- /usr/local/include/notcurses/notcurses.h:3116
      nctreecb : access function
           (arg1 : access ncplane;
            arg2 : System.Address;
            arg3 : int) return int;  -- /usr/local/include/notcurses/notcurses.h:3117
      indentcols : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3118
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3119
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3114

   type nctree is null record;   -- incomplete struct

   function nctree_create (n : access ncplane; opts : access constant nctree_options) return access nctree  -- /usr/local/include/notcurses/notcurses.h:3124
   with Import => True,
        Convention => C,
        External_Name => "nctree_create";

   function nctree_plane (n : access nctree) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3128
   with Import => True,
        Convention => C,
        External_Name => "nctree_plane";

   function nctree_redraw (n : access nctree) return int  -- /usr/local/include/notcurses/notcurses.h:3134
   with Import => True,
        Convention => C,
        External_Name => "nctree_redraw";

   function nctree_offer_input (n : access nctree; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3143
   with Import => True,
        Convention => C,
        External_Name => "nctree_offer_input";

   function nctree_focused (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3148
   with Import => True,
        Convention => C,
        External_Name => "nctree_focused";

   function nctree_next (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3151
   with Import => True,
        Convention => C,
        External_Name => "nctree_next";

   function nctree_prev (n : access nctree) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3154
   with Import => True,
        Convention => C,
        External_Name => "nctree_prev";

   function nctree_goto
     (n : access nctree;
      spec : access unsigned;
      failspec : access int) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3161
   with Import => True,
        Convention => C,
        External_Name => "nctree_goto";

   procedure nctree_destroy (n : access nctree)  -- /usr/local/include/notcurses/notcurses.h:3164
   with Import => True,
        Convention => C,
        External_Name => "nctree_destroy";

   type ncmenu_item is record
      desc : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3172
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:3173
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3171

   type ncmenu_section is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3177
      itemcount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3178
      items : access ncmenu_item;  -- /usr/local/include/notcurses/notcurses.h:3179
      shortcut : aliased ncinput;  -- /usr/local/include/notcurses/notcurses.h:3180
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3176

   type ncmenu_options is record
      sections : access ncmenu_section;  -- /usr/local/include/notcurses/notcurses.h:3187
      sectioncount : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3188
      headerchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3189
      sectionchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3190
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3191
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3186

   type ncmenu is null record;   -- incomplete struct

   function ncmenu_create (n : access ncplane; opts : access constant ncmenu_options) return access ncmenu  -- /usr/local/include/notcurses/notcurses.h:3197
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_create";

   function ncmenu_unroll (n : access ncmenu; sectionidx : int) return int  -- /usr/local/include/notcurses/notcurses.h:3202
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_unroll";

   function ncmenu_rollup (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3205
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_rollup";

   function ncmenu_nextsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3209
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextsection";

   function ncmenu_prevsection (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3210
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_prevsection";

   function ncmenu_nextitem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3214
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_nextitem";

   function ncmenu_previtem (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3215
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_previtem";

   function ncmenu_item_set_status
     (n : access ncmenu;
      section : Interfaces.C.Strings.chars_ptr;
      item : Interfaces.C.Strings.chars_ptr;
      enabled : Extensions.bool) return int  -- /usr/local/include/notcurses/notcurses.h:3218
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_item_set_status";

   function ncmenu_selected (n : access constant ncmenu; ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3224
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_selected";

   function ncmenu_mouse_selected
     (n : access constant ncmenu;
      click : access constant ncinput;
      ni : access ncinput) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3230
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_mouse_selected";

   function ncmenu_plane (n : access ncmenu) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3234
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_plane";

   function ncmenu_offer_input (n : access ncmenu; nc : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3245
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_offer_input";

   function ncmenu_destroy (n : access ncmenu) return int  -- /usr/local/include/notcurses/notcurses.h:3249
   with Import => True,
        Convention => C,
        External_Name => "ncmenu_destroy";

   type ncprogbar_options is record
      ulchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3264
      urchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3265
      blchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3266
      brchannel : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3267
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3268
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3263

   function ncprogbar_create (n : access ncplane; opts : access constant ncprogbar_options) return access ncprogbar  -- /usr/local/include/notcurses/notcurses.h:3273
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_create";

   function ncprogbar_plane (n : access ncprogbar) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3277
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_plane";

   function ncprogbar_set_progress (n : access ncprogbar; p : double) return int  -- /usr/local/include/notcurses/notcurses.h:3281
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_set_progress";

   function ncprogbar_progress (n : access constant ncprogbar) return double  -- /usr/local/include/notcurses/notcurses.h:3285
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_progress";

   procedure ncprogbar_destroy (n : access ncprogbar)  -- /usr/local/include/notcurses/notcurses.h:3289
   with Import => True,
        Convention => C,
        External_Name => "ncprogbar_destroy";

   type nctabbed_options is record
      selchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3298
      hdrchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3299
      sepchan : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3300
      separator : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3301
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3302
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3297

   type tabcb is access procedure
        (arg1 : access nctab;
         arg2 : access ncplane;
         arg3 : System.Address)
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3308

   function nctabbed_create (n : access ncplane; opts : access constant nctabbed_options) return access nctabbed  -- /usr/local/include/notcurses/notcurses.h:3315
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_create";

   procedure nctabbed_destroy (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3321
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_destroy";

   procedure nctabbed_redraw (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3326
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_redraw";

   procedure nctabbed_ensure_selected_header_visible (nt : access nctabbed)  -- /usr/local/include/notcurses/notcurses.h:3332
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_ensure_selected_header_visible";

   function nctabbed_selected (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3336
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selected";

   function nctabbed_leftmost (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3340
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_leftmost";

   function nctabbed_tabcount (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:3344
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_tabcount";

   function nctabbed_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3348
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_plane";

   function nctabbed_content_plane (nt : access nctabbed) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3352
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_content_plane";

   function nctab_cb (t : access nctab) return tabcb  -- /usr/local/include/notcurses/notcurses.h:3356
   with Import => True,
        Convention => C,
        External_Name => "nctab_cb";

   function nctab_name (t : access nctab) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3360
   with Import => True,
        Convention => C,
        External_Name => "nctab_name";

   function nctab_name_width (t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3364
   with Import => True,
        Convention => C,
        External_Name => "nctab_name_width";

   function nctab_userptr (t : access nctab) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3368
   with Import => True,
        Convention => C,
        External_Name => "nctab_userptr";

   function nctab_next (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3372
   with Import => True,
        Convention => C,
        External_Name => "nctab_next";

   function nctab_prev (t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3376
   with Import => True,
        Convention => C,
        External_Name => "nctab_prev";

   function nctabbed_add
     (nt : access nctabbed;
      after : access nctab;
      before : access nctab;
      tcb : tabcb;
      name : Interfaces.C.Strings.chars_ptr;
      opaque : System.Address) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3388
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_add";

   function nctabbed_del (nt : access nctabbed; t : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3398
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_del";

   function nctab_move
     (nt : access nctabbed;
      t : access nctab;
      after : access nctab;
      before : access nctab) return int  -- /usr/local/include/notcurses/notcurses.h:3404
   with Import => True,
        Convention => C,
        External_Name => "nctab_move";

   procedure nctab_move_right (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:3409
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_right";

   procedure nctab_move_left (nt : access nctabbed; t : access nctab)  -- /usr/local/include/notcurses/notcurses.h:3413
   with Import => True,
        Convention => C,
        External_Name => "nctab_move_left";

   procedure nctabbed_rotate (nt : access nctabbed; amt : int)  -- /usr/local/include/notcurses/notcurses.h:3419
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_rotate";

   function nctabbed_next (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3424
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_next";

   function nctabbed_prev (nt : access nctabbed) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3429
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_prev";

   function nctabbed_select (nt : access nctabbed; t : access nctab) return access nctab  -- /usr/local/include/notcurses/notcurses.h:3433
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_select";

   procedure nctabbed_channels
     (nt : access nctabbed;
      hdrchan : access Unsigned_64;
      selchan : access Unsigned_64;
      sepchan : access Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3438
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_channels";

   function nctabbed_hdrchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3443
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_hdrchan";

   function nctabbed_selchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3450
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_selchan";

   function nctabbed_sepchan (nt : access nctabbed) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:3457
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_sepchan";

   function nctabbed_separator (nt : access nctabbed) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3466
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator";

   function nctabbed_separator_width (nt : access nctabbed) return int  -- /usr/local/include/notcurses/notcurses.h:3470
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_separator_width";

   procedure nctabbed_set_hdrchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3474
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_hdrchan";

   procedure nctabbed_set_selchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3478
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_selchan";

   procedure nctabbed_set_sepchan (nt : access nctabbed; chan : Unsigned_64)  -- /usr/local/include/notcurses/notcurses.h:3482
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_sepchan";

   function nctab_set_cb (t : access nctab; newcb : tabcb) return tabcb  -- /usr/local/include/notcurses/notcurses.h:3486
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_cb";

   function nctab_set_name (t : access nctab; newname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3490
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_name";

   function nctab_set_userptr (t : access nctab; newopaque : System.Address) return System.Address  -- /usr/local/include/notcurses/notcurses.h:3494
   with Import => True,
        Convention => C,
        External_Name => "nctab_set_userptr";

   function nctabbed_set_separator (nt : access nctabbed; separator : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3499
   with Import => True,
        Convention => C,
        External_Name => "nctabbed_set_separator";

   type ncplot_options is record
      maxchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3548
      minchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3549
      legendstyle : aliased Unsigned_16;  -- /usr/local/include/notcurses/notcurses.h:3551
      gridtype : aliased ncblitter_e;  -- /usr/local/include/notcurses/notcurses.h:3554
      rangex : aliased int;  -- /usr/local/include/notcurses/notcurses.h:3559
      title : Interfaces.C.Strings.chars_ptr;  -- /usr/local/include/notcurses/notcurses.h:3560
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3561
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3545

   function ncuplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : Unsigned_64;
      maxy : Unsigned_64) return access ncuplot  -- /usr/local/include/notcurses/notcurses.h:3568
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_create";

   function ncdplot_create
     (n : access ncplane;
      opts : access constant ncplot_options;
      miny : double;
      maxy : double) return access ncdplot  -- /usr/local/include/notcurses/notcurses.h:3572
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_create";

   function ncuplot_plane (n : access ncuplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3577
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_plane";

   function ncdplot_plane (n : access ncdplot) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3580
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_plane";

   function ncuplot_add_sample
     (n : access ncuplot;
      x : Unsigned_64;
      y : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3587
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_add_sample";

   function ncdplot_add_sample
     (n : access ncdplot;
      x : Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:3589
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_add_sample";

   function ncuplot_set_sample
     (n : access ncuplot;
      x : Unsigned_64;
      y : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3591
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_set_sample";

   function ncdplot_set_sample
     (n : access ncdplot;
      x : Unsigned_64;
      y : double) return int  -- /usr/local/include/notcurses/notcurses.h:3593
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_set_sample";

   function ncuplot_sample
     (n : access constant ncuplot;
      x : Unsigned_64;
      y : access Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3596
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_sample";

   function ncdplot_sample
     (n : access constant ncdplot;
      x : Unsigned_64;
      y : access double) return int  -- /usr/local/include/notcurses/notcurses.h:3598
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_sample";

   procedure ncuplot_destroy (n : access ncuplot)  -- /usr/local/include/notcurses/notcurses.h:3601
   with Import => True,
        Convention => C,
        External_Name => "ncuplot_destroy";

   procedure ncdplot_destroy (n : access ncdplot)  -- /usr/local/include/notcurses/notcurses.h:3602
   with Import => True,
        Convention => C,
        External_Name => "ncdplot_destroy";

   type ncfdplane_callback is access function
        (arg1 : access ncfdplane;
         arg2 : System.Address;
         arg3 : Interfaces.C.size_t;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3604

   type ncfdplane_done_cb is access function
        (arg1 : access ncfdplane;
         arg2 : int;
         arg3 : System.Address) return int
   with Convention => C;  -- /usr/local/include/notcurses/notcurses.h:3605

   type ncfdplane_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3613
      follow : aliased Extensions.bool;  -- /usr/local/include/notcurses/notcurses.h:3614
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3615
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3612

   function ncfdplane_create
     (n : access ncplane;
      opts : access constant ncfdplane_options;
      fd : int;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncfdplane  -- /usr/local/include/notcurses/notcurses.h:3620
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_create";

   function ncfdplane_plane (n : access ncfdplane) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3624
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_plane";

   function ncfdplane_destroy (n : access ncfdplane) return int  -- /usr/local/include/notcurses/notcurses.h:3627
   with Import => True,
        Convention => C,
        External_Name => "ncfdplane_destroy";

   type ncsubproc_options is record
      curry : System.Address;  -- /usr/local/include/notcurses/notcurses.h:3630
      restart_period : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3631
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3632
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3629

   function ncsubproc_createv
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3636
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createv";

   function ncsubproc_createvp
     (n : access ncplane;
      opts : access constant ncsubproc_options;
      bin : Interfaces.C.Strings.chars_ptr;
      arg : System.Address;
      cbfxn : ncfdplane_callback;
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3641
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
      donecbfxn : ncfdplane_done_cb) return access ncsubproc  -- /usr/local/include/notcurses/notcurses.h:3646
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_createvpe";

   function ncsubproc_plane (n : access ncsubproc) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3651
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_plane";

   function ncsubproc_destroy (n : access ncsubproc) return int  -- /usr/local/include/notcurses/notcurses.h:3654
   with Import => True,
        Convention => C,
        External_Name => "ncsubproc_destroy";

   function ncplane_qrcode
     (n : access ncplane;
      ymax : access int;
      xmax : access int;
      data : System.Address;
      len : Interfaces.C.size_t) return int  -- /usr/local/include/notcurses/notcurses.h:3661
   with Import => True,
        Convention => C,
        External_Name => "ncplane_qrcode";

   type ncreader_options is record
      tchannels : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3676
      tattrword : aliased Unsigned_32;  -- /usr/local/include/notcurses/notcurses.h:3677
      flags : aliased Unsigned_64;  -- /usr/local/include/notcurses/notcurses.h:3678
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/notcurses/notcurses.h:3675

   function ncreader_create (n : access ncplane; opts : access constant ncreader_options) return access ncreader  -- /usr/local/include/notcurses/notcurses.h:3684
   with Import => True,
        Convention => C,
        External_Name => "ncreader_create";

   function ncreader_clear (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3688
   with Import => True,
        Convention => C,
        External_Name => "ncreader_clear";

   function ncreader_plane (n : access ncreader) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3691
   with Import => True,
        Convention => C,
        External_Name => "ncreader_plane";

   function ncreader_offer_input (n : access ncreader; ni : access constant ncinput) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3697
   with Import => True,
        Convention => C,
        External_Name => "ncreader_offer_input";

   function ncreader_move_left (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3702
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_left";

   function ncreader_move_right (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3704
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_right";

   function ncreader_move_up (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3706
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_up";

   function ncreader_move_down (n : access ncreader) return int  -- /usr/local/include/notcurses/notcurses.h:3708
   with Import => True,
        Convention => C,
        External_Name => "ncreader_move_down";

   function ncreader_write_egc (n : access ncreader; egc : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3713
   with Import => True,
        Convention => C,
        External_Name => "ncreader_write_egc";

   function ncreader_contents (n : access constant ncreader) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3717
   with Import => True,
        Convention => C,
        External_Name => "ncreader_contents";

   procedure ncreader_destroy (n : access ncreader; contents : System.Address)  -- /usr/local/include/notcurses/notcurses.h:3722
   with Import => True,
        Convention => C,
        External_Name => "ncreader_destroy";

   procedure notcurses_debug (nc : access constant notcurses; debugfp : File_Pointer)  -- /usr/local/include/notcurses/notcurses.h:3727
   with Import => True,
        Convention => C,
        External_Name => "notcurses_debug";

   procedure notcurses_debug_caps (nc : access constant notcurses; debugfp : File_Pointer)  -- /usr/local/include/notcurses/notcurses.h:3732
   with Import => True,
        Convention => C,
        External_Name => "notcurses_debug_caps";

   procedure cell_init (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3738
   with Import => True,
        Convention => C,
        External_Name => "cell_init";

   function cell_load
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/notcurses.h:3742
   with Import => True,
        Convention => C,
        External_Name => "cell_load";

   function cell_prime
     (n : access ncplane;
      c : access nccell;
      gcluster : Interfaces.C.Strings.chars_ptr;
      stylemask : Unsigned_32;
      channels : Unsigned_64) return int  -- /usr/local/include/notcurses/notcurses.h:3746
   with Import => True,
        Convention => C,
        External_Name => "cell_prime";

   function cell_duplicate
     (n : access ncplane;
      targ : access nccell;
      c : access constant nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3751
   with Import => True,
        Convention => C,
        External_Name => "cell_duplicate";

   procedure cell_release (n : access ncplane; c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3753
   with Import => True,
        Convention => C,
        External_Name => "cell_release";

   function ncvisual_default_blitter (utf8 : Extensions.bool; scale : ncscale_e) return ncblitter_e  -- /usr/local/include/notcurses/notcurses.h:3758
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_default_blitter";

   procedure cell_set_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3773
   with Import => True,
        Convention => C,
        External_Name => "cell_set_styles";

   function cell_styles (c : access constant nccell) return unsigned  -- /usr/local/include/notcurses/notcurses.h:3779
   with Import => True,
        Convention => C,
        External_Name => "cell_styles";

   procedure cell_on_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3784
   with Import => True,
        Convention => C,
        External_Name => "cell_on_styles";

   procedure cell_off_styles (c : access nccell; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3789
   with Import => True,
        Convention => C,
        External_Name => "cell_off_styles";

   procedure cell_set_fg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3794
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_default";

   procedure cell_set_bg_default (c : access nccell)  -- /usr/local/include/notcurses/notcurses.h:3799
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_default";

   function cell_set_fg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3804
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_alpha";

   function cell_set_bg_alpha (c : access nccell; alpha : int) return int  -- /usr/local/include/notcurses/notcurses.h:3809
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_alpha";

   function cell_double_wide_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3814
   with Import => True,
        Convention => C,
        External_Name => "cell_double_wide_p";

   function cell_wide_right_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3819
   with Import => True,
        Convention => C,
        External_Name => "cell_wide_right_p";

   function cell_wide_left_p (c : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3824
   with Import => True,
        Convention => C,
        External_Name => "cell_wide_left_p";

   function cell_extended_gcluster (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3829
   with Import => True,
        Convention => C,
        External_Name => "cell_extended_gcluster";

   function cell_strdup (n : access constant ncplane; c : access constant nccell) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3832
   with Import => True,
        Convention => C,
        External_Name => "cell_strdup";

   function cell_extract
     (n : access constant ncplane;
      c : access constant nccell;
      stylemask : access Unsigned_16;
      channels : access Unsigned_64) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/notcurses.h:3837
   with Import => True,
        Convention => C,
        External_Name => "cell_extract";

   function cellcmp
     (n1 : access constant ncplane;
      c1 : access constant nccell;
      n2 : access constant ncplane;
      c2 : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3843
   with Import => True,
        Convention => C,
        External_Name => "cellcmp";

   function cell_load_char
     (n : access ncplane;
      c : access nccell;
      ch : char) return int  -- /usr/local/include/notcurses/notcurses.h:3849
   with Import => True,
        Convention => C,
        External_Name => "cell_load_char";

   function cell_load_egc32
     (n : access ncplane;
      c : access nccell;
      egc : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3854
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
      name : Interfaces.C.Strings.chars_ptr) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3860
   with Import => True,
        Convention => C,
        External_Name => "ncplane_new";

   function cell_fg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3864
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_rgb";

   function cell_bg_rgb (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3869
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_rgb";

   function cell_fg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3874
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_alpha";

   function cell_bg_alpha (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3879
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_alpha";

   function cell_fg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3884
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_rgb8";

   function cell_bg_rgb8
     (cl : access constant nccell;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3890
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_rgb8";

   function cell_set_fg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:3895
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb8";

   procedure cell_set_fg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:3900
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb8_clipped";

   function cell_set_fg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3905
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_rgb";

   function cell_set_fg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:3910
   with Import => True,
        Convention => C,
        External_Name => "cell_set_fg_palindex";

   function cell_fg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3915
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_palindex";

   function cell_set_bg_rgb8
     (cl : access nccell;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:3920
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb8";

   procedure cell_set_bg_rgb8_clipped
     (cl : access nccell;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:3925
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb8_clipped";

   function cell_set_bg_rgb (c : access nccell; channel : Unsigned_32) return int  -- /usr/local/include/notcurses/notcurses.h:3930
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_rgb";

   function cell_set_bg_palindex (cl : access nccell; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:3935
   with Import => True,
        Convention => C,
        External_Name => "cell_set_bg_palindex";

   function cell_bg_palindex (cl : access constant nccell) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3940
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_palindex";

   function cell_fg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3945
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_default_p";

   function cell_fg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3950
   with Import => True,
        Convention => C,
        External_Name => "cell_fg_palindex_p";

   function cell_bg_default_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3955
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_default_p";

   function cell_bg_palindex_p (cl : access constant nccell) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:3960
   with Import => True,
        Convention => C,
        External_Name => "cell_bg_palindex_p";

   procedure ncplane_styles_set (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3964
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles_set";

   procedure ncplane_styles_on (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3966
   with Import => True,
        Convention => C,
        External_Name => "ncplane_styles_on";

   procedure ncplane_styles_off (n : access ncplane; stylebits : unsigned)  -- /usr/local/include/notcurses/notcurses.h:3968
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
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3972
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
      vl : access nccell) return int  -- /usr/local/include/notcurses/notcurses.h:3977
   with Import => True,
        Convention => C,
        External_Name => "cells_double_box";

   function ncplane_rgba
     (n : access constant ncplane;
      blit : ncblitter_e;
      begy : int;
      begx : int;
      leny : int;
      lenx : int) return access Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:3984
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
      scalex : access int) return int  -- /usr/local/include/notcurses/notcurses.h:3990
   with Import => True,
        Convention => C,
        External_Name => "ncvisual_geom";

   function nctablet_ncplane (t : access nctablet) return access ncplane  -- /usr/local/include/notcurses/notcurses.h:3997
   with Import => True,
        Convention => C,
        External_Name => "nctablet_ncplane";

   function palette256_new (nc : access notcurses) return access ncpalette  -- /usr/local/include/notcurses/notcurses.h:4000
   with Import => True,
        Convention => C,
        External_Name => "palette256_new";

   function palette256_use (nc : access notcurses; p : access constant ncpalette) return int  -- /usr/local/include/notcurses/notcurses.h:4003
   with Import => True,
        Convention => C,
        External_Name => "palette256_use";

   function palette256_set_rgb8
     (p : access ncpalette;
      idx : int;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4007
   with Import => True,
        Convention => C,
        External_Name => "palette256_set_rgb8";

   function palette256_set
     (p : access ncpalette;
      idx : int;
      rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4012
   with Import => True,
        Convention => C,
        External_Name => "palette256_set";

   function palette256_get_rgb8
     (p : access constant ncpalette;
      idx : int;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4017
   with Import => True,
        Convention => C,
        External_Name => "palette256_get_rgb8";

   procedure palette256_free (p : access ncpalette)  -- /usr/local/include/notcurses/notcurses.h:4021
   with Import => True,
        Convention => C,
        External_Name => "palette256_free";

   function channel_r (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4024
   with Import => True,
        Convention => C,
        External_Name => "channel_r";

   function channel_g (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4030
   with Import => True,
        Convention => C,
        External_Name => "channel_g";

   function channel_b (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4036
   with Import => True,
        Convention => C,
        External_Name => "channel_b";

   function channel_rgb8
     (channel : Unsigned_32;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4042
   with Import => True,
        Convention => C,
        External_Name => "channel_rgb8";

   function channel_set_rgb8
     (channel : access Unsigned_32;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4050
   with Import => True,
        Convention => C,
        External_Name => "channel_set_rgb8";

   procedure channel_set_rgb8_clipped
     (channel : access unsigned;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4058
   with Import => True,
        Convention => C,
        External_Name => "channel_set_rgb8_clipped";

   function channel_set (channel : access unsigned; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4064
   with Import => True,
        Convention => C,
        External_Name => "channel_set";

   function channel_alpha (channel : unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4070
   with Import => True,
        Convention => C,
        External_Name => "channel_alpha";

   function channel_palindex (channel : Unsigned_32) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4075
   with Import => True,
        Convention => C,
        External_Name => "channel_palindex";

   function channel_set_alpha (channel : access unsigned; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4081
   with Import => True,
        Convention => C,
        External_Name => "channel_set_alpha";

   function channel_set_palindex (channel : access Unsigned_32; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4086
   with Import => True,
        Convention => C,
        External_Name => "channel_set_palindex";

   function channel_default_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4091
   with Import => True,
        Convention => C,
        External_Name => "channel_default_p";

   function channel_palindex_p (channel : unsigned) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4096
   with Import => True,
        Convention => C,
        External_Name => "channel_palindex_p";

   function channel_set_default (channel : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4101
   with Import => True,
        Convention => C,
        External_Name => "channel_set_default";

   function channels_bchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4106
   with Import => True,
        Convention => C,
        External_Name => "channels_bchannel";

   function channels_fchannel (channels : Unsigned_64) return Unsigned_32  -- /usr/local/include/notcurses/notcurses.h:4111
   with Import => True,
        Convention => C,
        External_Name => "channels_fchannel";

   function channels_set_bchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4116
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bchannel";

   function channels_set_fchannel (channels : access Unsigned_64; channel : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4121
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fchannel";

   function channels_combine (fchan : Unsigned_32; bchan : Unsigned_32) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4126
   with Import => True,
        Convention => C,
        External_Name => "channels_combine";

   function channels_fg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4131
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_palindex";

   function channels_bg_palindex (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4136
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_palindex";

   function channels_fg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4141
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_rgb";

   function channels_bg_rgb (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4146
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_rgb";

   function channels_fg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4151
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_alpha";

   function channels_bg_alpha (channels : Unsigned_64) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4156
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_alpha";

   function channels_fg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4161
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_rgb8";

   function channels_bg_rgb8
     (channels : Unsigned_64;
      r : access unsigned;
      g : access unsigned;
      b : access unsigned) return unsigned  -- /usr/local/include/notcurses/notcurses.h:4166
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_rgb8";

   function channels_set_fg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4171
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb8";

   procedure channels_set_fg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4176
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb8_clipped";

   function channels_set_fg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4181
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_alpha";

   function channels_set_fg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4186
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_palindex";

   function channels_set_fg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4191
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_rgb";

   function channels_set_bg_rgb8
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int) return int  -- /usr/local/include/notcurses/notcurses.h:4196
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb8";

   procedure channels_set_bg_rgb8_clipped
     (channels : access Unsigned_64;
      r : int;
      g : int;
      b : int)  -- /usr/local/include/notcurses/notcurses.h:4201
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb8_clipped";

   function channels_set_bg_alpha (channels : access Unsigned_64; alpha : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4206
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_alpha";

   function channels_set_bg_palindex (channels : access Unsigned_64; idx : int) return int  -- /usr/local/include/notcurses/notcurses.h:4211
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_palindex";

   function channels_set_bg_rgb (channels : access Unsigned_64; rgb : unsigned) return int  -- /usr/local/include/notcurses/notcurses.h:4216
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_rgb";

   function channels_fg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4221
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_default_p";

   function channels_fg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4226
   with Import => True,
        Convention => C,
        External_Name => "channels_fg_palindex_p";

   function channels_bg_default_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4231
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_default_p";

   function channels_bg_palindex_p (channels : Unsigned_64) return Extensions.bool  -- /usr/local/include/notcurses/notcurses.h:4236
   with Import => True,
        Convention => C,
        External_Name => "channels_bg_palindex_p";

   function channels_set_fg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4241
   with Import => True,
        Convention => C,
        External_Name => "channels_set_fg_default";

   function channels_set_bg_default (channels : access Unsigned_64) return Unsigned_64  -- /usr/local/include/notcurses/notcurses.h:4246
   with Import => True,
        Convention => C,
        External_Name => "channels_set_bg_default";

   subtype palette256 is ncpalette;  -- /usr/local/include/notcurses/notcurses.h:4250

   subtype cell is nccell;  -- /usr/local/include/notcurses/notcurses.h:4252

end Notcurses_Thin;
