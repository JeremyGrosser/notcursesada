--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Notcurses_Thin;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;
with System.OS_Interface;

package Notcurses_Direct_Thin is

   NCDIRECT_OPTION_INHIBIT_SETLOCALE : constant := 16#0001#;  --  /usr/local/include/notcurses/direct.h:23

   NCDIRECT_OPTION_INHIBIT_CBREAK : constant := 16#0002#;  --  /usr/local/include/notcurses/direct.h:27

   NCDIRECT_OPTION_NO_QUIT_SIGHANDLERS : constant := 16#0008#;  --  /usr/local/include/notcurses/direct.h:32

   subtype ncdirectv is Notcurses_Thin.ncplane;  -- /usr/local/include/notcurses/direct.h:10

   type ncdirect is null record;   -- incomplete struct

   function ncdirect_init
     (termtype : Interfaces.C.Strings.chars_ptr;
      fp : Notcurses_Thin.File_Pointer;
      flags : Unsigned_64) return access ncdirect  -- /usr/local/include/notcurses/direct.h:41
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_init";

   function ncdirect_core_init
     (termtype : Interfaces.C.Strings.chars_ptr;
      fp : Notcurses_Thin.File_Pointer;
      flags : Unsigned_64) return access ncdirect  -- /usr/local/include/notcurses/direct.h:45
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_core_init";

   function ncdirect_readline (nc : access ncdirect; prompt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/direct.h:51
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_readline";

   function ncdirect_set_fg_rgb (nc : access ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:56
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_rgb";

   function ncdirect_set_bg_rgb (nc : access ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:57
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_rgb";

   function ncdirect_fg_rgb (nc : access ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:63
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_fg_rgb";

   function ncdirect_bg_rgb (nc : access ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:71
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_bg_rgb";

   function ncdirect_set_fg_palindex (nc : access ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:75
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_palindex";

   function ncdirect_set_bg_palindex (nc : access ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:76
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_palindex";

   function ncdirect_fg_palindex (nc : access ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:82
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_fg_palindex";

   function ncdirect_bg_palindex (nc : access ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:90
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_bg_palindex";

   function ncdirect_palette_size (nc : access constant ncdirect) return unsigned  -- /usr/local/include/notcurses/direct.h:97
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_palette_size";

   function ncdirect_putstr
     (nc : access ncdirect;
      channels : Unsigned_64;
      utf8 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/direct.h:102
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_putstr";

   function ncdirect_printf_aligned
     (n : access ncdirect;
      y : int;
      align : Notcurses_Thin.ncalign_e;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/direct.h:106
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_printf_aligned";

   function ncdirect_flush (nc : access constant ncdirect) return int  -- /usr/local/include/notcurses/direct.h:111
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_flush";

   function ncdirect_set_bg_rgb8
     (nc : access ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:114
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_rgb8";

   function ncdirect_set_fg_rgb8
     (nc : access ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:122
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_rgb8";

   function ncdirect_fg_rgb8
     (nc : access ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:134
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_fg_rgb8";

   function ncdirect_bg_rgb8
     (nc : access ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:143
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_bg_rgb8";

   function ncdirect_set_fg_default (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:147
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_default";

   function ncdirect_set_bg_default (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:148
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_default";

   function ncdirect_fg_default (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:154
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_fg_default";

   function ncdirect_bg_default (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:162
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_bg_default";

   function ncdirect_dim_x (nc : access constant ncdirect) return int  -- /usr/local/include/notcurses/direct.h:167
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_dim_x";

   function ncdirect_dim_y (nc : access constant ncdirect) return int  -- /usr/local/include/notcurses/direct.h:168
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_dim_y";

   function ncdirect_set_styles (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:171
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_styles";

   function ncdirect_on_styles (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:172
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_on_styles";

   function ncdirect_off_styles (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:173
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_off_styles";

   function ncdirect_styles_set (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:176
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_styles_set";

   function ncdirect_styles_on (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:178
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_styles_on";

   function ncdirect_styles_off (n : access ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:180
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_styles_off";

   function ncdirect_cursor_move_yx
     (n : access ncdirect;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/direct.h:184
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_move_yx";

   function ncdirect_cursor_enable (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:185
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_enable";

   function ncdirect_cursor_disable (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:186
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_disable";

   function ncdirect_cursor_up (nc : access ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:187
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_up";

   function ncdirect_cursor_left (nc : access ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:188
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_left";

   function ncdirect_cursor_right (nc : access ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:189
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_right";

   function ncdirect_cursor_down (nc : access ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:190
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_down";

   function ncdirect_cursor_yx
     (n : access ncdirect;
      y : access int;
      x : access int) return int  -- /usr/local/include/notcurses/direct.h:195
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_yx";

   function ncdirect_cursor_push (n : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:199
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_push";

   function ncdirect_cursor_pop (n : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:200
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_pop";

   function ncdirect_render_image
     (n : access ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      align : Notcurses_Thin.ncalign_e;
      blitter : Notcurses_Thin.ncblitter_e;
      scale : Notcurses_Thin.ncscale_e) return int  -- /usr/local/include/notcurses/direct.h:206
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_render_image";

   function ncdirect_clear (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:211
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_clear";

   function ncdirect_canopen_images (n : access constant ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:214
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canopen_images";

   function ncdirect_canutf8 (n : access constant ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:217
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canutf8";

   function ncdirect_check_pixel_support (n : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:222
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_check_pixel_support";

   function ncdirect_hline_interp
     (n : access ncdirect;
      egc : Interfaces.C.Strings.chars_ptr;
      len : int;
      h1 : Unsigned_64;
      h2 : Unsigned_64) return int  -- /usr/local/include/notcurses/direct.h:229
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_hline_interp";

   function ncdirect_vline_interp
     (n : access ncdirect;
      egc : Interfaces.C.Strings.chars_ptr;
      len : int;
      h1 : Unsigned_64;
      h2 : Unsigned_64) return int  -- /usr/local/include/notcurses/direct.h:231
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_vline_interp";

   function ncdirect_box
     (n : access ncdirect;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64;
      wchars : access Wide_Character;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:238
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_box";

   function ncdirect_rounded_box
     (n : access ncdirect;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:243
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_rounded_box";

   function ncdirect_double_box
     (n : access ncdirect;
      ul : Unsigned_64;
      ur : Unsigned_64;
      ll : Unsigned_64;
      lr : Unsigned_64;
      ylen : int;
      xlen : int;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:248
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_double_box";

   function ncdirect_getc
     (n : access ncdirect;
      ts : access constant System.OS_Interface.timespec;
      sigmask : access System.OS_Interface.sigset_t;
      ni : access Notcurses_Thin.ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/direct.h:259
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_getc";

   function ncdirect_inputready_fd (n : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:266
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_inputready_fd";

   function ncdirect_getc_nblock (n : access ncdirect; ni : access Notcurses_Thin.ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/direct.h:271
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_getc_nblock";

   function ncdirect_getc_blocking (n : access ncdirect; ni : access Notcurses_Thin.ncinput) return Wide_Wide_Character  -- /usr/local/include/notcurses/direct.h:281
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_getc_blocking";

   function ncdirect_stop (nc : access ncdirect) return int  -- /usr/local/include/notcurses/direct.h:288
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_stop";

   function ncdirect_render_frame
     (n : access ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      blitter : Notcurses_Thin.ncblitter_e;
      scale : Notcurses_Thin.ncscale_e;
      maxy : int;
      maxx : int) return access ncdirectv  -- /usr/local/include/notcurses/direct.h:296
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_render_frame";

   function ncdirect_raster_frame
     (n : access ncdirect;
      ncdv : access ncdirectv;
      align : Notcurses_Thin.ncalign_e) return int  -- /usr/local/include/notcurses/direct.h:301
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_raster_frame";

   function ncdirect_stream
     (n : access ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      streamer : Notcurses_Thin.ncstreamcb;
      vopts : access Notcurses_Thin.ncvisual_options;
      curry : System.Address) return int  -- /usr/local/include/notcurses/direct.h:303
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_stream";

end Notcurses_Direct_Thin;
