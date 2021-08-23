pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C_Streams;
with Interfaces;
with Notcurses_Thin;
with System.OS_Interface;
with System;
with Interfaces.C.Extensions;

package Notcurses_Direct_Thin is

   NCDIRECT_OPTION_INHIBIT_SETLOCALE : constant := 16#0001#;  --  /usr/local/include/notcurses/direct.h:29

   NCDIRECT_OPTION_INHIBIT_CBREAK : constant := 16#0002#;  --  /usr/local/include/notcurses/direct.h:33

   NCDIRECT_OPTION_DRAIN_INPUT : constant := 16#0004#;  --  /usr/local/include/notcurses/direct.h:38

   NCDIRECT_OPTION_NO_QUIT_SIGHANDLERS : constant := 16#0008#;  --  /usr/local/include/notcurses/direct.h:43

   NCDIRECT_OPTION_VERBOSE : constant := 16#0010#;  --  /usr/local/include/notcurses/direct.h:46

   NCDIRECT_OPTION_VERY_VERBOSE : constant := 16#0020#;  --  /usr/local/include/notcurses/direct.h:50

   function ncdirect_init
     (termtype : Interfaces.C.Strings.chars_ptr;
      fp : access Interfaces.C_Streams.FILEs;
      flags : Interfaces.Unsigned_64) return access Notcurses_Thin.ncdirect  -- /usr/local/include/notcurses/direct.h:59
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_init";

   function ncdirect_core_init
     (termtype : Interfaces.C.Strings.chars_ptr;
      fp : access Interfaces.C_Streams.FILEs;
      flags : Interfaces.Unsigned_64) return access Notcurses_Thin.ncdirect  -- /usr/local/include/notcurses/direct.h:63
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_core_init";

   function ncdirect_readline (nc : access Notcurses_Thin.ncdirect; prompt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/direct.h:68
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_readline";

   function ncdirect_set_fg_rgb (nc : access Notcurses_Thin.ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:73
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_rgb";

   function ncdirect_set_bg_rgb (nc : access Notcurses_Thin.ncdirect; rgb : unsigned) return int  -- /usr/local/include/notcurses/direct.h:75
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_rgb";

   function ncdirect_set_fg_palindex (nc : access Notcurses_Thin.ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:78
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_palindex";

   function ncdirect_set_bg_palindex (nc : access Notcurses_Thin.ncdirect; pidx : int) return int  -- /usr/local/include/notcurses/direct.h:80
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_palindex";

   function ncdirect_palette_size (nc : access constant Notcurses_Thin.ncdirect) return unsigned  -- /usr/local/include/notcurses/direct.h:86
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_palette_size";

   function ncdirect_putstr
     (nc : access Notcurses_Thin.ncdirect;
      channels : Interfaces.Unsigned_64;
      utf8 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/local/include/notcurses/direct.h:92
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_putstr";

   function ncdirect_putegc
     (nc : access Notcurses_Thin.ncdirect;
      channels : Interfaces.Unsigned_64;
      utf8 : Interfaces.C.Strings.chars_ptr;
      sbytes : access int) return int  -- /usr/local/include/notcurses/direct.h:99
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_putegc";

   function ncdirect_printf_aligned
     (n : access Notcurses_Thin.ncdirect;
      y : int;
      align : Notcurses_Thin.ncalign_e;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/local/include/notcurses/direct.h:105
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_printf_aligned";

   function ncdirect_flush (nc : access constant Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:110
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_flush";

   function ncdirect_set_bg_rgb8
     (nc : access Notcurses_Thin.ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:114
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_rgb8";

   function ncdirect_set_fg_rgb8
     (nc : access Notcurses_Thin.ncdirect;
      r : unsigned;
      g : unsigned;
      b : unsigned) return int  -- /usr/local/include/notcurses/direct.h:122
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_rgb8";

   function ncdirect_set_fg_default (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:129
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_fg_default";

   function ncdirect_set_bg_default (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:131
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_bg_default";

   function ncdirect_dim_x (nc : access Notcurses_Thin.ncdirect) return unsigned  -- /usr/local/include/notcurses/direct.h:135
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_dim_x";

   function ncdirect_dim_y (nc : access Notcurses_Thin.ncdirect) return unsigned  -- /usr/local/include/notcurses/direct.h:136
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_dim_y";

   function ncdirect_supported_styles (nc : access constant Notcurses_Thin.ncdirect) return Interfaces.Unsigned_16  -- /usr/local/include/notcurses/direct.h:142
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_supported_styles";

   function ncdirect_set_styles (n : access Notcurses_Thin.ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:146
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_set_styles";

   function ncdirect_on_styles (n : access Notcurses_Thin.ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:148
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_on_styles";

   function ncdirect_off_styles (n : access Notcurses_Thin.ncdirect; stylebits : unsigned) return int  -- /usr/local/include/notcurses/direct.h:150
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_off_styles";

   function ncdirect_styles (n : access constant Notcurses_Thin.ncdirect) return Interfaces.Unsigned_16  -- /usr/local/include/notcurses/direct.h:152
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_styles";

   function ncdirect_cursor_move_yx
     (n : access Notcurses_Thin.ncdirect;
      y : int;
      x : int) return int  -- /usr/local/include/notcurses/direct.h:156
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_move_yx";

   function ncdirect_cursor_enable (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:158
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_enable";

   function ncdirect_cursor_disable (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:160
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_disable";

   function ncdirect_cursor_up (nc : access Notcurses_Thin.ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:162
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_up";

   function ncdirect_cursor_left (nc : access Notcurses_Thin.ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:164
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_left";

   function ncdirect_cursor_right (nc : access Notcurses_Thin.ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:166
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_right";

   function ncdirect_cursor_down (nc : access Notcurses_Thin.ncdirect; num : int) return int  -- /usr/local/include/notcurses/direct.h:168
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_down";

   function ncdirect_cursor_yx
     (n : access Notcurses_Thin.ncdirect;
      y : access unsigned;
      x : access unsigned) return int  -- /usr/local/include/notcurses/direct.h:174
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_yx";

   function ncdirect_cursor_push (n : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:179
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_push";

   function ncdirect_cursor_pop (n : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:182
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cursor_pop";

   function ncdirect_clear (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:186
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_clear";

   function ncdirect_capabilities (n : access constant Notcurses_Thin.ncdirect) return access constant Notcurses_Thin.nccapabilities  -- /usr/local/include/notcurses/direct.h:189
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_capabilities";

   function ncdirect_hline_interp
     (n : access Notcurses_Thin.ncdirect;
      egc : Interfaces.C.Strings.chars_ptr;
      len : unsigned;
      h1 : Interfaces.Unsigned_64;
      h2 : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/direct.h:197
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_hline_interp";

   function ncdirect_vline_interp
     (n : access Notcurses_Thin.ncdirect;
      egc : Interfaces.C.Strings.chars_ptr;
      len : unsigned;
      h1 : Interfaces.Unsigned_64;
      h2 : Interfaces.Unsigned_64) return int  -- /usr/local/include/notcurses/direct.h:201
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_vline_interp";

   function ncdirect_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      wchars : access wchar_t;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:209
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_box";

   function ncdirect_light_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:215
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_light_box";

   function ncdirect_heavy_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:222
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_heavy_box";

   function ncdirect_ascii_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:229
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_ascii_box";

   function ncdirect_rounded_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:236
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_rounded_box";

   function ncdirect_double_box
     (n : access Notcurses_Thin.ncdirect;
      ul : Interfaces.Unsigned_64;
      ur : Interfaces.Unsigned_64;
      ll : Interfaces.Unsigned_64;
      lr : Interfaces.Unsigned_64;
      ylen : unsigned;
      xlen : unsigned;
      ctlword : unsigned) return int  -- /usr/local/include/notcurses/direct.h:242
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_double_box";

   function ncdirect_get
     (n : access Notcurses_Thin.ncdirect;
      absdl : access constant System.OS_Interface.timespec;
      ni : access Notcurses_Thin.ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/direct.h:252
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_get";

   function ncdirect_inputready_fd (n : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:260
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_inputready_fd";

   function ncdirect_get_nblock (n : access Notcurses_Thin.ncdirect; ni : access Notcurses_Thin.ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/direct.h:266
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_get_nblock";

   function ncdirect_get_blocking (n : access Notcurses_Thin.ncdirect; ni : access Notcurses_Thin.ncinput) return Interfaces.Unsigned_32  -- /usr/local/include/notcurses/direct.h:274
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_get_blocking";

   function ncdirect_stop (nc : access Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:279
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_stop";

   subtype ncdirectv is Notcurses_Thin.ncplane;  -- /usr/local/include/notcurses/direct.h:281

   subtype ncdirectf is Notcurses_Thin.ncvisual;  -- /usr/local/include/notcurses/direct.h:282

   function ncdirect_render_image
     (n : access Notcurses_Thin.ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      align : Notcurses_Thin.ncalign_e;
      blitter : Notcurses_Thin.ncblitter_e;
      scale : Notcurses_Thin.ncscale_e) return int  -- /usr/local/include/notcurses/direct.h:288
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_render_image";

   function ncdirect_render_frame
     (n : access Notcurses_Thin.ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      blitter : Notcurses_Thin.ncblitter_e;
      scale : Notcurses_Thin.ncscale_e;
      maxy : int;
      maxx : int) return access ncdirectv  -- /usr/local/include/notcurses/direct.h:299
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_render_frame";

   function ncdirect_raster_frame
     (n : access Notcurses_Thin.ncdirect;
      ncdv : access ncdirectv;
      align : Notcurses_Thin.ncalign_e) return int  -- /usr/local/include/notcurses/direct.h:306
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_raster_frame";

   function ncdirectf_from_file (n : access Notcurses_Thin.ncdirect; filename : Interfaces.C.Strings.chars_ptr) return access ncdirectf  -- /usr/local/include/notcurses/direct.h:313
   with Import => True,
        Convention => C,
        External_Name => "ncdirectf_from_file";

   procedure ncdirectf_free (frame : access ncdirectf)  -- /usr/local/include/notcurses/direct.h:317
   with Import => True,
        Convention => C,
        External_Name => "ncdirectf_free";

   function ncdirectf_render
     (n : access Notcurses_Thin.ncdirect;
      frame : access ncdirectf;
      vopts : access constant Notcurses_Thin.ncvisual_options) return access ncdirectv  -- /usr/local/include/notcurses/direct.h:322
   with Import => True,
        Convention => C,
        External_Name => "ncdirectf_render";

   function ncdirectf_geom
     (n : access Notcurses_Thin.ncdirect;
      frame : access ncdirectf;
      vopts : access constant Notcurses_Thin.ncvisual_options;
      geom : access Notcurses_Thin.ncvgeom) return int  -- /usr/local/include/notcurses/direct.h:327
   with Import => True,
        Convention => C,
        External_Name => "ncdirectf_geom";

   function ncdirect_stream
     (n : access Notcurses_Thin.ncdirect;
      filename : Interfaces.C.Strings.chars_ptr;
      streamer : Notcurses_Thin.ncstreamcb;
      vopts : access Notcurses_Thin.ncvisual_options;
      curry : System.Address) return int  -- /usr/local/include/notcurses/direct.h:332
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_stream";

   function ncdirect_detected_terminal (n : access constant Notcurses_Thin.ncdirect) return Interfaces.C.Strings.chars_ptr  -- /usr/local/include/notcurses/direct.h:338
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_detected_terminal";

   function ncdirect_cantruecolor (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:343
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cantruecolor";

   function ncdirect_canchangecolor (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:349
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canchangecolor";

   function ncdirect_canfade (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:355
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canfade";

   function ncdirect_canopen_images (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:361
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canopen_images";

   function ncdirect_canopen_videos (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:367
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canopen_videos";

   function ncdirect_canutf8 (n : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:372
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canutf8";

   function ncdirect_check_pixel_support (n : access constant Notcurses_Thin.ncdirect) return int  -- /usr/local/include/notcurses/direct.h:376
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_check_pixel_support";

   function ncdirect_canhalfblock (nc : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:381
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canhalfblock";

   function ncdirect_canquadrant (nc : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:387
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canquadrant";

   function ncdirect_cansextant (nc : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:393
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_cansextant";

   function ncdirect_canbraille (nc : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:399
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canbraille";

   function ncdirect_canget_cursor (nc : access constant Notcurses_Thin.ncdirect) return Extensions.bool  -- /usr/local/include/notcurses/direct.h:405
   with Import => True,
        Convention => C,
        External_Name => "ncdirect_canget_cursor";

end Notcurses_Direct_Thin;
