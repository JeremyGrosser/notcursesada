with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with System;

package NC is
   type Blitter is
      (Blit_Default,
       Blit_1x1,
       Blit_2x1,
       Blit_2x2,
       Blit_3x2,
       Blit_Braille,
       Blit_Pixel,
       Blit_4x1,
       Blit_8x1)
   with Convention => C;

   type Scale is
     (Scale_None,
      Scale_Scale,
      Scale_Stretch,
      Scale_None_Hires,
      Scale_Scale_Hires)
   with Convention => C;

   --  These log levels consciously map cleanly to those of libav; Notcurses
   --  itself does not use this full granularity. The log level does not affect
   --  the opening and closing banners, which can be disabled via the
   --  notcurses_option struct's 'suppress_banner'. Note that if stderr is
   --  connected to the same terminal on which we're rendering, any kind of
   --  logging will disrupt the output (which is undesirable). The "default"
   --  zero value is NCLOGLEVEL_PANIC.
   type Options_Log_Level is (Silent, Panic, Fatal, Error, Warning, Info, Verbose, Debug, Trace)
      with Size => 32;
   for Options_Log_Level use
      (Silent  => -1, --  default. print nothing once fullscreen service begins
       Panic   => 0,  --  print diagnostics related to catastrophic failure
       Fatal   => 1,  --  we're hanging around, but we've had a horrible fault
       Error   => 2,  --  we can't keep doing this, but we can do other things
       Warning => 3,  --  you probably don't want what's happening to happen
       Info    => 4,  --  "standard information"
       Verbose => 5,  --  "detailed information"
       Debug   => 6,  --  this is honestly a bit much
       Trace   => 7); --  there's probably a better way to do what you want

   type Options_Flags is record
      Inhibit_Setlocale    : Boolean;
      --  notcurses_init() will call setlocale() to inspect the current locale.
      --  If that locale is "C" or "POSIX", it will call setlocale(LC_ALL, "")
      --  to set the locale according to the LANG environment variable.
      --  Ideally, this will result in UTF8 being enabled, even if the client
      --  app didn't call setlocale() itself. Unless you're certain that you're
      --  invoking setlocale() prior to notcurses_init(), you should not set
      --  this bit. Even if you are invoking setlocale(), this behavior
      --  shouldn't be an issue unless you're doing something weird (setting a
      --  locale not based on LANG).

      No_Clear_Bitmaps     : Boolean;
      --  We typically try to clear any preexisting bitmaps. If we ought *not*
      --  try to do this, pass NCOPTION_NO_CLEAR_BITMAPS. Note that they might
      --  still get cleared even if this is set, and they might not get cleared
      --  even if this is not set. It's a tough world out there.

      No_Winch_Sighandler  : Boolean;
      --  We typically install a signal handler for SIGWINCH that generates a
      --  resize event in the notcurses_get() queue. Set to inhibit this
      --  handler.

      No_Quit_Sighandlers  : Boolean;
      --  We typically install a signal handler for SIG{INT, ILL, SEGV, ABRT,
      --  TERM, QUIT} that restores the screen, and then calls the old signal
      --  handler. Set to inhibit registration of these signal handlers.

      Preserve_Cursor      : Boolean;
      --  Initialize the standard plane's virtual cursor to match the physical
      --  cursor at context creation time. Together with
      --  NCOPTION_NO_ALTERNATE_SCREEN and a scrolling standard plane, this
      --  facilitates easy scrolling-style programs in rendered mode.

      Suppress_Banners     : Boolean;
      --  Notcurses typically prints version info in notcurses_init() and
      --  performance info in notcurses_stop(). This inhibits that output.

      No_Alternate_Screen  : Boolean;
      --  If smcup/rmcup capabilities are indicated, Notcurses defaults to
      --  making use of the "alternate screen". This flag inhibits use of
      --  smcup/rmcup.

      No_Font_Changes      : Boolean;
      --  Do not modify the font. Notcurses might attempt to change the font
      --  slightly, to support certain glyphs (especially on the Linux
      --  console). If this is set, no such modifications will be made. Note
      --  that font changes will not affect anything but the virtual
      --  console/terminal in which Notcurses is running.

      Drain_Input          : Boolean;
      --  Input may be freely dropped. This ought be provided when the program
      --  does not intend to handle input. Otherwise, input can accumulate in
      --  internal buffers, eventually preventing Notcurses from processing
      --  terminal messages.
   end record
      with Size => 64;

   for Options_Flags use record
      Inhibit_Setlocale    at 0 range 0 .. 0;
      No_Clear_Bitmaps     at 0 range 1 .. 1;
      No_Winch_Sighandler  at 0 range 2 .. 2;
      No_Quit_Sighandlers  at 0 range 3 .. 3;
      Preserve_Cursor      at 0 range 4 .. 4;
      Suppress_Banners     at 0 range 5 .. 5;
      No_Alternate_Screen  at 0 range 6 .. 6;
      No_Font_Changes      at 0 range 7 .. 7;
      Drain_Input          at 0 range 8 .. 8;
   end record;

   CLI_Mode_Flags : constant Options_Flags :=
      (No_Clear_Bitmaps    => True,
       No_Alternate_Screen => True,
       Preserve_Cursor     => True,
       others              => False);

   type Options is record
      Terminal_Type : chars_ptr := Null_Ptr;
      --  The name of the terminfo database entry describing this terminal. If
      --  null, the environment variable TERM is used. Failure to open the
      --  terminal definition will result in failure to initialize notcurses.

      Log_Level : Options_Log_Level := Silent;
      --  Progressively higher log levels result in more logging to stderr. By
      --  default, nothing is printed to stderr once fullscreen service begins.

      Margin_Top, Margin_Right, Margin_Bottom, Margin_Left : unsigned := 0;
      --  Desirable margins. If all are 0 (default), we will render to the entirety
      --  of the screen. If the screen is too small, we do what we can--this is
      --  strictly best-effort. Absolute coordinates are relative to the
      --  rendering area ((0, 0) is always the origin of the rendering area).

      Flags : Options_Flags := (others => False);
   end record
      with Convention => C_Pass_By_Copy;

   type Status_Code is new Interfaces.C.int;
   function Is_Error (SC : Status_Code)
      return Boolean;

   type Context is private;

   function Core_Init
      (Opts : access Options;
       Fp   : FILEs := stdout)
       return access Context
   with Import, Convention => C, External_Name => "notcurses_core_init";
   --  The same as notcurses_init(), but without any multimedia functionality,
   --  allowing for a svelter binary. Link with notcurses-core if this is used.

   function Stop
      (NC : access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_stop";
   --  Destroy a Notcurses context. A null NC is a no-op.

   function Enter_Alternate_Screen
      (NC : not null access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_enter_alternate_screen";
   --  Shift to the alternate screen, if available. If already using the
   --  alternate screen, this returns 0 immediately. If the alternate screen is
   --  not available, this returns -1 immediately. Entering the alternate
   --  screen turns off scrolling for the standard plane.

   function Leave_Alternate_Screen
      (NC : not null access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_leave_alternate_screen";
   --  Exit the alternate screen. Immediately returns 0 if not currently using
   --  the alternate screen.

   type Plane is private;

   function Standard_Plane
      (NC : not null access Context)
      return access Plane
   with Import, Convention => C, External_Name => "notcurses_stdplane";
   --  Get a reference to the standard plane (one matching our current idea of
   --  the terminal size) for this terminal. The standard plane always exists,
   --  and its origin is always at the uppermost, leftmost cell of the
   --  terminal.

   function Pile_Top
      (N : not null access Plane)
      return access Plane
   with Import, Convention => C, External_Name => "ncpile_top";
   --  Return the topmost plane of the pile containing N.

   function Pile_Bottom
      (N : not null access Plane)
      return access Plane
   with Import, Convention => C, External_Name => "ncpile_bottom";
   --  Return the bottommost plane of the pile containing N.

   function Top
      (NC : not null access Context)
      return access Plane;
   --  Return the topmost plane of the standard pile.

   function Bottom
      (NC : not null access Context)
      return access Plane;
   --  Return the bottommost plane of the standard pile.

   function Pile_Render
      (N : not null access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncpile_render";
   --  Renders the pile of which N is a part. Rendering this pile again will
   --  blow away the render. To actually write out the render, call
   --  Pile_Rasterize.

   function Pile_Rasterize
      (N : not null access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncpile_rasterize";
   --  Make the physical screen match the last rendered frame from the pile of
   --  which N is a part. This is a blocking call. Don't call this before the
   --  pile has been rendered (doing so will likely result in a blank screen).

   function Render
      (NC : not null access Context)
      return Status_Code;
   --  Renders and rasterizes the standard pile in one shot. Blocking call.

   function Pile_Render_To_Buffer
      (P   : not null access Plane;
       Buf : not null char_array_access;
       Len : not null access Interfaces.C.size_t)
       return Status_Code
   with Import, Convention => C, External_Name => "ncpile_render_to_buffer";
   --  Perform the rendering and rasterization portion of Pile_Render and
   --  Pile_Rasterize, but do not write the resulting buffer out to the
   --  terminal. Using this function, the user can control the writeout
   --  process. The returned buffer must be freed by the caller.

   function Pile_Render_To_File
      (P  : not null access Plane;
       Fp : not null access FILEs)
       return Status_Code
   with Import, Convention => C, External_Name => "ncpile_render_to_file";
   --  Write the last rendered frame, in its entirety, to Fp. If a frame has
   --  not yet been rendered, nothing will be written.

   procedure Drop_Planes
      (NC : not null access Context)
   with Import, Convention => C, External_Name => "notcurses_drop_planes";
   --  Destroy all ncplanes other than the stdplane.

   --  All input is taken from stdin. We attempt to read a single UTF8-encoded
   --  Unicode codepoint, *not* an entire Extended Grapheme Cluster. It is also
   --  possible that we will read a special keypress, i.e. anything that doesn't
   --  correspond to a Unicode codepoint (e.g. arrow keys, function keys, screen
   --  resize events, etc.). These are mapped into a Unicode's area beyond the
   --  17 65536-entry Planes, starting at U+1115000. See <notcurses/nckeys.h>.

   --  notcurses_get_nblock() is nonblocking. notcurses_get_blocking() blocks
   --  until a codepoint or special key is read, or until interrupted by a signal.
   --  notcurses_get() allows an optional timeout to be controlled.

   --  In the case of a valid read, a 32-bit Unicode codepoint is returned. 0 is
   --  returned to indicate that no input was available. Otherwise (including on
   --  EOF) (uint32_t)-1 is returned.

   type Input_Event_Type is (Unknown, Press, Repeat, Release)
      with Convention => C;

   type Input is record
      Id       : Unsigned_32;       --  Unicode codepoint or synthesized NCKEY event
      Y, X     : Integer;           --  y/x cell coordinate of event, -1 for undefined
      UTF8     : String (1 .. 5);   --  UTF8 representation, if one exists
      Alt      : Boolean;           --  Was Alt held?
      Shift    : Boolean;           --  Was Shift held?
      Ctrl     : Boolean;           --  Was Ctrl held?
      Evtype   : Input_Event_Type;
      Ypx, Xpx : Integer;           --  pixel offsets within cell, -1 for undefined
   end record
      with Convention => C_Pass_By_Copy;

   type Timespec is record
      tv_sec  : Interfaces.C.long;
      tv_nsec : Interfaces.C.long;
   end record
      with Convention => C;
   --  Use Ada.Calendar.Conversions.To_Struct_Timespec to populate this record.
   --  TODO: This seems non-portable and maybe broken.

   function Get
      (NC : not null access Context;
       Ts : access Timespec;
       Ni : access Input)
       return Unsigned_32
   with Import, Convention => C, External_Name => "notcurses_get";
   --  UTF-32-encoded Unicode codepoint from input. This might only be part
   --  larger EGC. Provide a NULL 'ts' to block at length, and otherwise a
   --  an absolute deadline calculated using CLOCK_MONOTONIC.  single
   --  Unicode code point, or a synthesized special key constant, on error.
   --  Returns 0 on a timeout. If an event is processed, value is the 'id'
   --  field from that event. 'ni' may be NULL.

   --  TODO: notcurses_getvec
   --        notcurses_inputready_fd
   --        notcurses_get_nblock
   --        notcurses_get_blocking

   function Has_Modifiers
      (Ni : not null access Input)
      return Boolean;

   pragma Warnings (Off, "24 bits of ""Mouse_Event_Mask"" unused");
   type Mouse_Event_Mask is record
      Move_Event     : Boolean;
      Button_Event   : Boolean;
      Drag_Event     : Boolean;
   end record
      with Convention => C_Pass_By_Copy,
           Size       => unsigned'Size;

   for Mouse_Event_Mask use record
      Move_Event     at 0 range 0 .. 0;
      Button_Event   at 0 range 1 .. 1;
      Drag_Event     at 0 range 2 .. 2;
   end record;

   function Mice_Enable
      (N    : not null access Context;
       Mask : Mouse_Event_Mask)
       return Status_Code
   with Import, Convention => C, External_Name => "notcurses_mice_enable";
   --  Enable mice events according to Mask. On failure, -1 is returned. On
   --  success, 0 is returned and mouse events will be published to Get

   function Mice_Disable
      (N : not null access Context)
       return Status_Code;
   --  Disable mouse events. Any events in the input queue can still be
   --  delivered.

   function Line_Signals_Disable
      (N : not null access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_linesigs_disable";
   --  Disable signals originating from the terminal's line discipline, i.e.
   --  SIGINT (^C), SIGQUIT (^\), and SIGTSTP (^Z). They are enabled by
   --  default.

   function Line_Signals_Enable
      (N : not null access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_linesigs_enable";
   --  Restore signals originating from the terminal's line discipline, i.e.
   --  SIGINT (^C), SIGQUIT (^\), and SIGTSTP (^Z), if disabled.

   function Refresh
      (N : not null access Context)
      return Status_Code
   with Import, Convention => C, External_Name => "notcurses_refresh";
   --  Refresh the physical screen to match what was last rendered (i.e.,
   --  without reflecting any changes since the last call to Render). This is
   --  primarily useful if the screen is externally corrupted, or if an
   --  NCKEY_RESIZE event has been read and you're not yet ready to render. The
   --  current screen geometry is returned in 'y' and 'x', if they are not
   --  null.

   function Get_Context
      (P : not null access Plane)
      return access Context
   with Import, Convention => C, External_Name => "ncplane_notcurses";
   --  Extract the Notcurses context to which this plane is attached.

   procedure Dimensions
      (P : not null access Plane;
       Y : access unsigned;
       X : access unsigned)
   with Import, Convention => C, External_Name => "ncplane_dim_yx";
   --  Return the dimensions of this Plane. Y or X may be null.

   --  TODO: ncplane_pixel_geom
   --        notcurses_at_yx

   type Resize_Callback is access function (N : access constant Plane) return Status_Code
      with Convention => C;

   type Plane_Options_Flags is record
      Horizontal_Aligned   : Boolean;
      --  Horizontal alignment relative to the parent plane. Use ncalign_e for 'x'.

      Vertical_Aligned     : Boolean;
      --  Vertical alignment relative to the parent plane. Use ncalign_e for 'y'.

      Marginalized         : Boolean;
      --  Maximize relative to the parent plane, modulo the provided margins.
      --  The margins are best-effort; the plane will always be at least 1
      --  column by 1 row. If the margins can be effected, the plane will be
      --  sized to all remaining space. 'y' and 'x' are overloaded as the top
      --  and left margins when this flag is used. 'rows' and 'cols' must be 0
      --  when this flag is used. This flag is exclusive with both of the
      --  alignment flags.

      Fixed                : Boolean;
      --  If this plane is bound to a scrolling plane, it ought *not* scroll
      --  along with the parent (it will still move with the parent,
      --  maintaining its relative position, if the parent is moved to a new
      --  location).
   end record
      with Size => 64;

   for Plane_Options_Flags use record
      Horizontal_Aligned   at 0 range 0 .. 0;
      Vertical_Aligned     at 0 range 1 .. 1;
      Marginalized         at 0 range 2 .. 2;
      Fixed                at 0 range 3 .. 3;
   end record;

   type Plane_Options is record
      Y, X           : Integer := 0;
      --  placement relative to parent plane

      Rows, Columns  : Natural := 0;
      --  must be > 0 unless Flags.Marginalized

      User_Pointer   : Integer := 0;
      --  user curry, may be Null_Address

      Name           : chars_ptr := Null_Ptr;
      --  name (used only for debugging)

      Resize         : Resize_Callback := null;
      --  callback when parent is resized

      Flags          : Plane_Options_Flags := (others => False);

      Margin_Bottom  : Natural := 0;
      Margin_Right   : Natural := 0;
      --  margins (requires Flags.Marginalized)
   end record
      with Convention => C_Pass_By_Copy;

   function Plane_Create
      (N    : not null access Plane;
       Opts : not null access constant Plane_Options)
       return access Plane
   with Import,
        Convention    => C,
        External_Name => "ncplane_create",
        Pre => Opts.all.Rows > 0 and Opts.all.Columns > 0;
   --  Create a new Plane bound to plane N, at the offset Y, X (relative to the
   --  origin of N) and the specified size.  This plane is initially at the top
   --  of the z-buffer, as if ncplane_move_top() had been called on it. The
   --  User_Pointer can be retrieved (and reset) later. A Name can be set, used
   --  in debugging.

   function Pile_Create
      (N    : not null access Plane;
       Opts : not null access constant Plane_Options)
       return access Plane
   with Import,
        Convention    => C,
        External_Name => "ncplane_create",
        Pre => Opts.all.Rows > 0 and Opts.all.Columns > 0;
   --  Same as Plane_Create, but creates a new pile. The returned plane will be
   --  the top, bottom, and root of this new pile.

   --  Utility resize callbacks. When a parent plane is resized, it invokes
   --  each child's resize callback. Any logic can be run in a resize callback,
   --  but these are some generically useful ones.

   function Plane_Resize_Maximize
      (N : access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncplane_resize_maximize";
   --  resize the plane to the visual region's size (used for the standard plane).

   function Plane_Resize_Marginalized
      (N : access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncplane_resize_marginalized";
   --  resize the plane to its parent's size, attempting to enforce the margins
   --  supplied along with NCPLANE_OPTION_MARGINALIZED.

   function Plane_Resize_Realign
      (N : access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncplane_resize_realign";
   --  realign the plane 'n' against its parent, using the alignments specified
   --  with NCPLANE_OPTION_HORALIGNED and/or NCPLANE_OPTION_VERALIGNED.

   function Plane_Resize_Placewithin
      (N : access Plane)
      return Status_Code
   with Import, Convention => C, External_Name => "ncplane_resize_placewithin";
   --  move the plane such that it is entirely within its parent, if possible.
   --  no resizing is performed.

   procedure Plane_Set_Resize_Callback
      (N        : access Plane;
       Callback : Resize_Callback)
   with Import, Convention => C, External_Name => "ncplane_set_resizecb";
   --  Replace the Plane's existing resize callback with Callback (which may be
   --  null). The standard plane's resize callback may not be changed.

   function Plane_Resize_Callback
      (N : access constant Plane)
      return Resize_Callback
   with Import, Convention => C, External_Name => "ncplane_resizecb";
   --  Returns the Plane's current resize callback.

   function Plane_Set_Name
      (N    : not null access Plane;
       Name : chars_ptr)
       return Status_Code
   with Import, Convention => C, External_Name => "ncplane_set_name";
   --  Set the plane's name (may be null), replacing any current name.

   function Plane_Name
      (N : not null access constant Plane)
      return chars_ptr
   with Import, Convention => C, External_Name => "ncplane_name";
   --  Return a heap-allocated copy of the plane's name, or null if it has
   --  none.

   function Plane_Reparent
      (N          : not null access Plane;
       New_Parent : not null access Plane)
       return access Plane
   with Import, Convention => C, External_Name => "ncplane_reparent";
   --  Plane N will be unbound from its parent plane, and will be made a bound
   --  child of New_Parent. If New_Parent is equal to N, N becomes the root of
   --  a new pile, unless N is already the root of a pile, in which case this
   --  is a no-op. Returns N. The standard plane cannot be reparented. Any
   --  planes bound to N are reparented to the previous parent of N.

   function Plane_Reparent_Family
      (N          : not null access Plane;
       New_Parent : not null access Plane)
       return access Plane
   with Import, Convention => C, External_Name => "ncplane_reparent_family";
   --  The same as Plane_Reparent, except any planes bound to N come along with
   --  it to its new destination. Their z-order is maintained. If New_Parent is
   --  an ancestor of N, null is returned, and no changes are made.

   function Plane_Duplicate
      (N      : not null access constant Plane;
       Opaque : System.Address)
       return access Plane
   with Import, Convention => C, External_Name => "ncplane_dup";
   --  Duplicate an existing Plane. The new plane will have the same
   --  geometry, will duplicate all content, and will start with the same
   --  rendering state. The new plane will be immediately above the old one on
   --  the z axis, and will be bound to the same parent (unless N is a root
   --  plane, in which case the new plane will be bound to it). Bound planes
   --  are *not* duplicated; the new plane is bound to the parent of N, but has
   --  no bound planes.

   procedure Plane_Translate
      (Src  : not null access constant Plane;
       Dst  : access Plane;
       Y, X : access Interfaces.C.int)
   with Import, Convention => C, External_Name => "ncplane_translate";
   --  provided a coordinate relative to the origin of Src, map it to the same
   --  absolute coordinate relative to the origin of Dst. either or both of Y
   --  and X may be null. if Dst is null, it is taken to be the standard plane.

   type C_bool is new Boolean
      with Convention => C;
   --  C99 says the size of bool is implementation defined. Ada defines it as
   --  8-bits wide. libgnat defines it like this, but for some reason
   --  doesn't export the symbol. Hopefully this gets us close to whatever the
   --  system's C compiler does.

   function Plane_Translate_Absolute
      (N    : not null access constant Plane;
       Y, X : access Interfaces.C.int)
       return C_bool
   with Import, Convention => C, External_Name => "ncplane_translate_abs";
   --  Fed absolute Y/X coordinates, determine whether that coordinate is
   --  within the ncplane N. If not, return False. If so, return True. Either
   --  way, translate the absolute coordinates relative to N. If the point is
   --  not within N, these coordinates will not be within the dimensions of the
   --  plane.

   function Plane_Set_Scrolling
      (N       : not null access Plane;
       Scrollp : Interfaces.C.unsigned)
       return C_bool
   with Import, Convention => C, External_Name => "ncplane_set_scrolling";
   --  All planes are created with scrolling disabled. Scrolling can be
   --  dynamically controlled with Plane_Set_Scrolling. Returns True if
   --  scrolling was previously enabled, or False if it was disabled.

   function Plane_Scrolling
      (N : not null access Plane)
      return C_bool
   with Import, Convention => C, External_Name => "ncplane_scrolling_p";

   function Plane_Set_Autogrow
      (N     : not null access Plane;
       Growp : Interfaces.C.unsigned)
       return C_bool
   with Import, Convention => C, External_Name => "ncplane_set_autogrow";

   function Plane_Autogrow
      (N : not null access Plane)
      return C_bool
   with Import, Convention => C, External_Name => "ncplane_autogrow";


   --  Palette API. Some terminals only support 256 colors, but allow the full
   --  palette to be specified with arbitrary RGB colors. In all cases, it's
   --  more performant to use indexed colors, since it's much less data to
   --  write to the terminal. If you can limit yourself to 256 colors, that's
   --  probably best.
   type Alpha is mod 2 ** 2
      with Size => 2;
   type Color is mod 2 ** 8
      with Size => 8;

   type Channel is record
      A       : Alpha;
      R, G, B : Color;
   end record
      with Size => 32;

   Max_Palette_Size : constant := 256;
   --  we support palette-indexed color up to 8 bits

   type Palette_Channels is array (1 .. Max_Palette_Size) of Channel;

   type Palette is record
      Channels : Palette_Channels;
   end record
      with Convention => C;

   function Palette_New
      (NC : not null access Context)
      return access Palette
   with Import, Convention => C, External_Name => "ncpalette_new";
   --  Create a new palette store. It will be initialized with notcurses' best
   --  knowledge of the currently configured palette. The palette upon startup
   --  cannot be reliably detected, sadly.

   function Palette_Use
      (NC : not null access Context;
       P  : not null access constant Palette)
       return Status_Code
   with Import, Convention => C, External_Name => "ncpalette_use";
   --  Attempt to configure the terminal with the provided palette 'p'. Does
   --  not transfer ownership of P; Palette_Free can (ought) still be called.

   procedure Palette_Free
      (P : access Palette)
   with Import, Convention => C, External_Name => "ncpalette_free";

   --  Capabilities, derived from terminfo, environment variables, and queries
   type Capabilities is record
      Colors            : Unsigned; --  size of palette for indexed colors
      UTF8              : Boolean;  --  are we using utf-8 encoding? from nl_langinfo(3)
      RGB               : Boolean;  --  24bit color? COLORTERM/heuristics/terminfo 'rgb'
      Can_Change_Colors : Boolean;  --  can we change the palette? terminfo 'ccc'
      --  these are assigned wholly through TERM- and query-based heuristics
      Halfblocks        : Boolean;  --  we assume halfblocks, but some are known to lack them
      Quadrants         : Boolean;  --  do we have (good, vetted) Unicode 1 quadrant support?
      Sextants          : Boolean;  --  do we have (good, vetted) Unicode 13 sextant support?
      Braille           : Boolean;  --  do we have Braille support? (linux console does not)
   end record;

   type Style is record
      Italic    : Boolean;
      Underline : Boolean;
      Undercurl : Boolean;
      Bold      : Boolean;
      Struck    : Boolean;
   end record
      with Size => 16;

   for Style use record
      Italic      at 0 range 4 .. 4;
      Underline   at 0 range 3 .. 3;
      Undercurl   at 0 range 2 .. 2;
      Bold        at 0 range 1 .. 1;
      Struck      at 0 range 0 .. 0;
   end record;

   function Supported_Styles
      (NC : not null access constant Context)
      return Style
   with Import, Convention => C, External_Name => "notcurses_supported_styles";
   --  Each attribute is only indicated as supported if the terminal can
   --  support it together with color. For more information, see the "ncv"
   --  capability in terminfo(5).

   function Palette_Size
      (NC : not null access constant Context)
      return Unsigned
   with Import, Convention => C, External_Name => "notcurses_palette_size";
   --  Returns the number of simultaneous colors claimed to be supported, or 1
   --  if there is no color support. Note that several terminal emulators
   --  advertise more colors than they actually support, downsampling
   --  internally.

   function Detected_Terminal
      (NC : not null access constant Context)
      return chars_ptr
   with Import, Convention => C, External_Name => "notcurses_detected_terminal";
   --  Returns the name (and sometimes version) of the terminal, as Notcurses
   --  has been best able to determine.

   function Detected_Capabilities
      (NC : not null access constant Context)
      return access constant Capabilities
   with Import, Convention => C, External_Name => "notcurses_capabilities";

private

   type Context is null record;
   type Plane is null record;

end NC;
