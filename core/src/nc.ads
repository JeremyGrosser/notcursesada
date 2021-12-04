with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with Interfaces.C; use Interfaces.C;

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

private

   type Context is null record;
   type Plane is null record;

end NC;
