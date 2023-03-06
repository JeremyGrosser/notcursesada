--
--  Copyright (C) 2021-2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Unchecked_Conversion;
with Ada.Characters.Conversions;
with Interfaces.C_Streams;
with Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with System.OS_Interface; --  timespec
with System;

package body Notcurses is
   function Initialize
      (Term_Type     : String := "";
       Level         : Log_Level := Error;
       Margin_Top    : Natural := 0;
       Margin_Right  : Natural := 0;
       Margin_Bottom : Natural := 0;
       Margin_Left   : Natural := 0;
       Flags         : Option_Flags := (others => <>))
       return Context
   is
      use Interfaces.C.Strings;

      LC_ALL : constant int := 0;
      function setlocale
         (Category : int;
          Locale   : String)
          return chars_ptr
      with Import, Convention => C, External_Name => "setlocale";

      type Options is record
         termtype    : chars_ptr;
         loglevel    : Log_Level;
         T, R, B, L  : unsigned;
         flags       : Option_Flags;
      end record;

      function notcurses_core_init
         (opts : access constant Options;
          fp   : access Interfaces.C_Streams.FILEs)
          return Context
      with Import, Convention => C, External_Name => "notcurses_core_init";

      O : aliased constant Options :=
         (termtype => (if Term_Type = "" then Null_Ptr else New_String (Term_Type)),  --  leaks ¯\_(ツ)_/¯
          loglevel => Level,
          T => unsigned (Margin_Top),
          R => unsigned (Margin_Right),
          B => unsigned (Margin_Bottom),
          L => unsigned (Margin_Left),
          flags => Flags);
   begin
      if setlocale (LC_ALL, "") = Null_Ptr then
         raise Program_Error with "setlocale(LC_ALL, "") failed";
      end if;

      return notcurses_core_init
         (opts => O'Access,
          fp   => null);
   end Initialize;

   procedure Stop
      (This : Context)
   is
      function notcurses_stop
         (nc : access C_Context)
         return int
      with Import, Convention => C, External_Name => "notcurses_stop";
   begin
      if notcurses_stop (This) /= 0 then
         raise Program_Error;
      end if;
   end Stop;

   procedure Render
      (This : Context)
   is
      function ncpile_render
         (n : Plane)
         return int
      with Import, Convention => C, External_Name => "ncpile_render";

      function ncpile_rasterize
         (n : Plane)
         return int
      with Import, Convention => C, External_Name => "ncpile_rasterize";

      P : constant Plane := Standard_Plane (This);
   begin
      if ncpile_render (P) /= 0 then
         raise Program_Error;
      end if;

      if ncpile_rasterize (P) /= 0 then
         raise Program_Error;
      end if;
   end Render;

   function Cursor
      (This : Context)
      return Coordinate
   is
      function notcurses_cursor_yx
         (nc   : Context;
          y, x : not null access int)
          return int
      with Import, Convention => C, External_Name => "notcurses_cursor_yx";

      Y, X : aliased int;
   begin
      if notcurses_cursor_yx (This, Y'Access, X'Access) /= 0 then
         raise Program_Error;
      end if;
      return Coordinate'(Y => Integer (Y), X => Integer (X));
   end Cursor;

   procedure Enable_Cursor
      (This : Context;
       Pos  : Coordinate)
   is
      function notcurses_cursor_enable
         (nc : Context;
          y, x : int)
          return int
      with Import, Convention => C, External_Name => "notcurses_cursor_enable";
   begin
      if notcurses_cursor_enable (This, int (Pos.Y), int (Pos.X)) /= 0 then
         raise Program_Error;
      end if;
   end Enable_Cursor;

   procedure Disable_Cursor
      (This : Context)
   is
      function notcurses_cursor_disable
         (nc : Context)
          return int
      with Import, Convention => C, External_Name => "notcurses_cursor_disable";
   begin
      if notcurses_cursor_disable (This) /= 0 then
         raise Program_Error;
      end if;
   end Disable_Cursor;

   function Standard_Plane
      (This : Context)
      return Plane
   is
      function notcurses_stdplane
         (nc : Context)
         return Plane
      with Import, Convention => C, External_Name => "notcurses_stdplane";
   begin
      return notcurses_stdplane (This);
   end Standard_Plane;

   function Dimensions
      (This : Plane)
      return Coordinate
   is
      procedure ncplane_dim_yx
         (nc : Plane;
          y, x : access int)
      with Import, Convention => C, External_Name => "ncplane_dim_yx";
      Y, X : aliased int;
   begin
      ncplane_dim_yx (This, Y'Access, X'Access);
      return Coordinate'(Y => Integer (Y), X => Integer (X));
   end Dimensions;

   function Create_Plane
      (Parent : Plane;
       Size   : Coordinate;
       Pos    : Coordinate := (0, 0);
       Resize : Resize_Behavior := Maximize)
      return Plane
   is
      type Resize_Callback is access function (n : Plane) return int
         with Convention => C;

      function ncplane_resize_maximize (n : Plane) return int
         with Import, Convention => C, External_Name => "ncplane_resize_maximize";

      function ncplane_resize_marginalized (n : Plane) return int
         with Import, Convention => C, External_Name => "ncplane_resize_marginalized";

      function ncplane_resize_realign (n : Plane) return int
      with Import, Convention => C, External_Name => "ncplane_resize_realign";

      function ncplane_resize_placewithin (n : Plane) return int
      with Import, Convention => C, External_Name => "ncplane_resize_placewithin";

      type Plane_Options is record
         y, x : int;
         rows, cols : unsigned;
         userptr, name : System.Address;
         resizecb : Resize_Callback;
         flags : Interfaces.Unsigned_64;
         margin_b, margin_r : unsigned;
      end record;

      function ncplane_create
         (n : Plane;
          nopts : not null access Plane_Options)
          return Plane
      with Import, Convention => C, External_Name => "ncplane_create";

      Opts : aliased Plane_Options :=
         (y          => int (Pos.Y),
          x          => int (Pos.X),
          rows       => unsigned (Size.Y),
          cols       => unsigned (Size.X),
          userptr    => System.Null_Address,
          name       => System.Null_Address,
          resizecb   => null,
          flags      => 0,
          margin_b   => 0,
          margin_r   => 0);
   begin
      case Resize is
         when No_Resize =>
            Opts.resizecb := null;
         when Maximize =>
            Opts.resizecb := ncplane_resize_maximize'Access;
         when Marginalized =>
            Opts.resizecb := ncplane_resize_marginalized'Access;
         when Realign =>
            Opts.resizecb := ncplane_resize_realign'Access;
         when Place_Within =>
            Opts.resizecb := ncplane_resize_placewithin'Access;
      end case;

      return ncplane_create (Parent, Opts'Access);
   end Create_Plane;

   procedure Destroy
      (This : Plane)
   is
      function ncplane_destroy
         (ncp : Plane)
         return int
      with Import, Convention => C, External_Name => "ncplane_destroy";
   begin
      if ncplane_destroy (This) /= 0 then
         raise Program_Error;
      end if;
   end Destroy;

   function Parent
      (This : Plane)
      return Plane
   is
      function ncplane_parent
         (n : Plane)
         return Plane
      with Import, Convention => C, External_Name => "ncplane_parent";
   begin
      return ncplane_parent (This);
   end Parent;

   function "+" (Left, Right : Coordinate) return Coordinate
   is ((Y => Left.Y + Right.Y, X => Left.X + Right.X));

   function "-" (Left, Right : Coordinate) return Coordinate
   is ((Y => Left.Y - Right.Y, X => Left.X - Right.X));

   function Position
      (This : Plane)
      return Coordinate
   is
      procedure ncplane_yx
         (n : Plane;
          y, x : access int)
      with Import, Convention => C, External_Name => "ncplane_yx";
      Y, X : aliased int;
   begin
      ncplane_yx (This, Y'Access, X'Access);
      return Coordinate'(Y => Integer (Y), X => Integer (X));
   end Position;

   procedure Move
      (This : Plane;
       To   : Coordinate)
   is
      function ncplane_move_yx
         (n : Plane;
          y, x : int)
          return int
      with Import, Convention => C, External_Name => "ncplane_move_yx";
   begin
      if ncplane_move_yx (This, int (To.Y), int (To.X)) /= 0 then
         raise Program_Error;
      end if;
   end Move;

   procedure Move_Relative
      (This : Plane;
       To   : Coordinate)
   is
   begin
      Move (This, Position (This) + To);
   end Move_Relative;

   procedure Resize
      (This : Plane;
       Size : Coordinate)
   is
      function ncplane_resize
         (n : Plane;
          keepy, keepx : int;
          keepleny, keeplenx : unsigned;
          yoff, xoff : int;
          ylen, xlen : unsigned)
          return int
      with Import, Convention => C, External_Name => "ncplane_resize";

      Old : constant Coordinate := Dimensions (This);
      keepleny, keeplenx : unsigned;
   begin
      keepleny := unsigned (if Old.Y > Size.Y then Size.Y else Old.Y);
      keeplenx := unsigned (if Old.X > Size.X then Size.X else Old.X);
      if ncplane_resize (This, 0, 0, keepleny, keeplenx, 0, 0, unsigned (Size.Y), unsigned (Size.X)) /= 0 then
         raise Program_Error;
      end if;
   end Resize;

   procedure Erase
      (This : Plane)
   is
      procedure ncplane_erase
         (n : Plane)
      with Import, Convention => C, External_Name => "ncplane_erase";
   begin
      ncplane_erase (This);
   end Erase;

   procedure Erase_Region
      (This  : Plane;
       Start : Coordinate := Current_Position;
       Size  : Coordinate := (0, 0))
   is
      function ncplane_erase_region
         (n : Plane;
          ystart, xstart : int;
          ylen, xlen : int)
          return int
      with Import, Convention => C, External_Name => "ncplane_erase_region";
   begin
      if ncplane_erase_region
         (n       => This,
          ystart  => int (Start.Y),
          xstart  => int (Start.X),
          ylen    => int (Size.Y),
          xlen    => int (Size.X)) /= 0
      then
         raise Program_Error;
      end if;
   end Erase_Region;

   procedure Set_Scrolling
      (This    : Plane;
       Enabled : Boolean)
   is
      function ncplane_set_scrolling
         (n : Plane;
          scrollp : unsigned)
          return C_bool
      with Import, Convention => C, External_Name => "ncplane_set_scrolling";
      Ignore : C_bool with Unreferenced;
   begin
      Ignore := ncplane_set_scrolling (This, (if Enabled then 1 else 0));
   end Set_Scrolling;

   function Scrolling
      (This : Plane)
      return Boolean
   is
      function ncplane_scrolling_p
         (n : Plane)
         return C_bool
      with Import, Convention => C, External_Name => "ncplane_scrolling_p";
   begin
      return Boolean (ncplane_scrolling_p (This));
   end Scrolling;

   procedure Reorder_Above
      (This  : Plane;
       Other : Plane)
   is
      function ncplane_move_above
         (n, above : Plane)
         return int
      with Import, Convention => C, External_Name => "ncplane_move_above";
   begin
      if ncplane_move_above (This, Other) /= 0 then
         raise Program_Error;
      end if;
   end Reorder_Above;

   procedure Reorder_Below
      (This  : Plane;
       Other : Plane)
   is
      function ncplane_move_below
         (n, below : Plane)
         return int
      with Import, Convention => C, External_Name => "ncplane_move_below";
   begin
      if ncplane_move_below (This, Other) /= 0 then
         raise Program_Error;
      end if;
   end Reorder_Below;

   procedure Reorder_Top
      (This : Plane)
   is
      function ncplane_move_above
         (n : Plane; above : System.Address := System.Null_Address)
         return int
      with Import, Convention => C, External_Name => "ncplane_move_above";
   begin
      if ncplane_move_above (This) /= 0 then
         raise Program_Error;
      end if;
   end Reorder_Top;

   procedure Reorder_Bottom
      (This : Plane)
   is
      function ncplane_move_below
         (n : Plane; below : System.Address := System.Null_Address)
         return int
      with Import, Convention => C, External_Name => "ncplane_move_below";
   begin
      if ncplane_move_below (This) /= 0 then
         raise Program_Error;
      end if;
   end Reorder_Bottom;

   procedure Move_Cursor
      (This : Plane;
       To   : Coordinate)
   is
      function ncplane_cursor_move_yx
         (n : Plane;
          y, x : int)
          return int
      with Import, Convention => C, External_Name => "ncplane_cursor_move_yx";
   begin
      if ncplane_cursor_move_yx (This, int (To.Y), int (To.X)) /= 0 then
         raise Program_Error;
      end if;
   end Move_Cursor;

   procedure Move_Cursor_Relative
      (This : Plane;
       To   : Coordinate)
   is
      function ncplane_cursor_move_rel
         (n : Plane;
          y, x : int)
          return int
      with Import, Convention => C, External_Name => "ncplane_cursor_move_rel";
   begin
      if ncplane_cursor_move_rel (This, int (To.Y), int (To.X)) /= 0 then
         raise Program_Error;
      end if;
   end Move_Cursor_Relative;

   function Cursor
      (This : Plane)
      return Coordinate
   is
      procedure ncplane_cursor_yx
         (n : Plane;
          y, x : access unsigned)
      with Import, Convention => C, External_Name => "ncplane_cursor_yx";
      Y, X : aliased unsigned;
   begin
      ncplane_cursor_yx (This, Y'Access, X'Access);
      return Coordinate'(Y => Integer (Y), X => Integer (X));
   end Cursor;

   function RGB
      (R, G, B : Color_Value)
      return Color
   is ((Palette => False, Not_Default  => True, A => Opaque,
        R => R, G => G, B => B));

   type Cell is record
      gcluster          : Interfaces.Unsigned_32 := 0;
      gcluster_backstop : Interfaces.Unsigned_8 := 0;
      width             : Interfaces.Unsigned_8 := 0;
      stylemask         : Style_Mask := Default_Style;
      bg, fg            : Interfaces.Unsigned_32 := 0;
   end record;

   function To_Cell
      (Ch : Wide_Wide_Character)
      return Cell
   is
      function wcwidth
         (c : wchar_t)
         return int
      with Import, Convention => C, External_Name => "wcwidth";

      EGC       : constant String := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Ch & "");
      EGC_Bytes : String (1 .. 4) := (others => Character'Val (0));
      EGC_Word  : Interfaces.Unsigned_32
         with Import, Address => EGC_Bytes'Address;

      C : Cell := (others => <>);
      Width : int := wcwidth (wchar_t (Ada.Characters.Conversions.To_Wide_Character (Ch)));
   begin
      EGC_Bytes (1 .. EGC'Length) := EGC;
      C.gcluster := EGC_Word;

      if Ch = Wide_Wide_Character'Val (0) or else Width < 0 then
         Width := 1;
      end if;
      C.width := Interfaces.Unsigned_8 (Width);
      return C;
   end To_Cell;

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
      (Source => Color,
       Target => Interfaces.Unsigned_32);

   procedure Put
      (This       : Plane;
       Ch         : Wide_Wide_Character;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color)
   is
      function ncplane_putc_yx
         (n : Plane;
          y, x : int;
          c : access Cell)
          return int
      with Import, Convention => C, External_Name => "ncplane_putc_yx";
      C : aliased Cell := To_Cell (Ch);
   begin
      C.stylemask := Style;
      C.fg := To_Unsigned_32 (Foreground);
      C.bg := To_Unsigned_32 (Background);

      if ncplane_putc_yx (This, int (Pos.Y), int (Pos.X), C'Access) = -1 then
         raise Program_Error;
      end if;
   end Put;

   procedure Put
      (This       : Plane;
       Str        : Wide_Wide_String;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color)
   is
      P : Coordinate := Pos;
   begin
      for Ch of Str loop
         Put (This, Ch, P, Style, Foreground, Background);
         P := Current_Position;
      end loop;
   end Put;

   procedure Put_Character
      (This       : Plane;
       Ch         : Character;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color)
   is
   begin
      Put (This, Ada.Characters.Conversions.To_Wide_Wide_Character (Ch), Pos, Style, Foreground, Background);
   end Put_Character;

   procedure Put_String
      (This       : Plane;
       Str        : String;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color)
   is
   begin
      Put (This, Ada.Characters.Conversions.To_Wide_Wide_String (Str), Pos, Style, Foreground, Background);
   end Put_String;

   procedure New_Line
      (This : Plane)
   is
   begin
      Put_Character (This, ASCII.LF);
   end New_Line;

   procedure Put_Line
      (This       : Plane;
       Str        : String;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color)
   is
   begin
      Put_String (This, Str, Pos, Style, Foreground, Background);
      New_Line (This);
   end Put_Line;

   function Get_Blocking
      (This : Context)
      return Input_Event
   is
      use type Interfaces.Unsigned_32;

      type ncinput is record
         id          : Interfaces.Unsigned_32;
         y, x        : int;
         utf8        : String (1 .. 5);
         alt         : Boolean;
         shift       : Boolean;
         ctrl        : Boolean;
         evtype      : Input_Action;
         modifiers   : Input_Modifiers;
         ypx, xpx    : int;
      end record;

      function notcurses_get
         (n : Context;
          ts : access System.OS_Interface.timespec;
          ni : access ncinput)
          return Interfaces.Unsigned_32
      with Import, Convention => C, External_Name => "notcurses_get";

      I : aliased ncinput;
   begin
      if notcurses_get (This, null, I'Access) = -1 then
         raise Program_Error;
      else
         return Input_Event'(
            Id             => Natural (I.id),
            Pos            => (Y => Integer (I.y), X => Integer (I.x)),
            Pixel_Offset   => (Y => Integer (I.ypx), X => Integer (I.xpx)),
            Action         => I.evtype,
            Modifiers      => I.modifiers);
      end if;
   end Get_Blocking;

   function Is_Key_Event
      (Event : Input_Event)
      return Boolean
   is (Event.Id in Notcurses_Keys.PRETERUNICODEBASE .. Notcurses_Keys.PRETERUNICODEBASE + 5000);

   function To_Key
      (Event : Input_Event)
      return Notcurses_Keys.Key
   is (Notcurses_Keys.Key'Enum_Val (Event.Id - Notcurses_Keys.PRETERUNICODEBASE));

   function To_Wide_Wide_Character
      (Event : Input_Event)
      return Wide_Wide_Character
   is (Wide_Wide_Character'Val (Event.Id));

end Notcurses;
