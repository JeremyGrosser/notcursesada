--
--  Copyright (C) 2021-2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
package Notcurses
   with Preelaborate
is
   type Context is private;

   type Log_Level is (Silent, Panic, Fatal, Error, Warning, Info, Verbose, Debug, Trace)
      with Size => 32;
   for Log_Level use
      (Silent  => -1,
       Panic   => 0,
       Fatal   => 1,
       Error   => 2,
       Warning => 3,
       Info    => 4,
       Verbose => 5,
       Debug   => 6,
       Trace   => 7);

   type Option_Flags is record
      Inhibit_Setlocale    : Boolean := False;
      No_Clear_Bitmaps     : Boolean := False;
      No_Winch_Sighandler  : Boolean := False;
      No_Quit_Sighandlers  : Boolean := False;
      Preserve_Cursor      : Boolean := False;
      Suppress_Banners     : Boolean := True;
      No_Alternate_Screen  : Boolean := False;
      No_Font_Changes      : Boolean := False;
      Drain_Input          : Boolean := False;
      Scrolling            : Boolean := False;
   end record
      with Size => 64;
   for Option_Flags use record
      Inhibit_Setlocale    at 0 range 0 .. 0;
      No_Clear_Bitmaps     at 0 range 1 .. 1;
      No_Winch_Sighandler  at 0 range 2 .. 2;
      No_Quit_Sighandlers  at 0 range 3 .. 3;
      Preserve_Cursor      at 0 range 4 .. 4;
      Suppress_Banners     at 0 range 5 .. 5;
      No_Alternate_Screen  at 0 range 6 .. 6;
      No_Font_Changes      at 0 range 7 .. 7;
      Drain_Input          at 0 range 8 .. 8;
      Scrolling            at 0 range 9 .. 9;
   end record;

   CLI_Mode : constant Option_Flags :=
      (Scrolling           => True,
       No_Alternate_Screen => True,
       Preserve_Cursor     => True,
       No_Clear_Bitmaps    => True,
       others              => <>);

   function Initialize
      (Term_Type     : String := "";
       Level         : Log_Level := Error;
       Margin_Top    : Natural := 0;
       Margin_Right  : Natural := 0;
       Margin_Bottom : Natural := 0;
       Margin_Left   : Natural := 0;
       Flags         : Option_Flags := (others => <>))
       return Context;

   procedure Stop
      (This : Context);

   procedure Render
      (This : Context);

   type Coordinate is record
      Y, X : Integer;
   end record;

   Current_Position : constant Coordinate := (-1, -1);

   function "+" (Left, Right : Coordinate) return Coordinate;
   function "-" (Left, Right : Coordinate) return Coordinate;

   function Cursor
      (This : Context)
      return Coordinate;

   procedure Enable_Cursor
      (This : Context;
       Pos  : Coordinate);

   procedure Disable_Cursor
      (This : Context);

   type Plane is private;

   function Standard_Plane
      (This : Context)
      return Plane;

   type Resize_Behavior is (No_Resize, Maximize, Marginalized, Realign, Place_Within);

   function Create_Plane
      (Parent : Plane;
       Size   : Coordinate;
       Pos    : Coordinate := (0, 0);
       Resize : Resize_Behavior := Maximize)
      return Plane;

   procedure Destroy
      (This : Plane);

   function Parent
      (This : Plane)
      return Plane;

   function Position
      (This : Plane)
      return Coordinate;

   procedure Move
      (This : Plane;
       To   : Coordinate);

   procedure Move_Relative
      (This : Plane;
       To   : Coordinate);

   function Dimensions
      (This : Plane)
      return Coordinate;

   procedure Resize
      (This : Plane;
       Size : Coordinate);

   procedure Erase
      (This : Plane);

   procedure Erase_Region
      (This  : Plane;
       Start : Coordinate := Current_Position;
       Size  : Coordinate := (0, 0));

   procedure Set_Scrolling
      (This    : Plane;
       Enabled : Boolean);

   function Scrolling
      (This : Plane)
      return Boolean;

   procedure Reorder_Above
      (This  : Plane;
       Other : Plane);

   procedure Reorder_Below
      (This  : Plane;
       Other : Plane);

   procedure Reorder_Top
      (This  : Plane);

   procedure Reorder_Bottom
      (This  : Plane);

   procedure Move_Cursor
      (This : Plane;
       To   : Coordinate);

   procedure Move_Cursor_Relative
      (This : Plane;
       To   : Coordinate);

   function Cursor
      (This : Plane)
      return Coordinate;

   type Style_Mask is record
      Struck      : Boolean;
      Bold        : Boolean;
      Undercurl   : Boolean;
      Underline   : Boolean;
      Italic      : Boolean;
   end record
      with Size => 16;
   for Style_Mask use record
      Struck      at 0 range 0 .. 0;
      Bold        at 0 range 1 .. 1;
      Undercurl   at 0 range 2 .. 2;
      Underline   at 0 range 3 .. 3;
      Italic      at 0 range 4 .. 4;
   end record;

   Default_Style : constant Style_Mask := (others => False);

   type Color_Value is mod 2 ** 8
      with Size => 8;

   type Alpha_Value is (Opaque, Blend, Transparent, High_Contrast)
      with Size => 2;

   type Palette_Index is mod 2 ** 8
      with Size => 8;

   type Color
      (Palette : Boolean)
   is record
      Not_Default : Boolean := True;
      A : Alpha_Value := Opaque;
      case Palette is
         when True =>
            Index : Palette_Index := 0;
         when False =>
            R, G, B : Color_Value := 0;
      end case;
   end record
      with Size => 32;
   for Color use record
      Index       at 0 range 0 .. 7;
      B           at 0 range 0 .. 7;
      G           at 0 range 8 .. 15;
      R           at 0 range 16 .. 23;
      Palette     at 0 range 27 .. 27;
      A           at 0 range 28 .. 29;
      Not_Default at 0 range 30 .. 30;
   end record;

   Default_Color : constant Color :=
      (Palette       => True,
       Not_Default   => False,
       others        => <>);

   function RGB
      (R, G, B : Color_Value)
      return Color;

   procedure Put
      (This       : Plane;
       Ch         : Wide_Wide_Character;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color);

   procedure Put
      (This       : Plane;
       Str        : Wide_Wide_String;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color);

   procedure Put_Character
      (This       : Plane;
       Ch         : Character;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color);

   procedure Put_String
      (This       : Plane;
       Str        : String;
       Pos        : Coordinate := Current_Position;
       Style      : Style_Mask := Default_Style;
       Foreground : Color := Default_Color;
       Background : Color := Default_Color);

   procedure New_Line
      (This : Plane)
   with Pre => Scrolling (This);

   type Input_Action is (Unknown, Press, Repeat, Release);
   type Input_Modifiers is record
      Shift, Alt, Ctrl, Super, Hyper, Meta, Caps_Lock, Num_Lock : Boolean;
   end record
      with Size => 32;
   for Input_Modifiers use record
      Shift       at 0 range 0 .. 0;
      Alt         at 0 range 1 .. 1;
      Ctrl        at 0 range 2 .. 2;
      Super       at 0 range 3 .. 3;
      Hyper       at 0 range 4 .. 4;
      Meta        at 0 range 5 .. 5;
      Caps_Lock   at 0 range 6 .. 6;
      Num_Lock    at 0 range 7 .. 7;
   end record;

   type Input_Event is record
      Id             : Natural;
      Ch             : Wide_Wide_Character;
      Pos            : Coordinate := (-1, -1);
      Action         : Input_Action := Unknown;
      Modifiers      : Input_Modifiers := (others => False);
      Pixel_Offset   : Coordinate := (-1, -1);
   end record;

   function Get_Blocking
      (This : Context)
      return Input_Event;

private

   type C_Context is null record;
   type Context is not null access all C_Context;

   type C_Plane is null record;
   type Plane is not null access all C_Plane;

end Notcurses;
