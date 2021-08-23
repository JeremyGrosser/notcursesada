--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Notcurses.Channel; use Notcurses.Channel;
with Interfaces; use Interfaces;

package Notcurses.Plane is

   type Alignment is (Unaligned, Left, Center, Right);

   --  Get a reference to the standard plane using the default context. The
   --  default context is initialized if needed.
   function Standard_Plane
      return Notcurses_Plane;

   function Context
      (Plane : Notcurses_Plane)
      return Notcurses_Context;

   function Dimensions
      (Plane : Notcurses_Plane)
      return Coordinate;

   function Create_Sub_Plane
      (Plane    : Notcurses_Plane;
       Position : Coordinate;
       Size     : Coordinate)
      return Notcurses_Plane;

   procedure Destroy
      (Plane : in out Notcurses_Plane);

   procedure Erase
      (Plane : Notcurses_Plane);

   procedure Put
      (Plane : Notcurses_Plane;
       Str   : Wide_Wide_String;
       Y, X  : Integer := -1);

   procedure Put_Aligned
      (Plane : Notcurses_Plane;
       Str   : Wide_Wide_String;
       Y     : Integer := -1;
       Align : Alignment := Unaligned);

   procedure Fill
      (Plane : Notcurses_Plane;
       C     : Wide_Wide_Character);

   procedure Set_Background
      (Plane   : Notcurses_Plane;
       Channel : Notcurses_Channel);

   procedure Set_Foreground
      (Plane   : Notcurses_Plane;
       Channel : Notcurses_Channel);

   procedure Set_Background_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Color_Type);

   procedure Set_Foreground_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Color_Type);

   package Cursor is
      function Position
         (Plane : Notcurses_Plane)
         return Coordinate;

      procedure Move
         (Plane    : Notcurses_Plane;
          Position : Coordinate);

      procedure Move_Relative
         (Plane  : Notcurses_Plane;
          Offset : Coordinate);

      procedure Home
         (Plane : Notcurses_Plane);
   end Cursor;

end Notcurses.Plane;
