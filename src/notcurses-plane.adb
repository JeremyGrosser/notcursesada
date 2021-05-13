--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Notcurses.Context;

package body Notcurses.Plane is

   function Standard_Plane
      return Notcurses_Plane
   is
   begin
      if Default_Context = null then
         Notcurses.Context.Initialize;
      end if;

      return Thin.notcurses_stdplane (Default_Context);
   end Standard_Plane;

   function Context
      (Plane : Notcurses_Plane)
      return Notcurses_Context
   is (Thin.ncplane_notcurses (Plane));

   function Dimensions
      (Plane : Notcurses_Plane)
      return Coordinate
   is
      Y, X : aliased int;
   begin
      Thin.ncplane_dim_yx (Plane, Y'Access, X'Access);
      return
         (Y => Integer (Y),
          X => Integer (X));
   end Dimensions;

   function Create_Sub_Plane
      (Plane    : Notcurses_Plane;
       Position : Coordinate;
       Size     : Coordinate)
      return Notcurses_Plane
   is
      Options : aliased constant Thin.ncplane_options :=
         (y        => int (Position.Y),
          x        => int (Position.X),
          rows     => int (Size.Y),
          cols     => int (Size.X),
          userptr  => System.Null_Address,
          name     => Null_Ptr,
          resizecb => null,
          flags    => 0,
          margin_b => 0,
          margin_r => 0);
      New_Plane : Notcurses_Plane;
   begin
      New_Plane := Thin.ncplane_create (Plane, Options'Access);
      if New_Plane = null then
         raise Notcurses_Error with "Failed to create subplane";
      end if;
      return New_Plane;
   end Create_Sub_Plane;

   procedure Destroy
      (Plane : Notcurses_Plane)
   is
   begin
      if Thin.ncplane_destroy (Plane) /= 0 then
         raise Notcurses_Error with "Failed to destroy plane";
      end if;
   end Destroy;

   procedure Erase
      (Plane : Notcurses_Plane)
   is
   begin
      Thin.ncplane_erase (Plane);
   end Erase;

   procedure Erase_Region
      (Plane : Notcurses_Plane;
       Start : Coordinate;
       Size  : Coordinate)
   is
      Result : int;
   begin
      Result := Thin.ncplane_erase_region (Plane, int (Start.Y), int (Start.X), int (Size.Y), int (Size.X));
      if Result /= 0 then
         raise Notcurses_Error with "Failed to erase plane region";
      end if;
   end Erase_Region;

   procedure Put
      (Plane : Notcurses_Plane;
       Str   : Wide_Wide_String;
       Y, X  : Integer := -1)
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      Chars  : aliased chars_ptr := New_String (Encode (Str));
      Result : int;
   begin
      Result := Thin.ncplane_putstr_yx (Plane, int (Y), int (X), Chars);
      Free (Chars);
      if Result < 0 then
         raise Notcurses_Error with "Failed to put Wide_Wide_String on plane";
      end if;
   end Put;

   procedure Set_Background_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Unsigned_8)
   is
   begin
      if Thin.ncplane_set_bg_rgb8 (Plane, int (R), int (G), int (B)) /= 0 then
         raise Notcurses_Error with "Failed to set plane background color";
      end if;
   end Set_Background_RGB;

   procedure Set_Foreground_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Unsigned_8)
   is
   begin
      if Thin.ncplane_set_fg_rgb8 (Plane, int (R), int (G), int (B)) /= 0 then
         raise Notcurses_Error with "Failed to set plane foreground color";
      end if;
   end Set_Foreground_RGB;
end Notcurses.Plane;
