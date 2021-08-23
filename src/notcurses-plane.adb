--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
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
      Y, X : aliased unsigned;
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
          rows     => unsigned (Size.Y),
          cols     => unsigned (Size.X),
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
      (Plane : in out Notcurses_Plane)
   is
   begin
      if Thin.ncplane_destroy (Plane) /= 0 then
         raise Notcurses_Error with "Failed to destroy plane";
      end if;
      Plane := null;
   end Destroy;

   procedure Erase
      (Plane : Notcurses_Plane)
   is
   begin
      Thin.ncplane_erase (Plane);
   end Erase;

   procedure Put
      (Plane : Notcurses_Plane;
       Str   : Wide_Wide_String;
       Y, X  : Integer := -1)
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      Chars  : aliased chars_ptr := New_String (Encode (Str));
      Result : int;
   begin
      Result := Thin.ncplane_putegc_yx
         (n       => Plane,
          y       => int (Y),
          x       => int (X),
          gclust  => Chars,
          sbytes  => null);

      Free (Chars);

      if Result < 0 then
         raise Notcurses_Error with "Failed to put Wide_Wide_String on plane";
      end if;
   end Put;

   procedure Put_Aligned
      (Plane : Notcurses_Plane;
       Str   : Wide_Wide_String;
       Y     : Integer := -1;
       Align : Alignment := Unaligned)
   is
      function wcswidth
         (s : chars_ptr;
          n : size_t)
          return int
      with Import, Convention => C;

      function notcurses_align
         (availu  : int;
          align   : Thin.ncalign_e;
          u       : int)
          return int;

      function notcurses_align
         (availu  : int;
          align   : Thin.ncalign_e;
          u       : int)
          return int
      is
         use type Thin.ncalign_e;
      begin
         if align = Thin.NCALIGN_LEFT then
            return 0;
         end if;

         if align = Thin.NCALIGN_CENTER then
            return (availu - u) / 2;
         end if;

         if align = Thin.NCALIGN_RIGHT then
            return availu - u;
         end if;

         return int'First;
      end notcurses_align;

      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      Chars  : aliased chars_ptr := New_String (Encode (Str));
      Width  : constant int := wcswidth (Chars, size_t (int'Last));
      NCA    : Thin.ncalign_e;
      X_Pos  : int;
      Result : int;
   begin
      case Align is
         when Unaligned => NCA := Thin.NCALIGN_UNALIGNED;
         when Left      => NCA := Thin.NCALIGN_LEFT;
         when Right     => NCA := Thin.NCALIGN_RIGHT;
         when Center    => NCA := Thin.NCALIGN_CENTER;
      end case;

      X_Pos := notcurses_align
         (availu  => int (Dimensions (Plane).X),
          align   => NCA,
          u       => Width);

      if X_Pos < 0 then
         X_Pos := 0;
      end if;

      Result := Thin.ncplane_putegc_yx
         (n       => Plane,
          y       => int (Y),
          x       => X_Pos,
          gclust  => Chars,
          sbytes  => null);

      Free (Chars);
      --  if Result < 0 then
      --     raise Notcurses_Error with "Failed to put Wide_Wide_String on plane";
      --  end if;
   end Put_Aligned;

   procedure Fill
      (Plane : Notcurses_Plane;
       C     : Wide_Wide_Character)
   is
      Size : constant Coordinate := Dimensions (Plane);
      S    : constant Wide_Wide_String (1 .. Size.X) := (others => C);
   begin
      for Y in 0 .. Size.Y - 1 loop
         Put (Plane, S, Y => Y, X => 0);
      end loop;
   end Fill;

   procedure Set_Background
      (Plane   : Notcurses_Plane;
       Channel : Notcurses_Channel)
   is
      Result : Unsigned_64
         with Unreferenced;
   begin
      Result := Thin.ncplane_set_bchannel (Plane, To_C (Channel));
   end Set_Background;

   procedure Set_Foreground
      (Plane   : Notcurses_Plane;
       Channel : Notcurses_Channel)
   is
      Result : Unsigned_64
         with Unreferenced;
   begin
      Result := Thin.ncplane_set_fchannel (Plane, To_C (Channel));
   end Set_Foreground;

   procedure Set_Background_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Color_Type)
   is
      Channel : constant Notcurses_Channel :=
         (R           => R,
          G           => G,
          B           => B,
          Not_Default => True,
          Use_Palette => False,
          Alpha       => Opaque);
   begin
      Set_Background (Plane, Channel);
   end Set_Background_RGB;

   procedure Set_Foreground_RGB
      (Plane   : Notcurses_Plane;
       R, G, B : Color_Type)
   is
      Result : Unsigned_64
         with Unreferenced;
   begin
      Result := Thin.ncplane_set_fchannel (Plane,
         Thin.NC_BGDEFAULT_MASK or
         Shift_Left (Unsigned_32 (R), 16) or
         Shift_Left (Unsigned_32 (G), 8) or
                     Unsigned_32 (B));
   end Set_Foreground_RGB;

   package body Cursor is
      function Position
         (Plane : Notcurses_Plane)
         return Coordinate
      is
         Y, X : aliased unsigned;
      begin
         Thin.ncplane_cursor_yx (Plane, Y'Access, X'Access);
         return (Y => Integer (Y), X => Integer (X));
      end Position;

      procedure Move
         (Plane    : Notcurses_Plane;
          Position : Coordinate)
      is
      begin
         if Thin.ncplane_cursor_move_yx (Plane, int (Position.Y), int (Position.X)) = -1 then
            raise Notcurses_Error with "Failed to move the cursor";
         end if;
      end Move;

      procedure Move_Relative
         (Plane  : Notcurses_Plane;
          Offset : Coordinate)
      is
      begin
         Move (Plane, Cursor.Position (Plane) + Offset);
      end Move_Relative;

      procedure Home
         (Plane : Notcurses_Plane)
      is
      begin
         Thin.ncplane_home (Plane);
      end Home;
   end Cursor;

end Notcurses.Plane;
