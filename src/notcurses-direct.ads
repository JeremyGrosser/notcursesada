--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Notcurses_Direct_Thin;

package Notcurses.Direct is
   type Notcurses_Direct is private;

   procedure Initialize
      (This          : out Notcurses_Direct;
       Terminal_Type : String := "");

   procedure Set_Cursor_Enabled
      (This    : Notcurses_Direct;
       Enabled : Boolean);

   function Dimensions
      (This : Notcurses_Direct)
      return Coordinate;

   procedure Put
      (This : Notcurses_Direct;
       Str  : String);

   procedure Stop
      (This : in out Notcurses_Direct);

private

   package Direct_Thin renames Notcurses_Direct_Thin;

   type Notcurses_Direct is access all Direct_Thin.ncdirect;

end Notcurses.Direct;
