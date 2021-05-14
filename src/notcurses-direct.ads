--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Notcurses.Channel; use Notcurses.Channel;
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
      (This       : Notcurses_Direct;
       Str        : Wide_Wide_String;
       Foreground : Notcurses_Channel :=
          (Not_Default => False, others => <>);
       Background : Notcurses_Channel :=
          (Not_Default => False, others => <>));

   procedure New_Line
      (This : Notcurses_Direct);

   procedure Set_Background_RGB
      (This    : Notcurses_Direct;
       R, G, B : Color_Type);

   procedure Set_Foreground_RGB
      (This    : Notcurses_Direct;
       R, G, B : Color_Type);

   procedure Stop
      (This : in out Notcurses_Direct);

private

   package Direct_Thin renames Notcurses_Direct_Thin;

   type Notcurses_Direct is access all Direct_Thin.ncdirect;

end Notcurses.Direct;
