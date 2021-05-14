--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
package Notcurses.Context is

   --  These procedures modify the default context
   procedure Initialize;
   procedure Stop;

   procedure Render
      (Context : Notcurses_Context);

   procedure Enable_Cursor
      (Context : Notcurses_Context;
       Y, X    : Integer := 0);

   procedure Enable_Mouse
      (Context : Notcurses_Context);

   function Get
      (Context : Notcurses_Context)
      return Notcurses_Input;

   function Palette_Size
      (Context : Notcurses_Context)
      return Natural;

   procedure Stop
      (Context : in out Notcurses_Context);

end Notcurses.Context;
