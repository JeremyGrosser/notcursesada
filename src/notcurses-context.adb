--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Interfaces.C; use Interfaces.C;
with Interfaces.C_Streams;

package body Notcurses.Context is

   procedure Initialize is
   begin
      Default_Context := Notcurses_Context
         (Thin.notcurses_init (Default_Options'Access, Interfaces.C_Streams.NULL_Stream));
   end Initialize;

   procedure Render
      (Context : Notcurses_Context)
   is
      Result : int;
   begin
      Result := Thin.notcurses_render (Context);
      if Result /= 0 then
         raise Notcurses_Error with "Failed to render notcurses context";
      end if;
   end Render;

   procedure Enable_Cursor
      (Context : Notcurses_Context;
       Y, X    : Integer := 0)
   is
   begin
      if Thin.notcurses_cursor_enable (Context, int (Y), int (X)) /= 0 then
         raise Notcurses_Error with "Failed to enable cursor";
      end if;
   end Enable_Cursor;

   procedure Enable_Mouse
      (Context : Notcurses_Context)
   is
   begin
      if Thin.notcurses_mouse_enable (Context) /= 0 then
         raise Notcurses_Error with "Failed to enable mouse";
      end if;
   end Enable_Mouse;

   function Get
      (Context : Notcurses_Context)
      return Notcurses_Input
   is
      NI          : aliased Thin.ncinput;
      Char        : Wide_Wide_Character with Unreferenced;
   begin
      Char := Wide_Wide_Character'Val (Thin.notcurses_get (Context, null, NI'Access));
      return To_Ada (NI);
   end Get;

   function Palette_Size
      (Context : Notcurses_Context)
      return Natural
   is
   begin
      return Natural (Thin.notcurses_palette_size (Context)) - 1;
   end Palette_Size;

   procedure Stop
      (Context : in out Notcurses_Context)
   is
      Result : int;
   begin
      Result := Thin.notcurses_stop (Context);
      if Result /= 0 then
         raise Notcurses_Error with "Failed to stop notcurses context";
      end if;
      Context := null;
   end Stop;

   procedure Stop is
   begin
      Stop (Default_Context);
      Default_Context := null;
   end Stop;

end Notcurses.Context;
