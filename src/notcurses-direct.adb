--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with Interfaces;

package body Notcurses.Direct is
   procedure Initialize
      (This          : out Notcurses_Direct;
       Terminal_Type : String := "")
   is
      Term : chars_ptr;
   begin
      if Terminal_Type /= "" then
         Term := New_String (Terminal_Type);
         This := Direct_Thin.ncdirect_init (Term, Thin.Null_File, 0);
         Free (Term);
      else
         This := Direct_Thin.ncdirect_init (Null_Ptr, Thin.Null_File, 0);
      end if;

      if This = null then
         raise Notcurses_Error with "Failed to initialize notcurses direct";
      end if;
   end Initialize;

   procedure Set_Cursor_Enabled
      (This    : Notcurses_Direct;
       Enabled : Boolean)
   is
      Result : int;
   begin
      if Enabled then
         Result := Direct_Thin.ncdirect_cursor_enable (This);
      else
         Result := Direct_Thin.ncdirect_cursor_disable (This);
      end if;

      if Result /= 0 then
         raise Notcurses_Error with "Failed to set direct cursor enabled";
      end if;
   end Set_Cursor_Enabled;

   function Dimensions
      (This : Notcurses_Direct)
      return Coordinate
   is ((X => Integer (Direct_Thin.ncdirect_dim_x (This)),
        Y => Integer (Direct_Thin.ncdirect_dim_y (This))));

   procedure Put
      (This : Notcurses_Direct;
       Str  : String)
   is
      Channels : constant Interfaces.Unsigned_64 := 0;
      Chars    : chars_ptr := New_String (Str);
      Result   : int;
   begin
      Result := Direct_Thin.ncdirect_putstr (This, Channels, Chars);
      Free (Chars);
      if Result < 0 then
         raise Notcurses_Error with "Failed to direct put string";
      end if;
   end Put;

   procedure Stop
      (This : in out Notcurses_Direct)
   is
   begin
      if Direct_Thin.ncdirect_stop (This) /= 0 then
         raise Notcurses_Error with "Failed to stop notcurses direct";
      else
         This := null;
      end if;
   end Stop;
end Notcurses.Direct;
