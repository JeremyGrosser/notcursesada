with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Interfaces.C.Strings;
with Notcurses_Thin;

package body Notcurses.Strings is
   function Width
      (Str : Wide_Wide_String)
      return Natural
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      use Interfaces.C.Strings;
      Chars : constant chars_ptr := New_String (Encode (Str));
   begin
      return Natural (Notcurses_Thin.ncstrwidth (Chars));
   end Width;
end Notcurses.Strings;
