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
      use Interfaces.C;
      Chars : chars_ptr := New_String (Encode (Str));
      Columns : aliased int;
      Result : int;
   begin
      Result := Notcurses_Thin.ncstrwidth (Chars, null, Columns'Access);
      if Result < 0 then
         raise Program_Error with "ncstrwidth returned error";
      end if;
      Free (Chars);
      return Integer (Columns);
   end Width;
end Notcurses.Strings;
