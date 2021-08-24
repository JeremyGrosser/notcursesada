with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Notcurses.Visual is
   function From_File
      (Filename : Wide_Wide_String)
      return Notcurses_Visual
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      use Interfaces.C.Strings;
      Chars : aliased constant chars_ptr := New_String (Encode (Filename));
   begin
      return Thin.ncvisual_from_file (Chars);
   end From_File;

   procedure Render
      (Context : Notcurses_Context;
       Visual  : Notcurses_Visual;
       Options : Visual_Options;
       Plane   : out Notcurses_Plane)
   is
      O : aliased Thin.ncvisual_options := To_C (Options);
   begin
      Plane := Thin.ncvisual_render (Context, Visual, O'Access);
   end Render;

   function Decode
      (Visual : Notcurses_Visual)
      return Decode_Status
   is
      use Interfaces.C;
      Status : int;
   begin
      Status := Thin.ncvisual_decode (Visual);
      case Status is
         when 0 =>
            return Ok;
         when 1 =>
            return End_Of_File;
         when others =>
            return Error;
      end case;
   end Decode;

   procedure Destroy
      (This : Notcurses_Visual)
   is
   begin
      Thin.ncvisual_destroy (This);
   end Destroy;

   function To_C
      (O : Visual_Options)
      return Thin.ncvisual_options
   is
      X : constant Thin.ncvisual_options :=
         (n          => O.Plane,
          scaling    => Thin.ncscale_e'Val (Scale_Mode'Pos (O.Scaling)),
          y          => Interfaces.C.int (O.Position.Y),
          x          => Interfaces.C.int (O.Position.X),
          begy       => Interfaces.C.int (O.Origin.Y),
          begx       => Interfaces.C.int (O.Origin.X),
          leny       => Interfaces.C.int (O.Size.Y),
          lenx       => Interfaces.C.int (O.Size.X),
          blitter    => Thin.ncblitter_e'Val (Blitter'Pos (O.Blit)),
          flags      => To_UInt64 (O.Flags),
          transcolor => O.Transparent_Color);
   begin
      return X;
   end To_C;
end Notcurses.Visual;
