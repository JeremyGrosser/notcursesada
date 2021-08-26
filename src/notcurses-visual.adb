with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Notcurses.Visual is
   function From_Bitmap
      (Bitmap : RGBA_Bitmap)
      return Notcurses_Visual
   is
      use Interfaces.C;
      Rows    : constant int := int (Bitmap'Last (1) - Bitmap'First (1) + 1);
      Columns : constant int := int (Bitmap'Last (2) - Bitmap'First (2) + 1);
   begin
      return Thin.ncvisual_from_rgba
         (rgba       => Bitmap'Address,
          rows       => Rows,
          rowstride  => Columns,
          cols       => Columns);
   end From_Bitmap;

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

   function From_Plane
      (Plane    : Notcurses_Plane;
       Blit     : Blitter;
       Position : Coordinate := (0, 0);
       Size     : Coordinate := (-1, -1))
      return Notcurses_Visual
   is
      use Interfaces.C;
      B : constant Thin.ncblitter_e := Thin.ncblitter_e'Val (Blitter'Pos (Blit));
   begin
      return Thin.ncvisual_from_plane (Plane, B,
         int (Position.Y), int (Position.X),
         int (Size.Y), int (Size.X));
   end From_Plane;

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

   function Dimensions
      (Context : Notcurses_Context;
       Visual  : Notcurses_Visual;
       Options : Visual_Options)
       return Coordinate
   is
      use Interfaces.C;
      O      : aliased Thin.ncvisual_options := To_C (Options);
      X, Y   : aliased int;
      Status : int;
   begin
      Status := Thin.ncvisual_blitter_geom (Context, Visual, O'Access, Y'Access, X'Access, null, null, null);
      if Status /= 0 then
         raise Notcurses_Error with "Invalid visual options";
      end if;
      return (Y => Integer (Y), X => Integer (X));
   end Dimensions;

   function Scale
      (Context : Notcurses_Context;
       Visual  : Notcurses_Visual;
       Options : Visual_Options)
       return Coordinate
   is
      use Interfaces.C;
      O      : aliased Thin.ncvisual_options := To_C (Options);
      X, Y   : aliased int;
      Status : int;
   begin
      Status := Thin.ncvisual_blitter_geom (Context, Visual, O'Access, null, null, Y'Access, X'Access, null);
      if Status /= 0 then
         raise Notcurses_Error with "Invalid visual options";
      end if;
      return (Y => Integer (Y), X => Integer (X));
   end Scale;

   function Media_Default_Blitter
      (Context : Notcurses_Context;
       Scale   : Scale_Mode)
      return Blitter
   is
      use Thin;
      S : constant ncscale_e := ncscale_e'Val (Scale_Mode'Pos (Scale));
      B : ncblitter_e;
   begin
      B := ncvisual_media_defblitter (Context, S);
      return Blitter'Val (ncblitter_e'Pos (B));
   end Media_Default_Blitter;

   procedure Set
      (Visual   : Notcurses_Visual;
       Position : Coordinate;
       Color    : RGBA)
   is
      use Interfaces.C;
      Status : int;
   begin
      Status := Thin.ncvisual_set_yx (Visual, int (Position.Y), int (Position.X), To_C (Color));
      if Status /= 0 then
         raise Notcurses_Error with "Set pixel at (" & Position.Y'Image & "," & Position.X'Image & ") failed";
      end if;
   end Set;

   function Get
      (Visual   : Notcurses_Visual;
       Position : Coordinate)
       return RGBA
   is
      use Interfaces.C;
      use Interfaces;
      Pixel  : aliased Unsigned_32;
      Status : int;
   begin
      Status := Thin.ncvisual_at_yx (Visual, int (Position.Y), int (Position.X), Pixel'Access);
      if Status /= 0 then
         raise Notcurses_Error with "Get pixel at (" & Position.Y'Image & "," & Position.X'Image & ") failed";
      end if;
      return To_Ada (Pixel);
   end Get;

end Notcurses.Visual;
