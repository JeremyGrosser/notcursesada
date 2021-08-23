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
      return Thin.ncvisual_from_plane
         (n    => Plane,
          blit => B,
          begy => int (Position.Y),
          begx => int (Position.X),
          leny => unsigned (Size.Y),
          lenx => unsigned (Size.X));
   end From_Plane;

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
      use Interfaces.C;
      X : constant Thin.ncvisual_options :=
         (n          => O.Plane,
          scaling    => Thin.ncscale_e'Val (Scale_Mode'Pos (O.Scaling)),
          y          => int (O.Position.Y),
          x          => int (O.Position.X),
          begy       => unsigned (O.Origin.Y),
          begx       => unsigned (O.Origin.X),
          leny       => unsigned (O.Size.Y),
          lenx       => unsigned (O.Size.X),
          blitter    => Thin.ncblitter_e'Val (Blitter'Pos (O.Blit)),
          flags      => To_UInt64 (O.Flags),
          transcolor => O.Transparent_Color,
          pxoffy     => unsigned (O.Offset.Y),
          pxoffx     => unsigned (O.Offset.X));
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
      Geom   : aliased Thin.ncvgeom;
      Status : int;
   begin
      Status := Thin.ncvisual_geom
         (nc    => Context,
          n     => Visual,
          vopts => O'Access,
          geom  => Geom'Access);
      if Status /= 0 then
         raise Notcurses_Error with "Invalid visual options";
      end if;
      return (Y => Integer (Geom.leny), X => Integer (Geom.lenx));
   end Dimensions;

   function Scale
      (Context : Notcurses_Context;
       Visual  : Notcurses_Visual;
       Options : Visual_Options)
       return Coordinate
   is
      use Interfaces.C;
      O      : aliased Thin.ncvisual_options := To_C (Options);
      Geom   : aliased Thin.ncvgeom;
      Status : int;
   begin
      Status := Thin.ncvisual_geom
         (nc    => Context,
          n     => Visual,
          vopts => O'Access,
          geom  => Geom'Access);
      if Status /= 0 then
         raise Notcurses_Error with "Invalid visual options";
      end if;
      return (Y => Integer (Geom.scaley), X => Integer (Geom.scalex));
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
      Status := Thin.ncvisual_set_yx
         (n       => Visual,
          y       => unsigned (Position.Y),
          x       => unsigned (Position.X),
          pixel   => To_C (Color));
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
      Status := Thin.ncvisual_at_yx
         (n => Visual,
          y => unsigned (Position.Y),
          x => unsigned (Position.X),
          pixel => Pixel'Access);
      if Status /= 0 then
         raise Notcurses_Error with "Get pixel at (" & Position.Y'Image & "," & Position.X'Image & ") failed";
      end if;
      return To_Ada (Pixel);
   end Get;

end Notcurses.Visual;
