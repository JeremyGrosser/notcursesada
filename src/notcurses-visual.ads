package Notcurses.Visual is
   type Notcurses_Visual is private;

   type Scale_Mode is (None, Scale, Stretch, None_Hires, None_Scale_Hires);
   type Blitter is (Blit_Default, Blit_1x1, Blit_2x1, Blit_2x2, Blit_3x2, Blit_Braille, Blit_Pixel, Blit_4x1, Blit_8x1);

   type Visual_Flags is record
      No_Degrade           : Boolean := False; --  fail rather than degrade
      Blend                : Boolean := False; --  use NCALPHA_BLEND with visual
      Horizontal_Aligned   : Boolean := False; --  Position.X is an alignment, not absolute
      Vertical_Aligned     : Boolean := False; --  Position.Y is an alignment, not absolute
      Add_Alpha            : Boolean := False; --  transcolor is in effect
      Child_Plane          : Boolean := False; --  interpret n as parent
      No_Interpolate       : Boolean := False; --  non-interpolative scaling
   end record
      with Size => 64;
   for Visual_Flags use record
      No_Degrade           at 0 range 0 .. 0;
      Blend                at 0 range 1 .. 1;
      Horizontal_Aligned   at 0 range 2 .. 2;
      Vertical_Aligned     at 0 range 3 .. 3;
      Add_Alpha            at 0 range 4 .. 4;
      Child_Plane          at 0 range 5 .. 5;
      No_Interpolate       at 0 range 6 .. 6;
   end record;

   type Visual_Options is record
      Plane             : Notcurses_Plane;
      Scaling           : Scale_Mode := None;      --  ignored if Plane is provided
      Position          : Coordinate := (0, 0);    --  Offset the visual from the origin of Plane (or Standard_Plane if Plane is null)
      Origin            : Coordinate := (0, 0);    --  Mask the visual starting at Origin
      Size              : Coordinate := (0, 0);    --  Size of the Mask, or (0, 0) if no mask should be applied
      Blit              : Blitter := Blit_Default; --  Glyph set to use. Default selects the best available for your terminal
      Flags             : Visual_Flags := (others => False);
      Transparent_Color : Interfaces.Unsigned_32;  --  if Flags.Add_Alpha, treat this color as transparent
   end record;

   type Decode_Status is (Ok, End_Of_File, Error);

   function From_File
      (Filename : Wide_Wide_String)
      return Notcurses_Visual;

   procedure Render
      (Context : Notcurses_Context;
       Visual  : Notcurses_Visual;
       Options : Visual_Options;
       Plane   : out Notcurses_Plane);

   function Decode
      (Visual : Notcurses_Visual)
      return Decode_Status;

   procedure Destroy
      (This : Notcurses_Visual);

private

   type Notcurses_Visual is access all Thin.ncvisual;

   function To_UInt64 is new Ada.Unchecked_Conversion
      (Source => Visual_Flags,
       Target => Interfaces.Unsigned_64);

   function To_C
      (O : Visual_Options)
      return Thin.ncvisual_options;

end Notcurses.Visual;
