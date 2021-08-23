with Ada.Unchecked_Conversion;

package Notcurses.Channel is

   type Color_Type is mod 2 ** 8;

   type Alpha_Type is (Opaque, Blend, Transparent, High_Contrast)
      with Size => 2;

   type Palette_Index is mod 2 ** 8;

   type Notcurses_Channel
      (Use_Palette  : Boolean := False)
   is record
      Not_Default   : Boolean := True;
      Alpha         : Alpha_Type := Opaque;
      case Use_Palette is
         when True =>
            Palette : Palette_Index := 0;
         when False =>
            R, G, B : Color_Type := 0;
      end case;
   end record
      with Size => 32;

   for Notcurses_Channel use record
      B           at 0 range 0  ..  7;
      G           at 0 range 8  .. 15;
      R           at 0 range 16 .. 23;
      Palette     at 0 range 0  .. 23;
      Use_Palette at 0 range 27 .. 27;
      Alpha       at 0 range 28 .. 29;
      Not_Default at 0 range 30 .. 30;
   end record;

   Default_Color : constant Notcurses_Channel :=
      (Use_Palette => False,
       Not_Default => False,
       Alpha       => Opaque,
       R           => Color_Type'Last,
       G           => Color_Type'Last,
       B           => Color_Type'Last);

   function To_C is new Ada.Unchecked_Conversion
      (Source => Notcurses_Channel,
       Target => Interfaces.Unsigned_32);

   function To_Ada is new Ada.Unchecked_Conversion
      (Source => Interfaces.Unsigned_32,
       Target => Notcurses_Channel);

end Notcurses.Channel;
