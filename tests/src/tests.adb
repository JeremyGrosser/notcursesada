--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;
with Notcurses.Context;
with Notcurses.Plane;
with Notcurses.Progress_Bar;
with Notcurses.Channel;
with Notcurses;

package body Tests is
   procedure Test_Version is
   begin
      Ada.Text_IO.Put_Line (Notcurses.Version);
   end Test_Version;

   procedure Test_Hello_World is
      Plane   : constant Notcurses.Notcurses_Plane := Notcurses.Plane.Standard_Plane;
      Context : constant Notcurses.Notcurses_Context := Notcurses.Plane.Context (Plane);
   begin
      Notcurses.Plane.Put (Plane, "Hello, world!");
      Notcurses.Context.Render (Context);
      delay 1.0;
      Notcurses.Plane.Erase (Plane);
   end Test_Hello_World;

   procedure Test_Colors is
      use Notcurses;
      use Notcurses.Plane;
      Plane : constant Notcurses_Plane := Standard_Plane;
   begin
      Set_Background_RGB (Plane, 0, 0, 255);
      Set_Foreground_RGB (Plane, 255, 0, 0);
      Put (Plane, "Red on blue",
         Y => 0);

      Set_Background_RGB (Plane, 0, 255, 0);
      Set_Foreground_RGB (Plane, 255, 255, 255);
      Put (Plane, "White on green",
         Y => 1,
         X => 0);

      Set_Background_RGB (Plane, 0, 0, 0);
      Set_Foreground_RGB (Plane, 255, 0, 255);
      Put (Plane, "Purple on black",
         Y => 2,
         X => 0);

      Notcurses.Context.Render (Notcurses.Plane.Context (Plane));
      delay 1.0;
      Notcurses.Plane.Erase (Plane);
   end Test_Colors;

   procedure Test_Palette is
      use Notcurses;
      use Notcurses.Plane;
      use Notcurses.Channel;

      Plane        : constant Notcurses_Plane := Standard_Plane;
      Context      : constant Notcurses_Context := Notcurses.Plane.Context (Plane);
      Dims         : constant Coordinate := Dimensions (Plane);
      Palette_Size : constant Natural := Notcurses.Context.Palette_Size (Context);
      Point        : Coordinate := (0, 0);
      Channel      : Notcurses_Channel :=
         (Use_Palette => True,
          Palette     => 0,
          Alpha       => Opaque,
          Not_Default => True);
   begin
      for I in 0 .. Palette_Size loop
         Channel.Palette := Palette_Index (I);
         Set_Background (Plane, Channel);

         Put (Plane, " ",
            Y => Point.Y,
            X => Point.X);

         Point.X := Point.X + 1;

         if Point.X = Dims.X then
            Point.Y := Point.Y + 1;
            Point.X := 0;
         end if;

         if Point.Y >= Dims.Y then
            Point.Y := 0;
         end if;
      end loop;

      Notcurses.Context.Render (Context);
      delay 1.0;
      Notcurses.Plane.Erase (Plane);
   end Test_Palette;

   procedure Test_Dimensions is
      use Notcurses;
      use Notcurses.Plane;
      use Notcurses.Channel;
      Plane : constant Notcurses_Plane := Standard_Plane;

      Red   : Color_Type := 16#80#;
      Green : Color_Type := 16#80#;
      Blue  : Color_Type := 16#80#;

      Dims  : constant Coordinate := Dimensions (Plane);
   begin
      for Y in 0 .. Dims.Y - 1 loop
         for X in 0 .. Dims.X - 1 loop
            Set_Foreground_RGB (Plane, Red, Green, Blue);
            Set_Background_RGB (Plane, Blue, Red, Green);
            Put (Plane, "X", Y, X);
            Blue := Blue + 2;
            if Blue = Color_Type'Last then
               Green := Green + 2;
               if Green = Color_Type'Last then
                  Red := (Red + 2);
               end if;
            end if;
         end loop;
      end loop;
      Notcurses.Context.Render (Notcurses.Plane.Context (Plane));
      delay 1.0;
      Notcurses.Plane.Erase (Plane);
   end Test_Dimensions;

   procedure Test_Input is
      use Notcurses;
      use Notcurses.Plane;
      use Notcurses.Context;
      Plane   : constant Notcurses_Plane := Standard_Plane;
      Context : constant Notcurses_Context := Notcurses.Plane.Context (Plane);
      Input   : Wide_Wide_Character;
   begin
      Set_Background_RGB (Plane, R => 0, G => 0, B => 0);
      Enable_Cursor (Context, X => 0, Y => 1);
      Enable_Mouse (Context);

      loop
         Set_Foreground_RGB (Plane, R => 0, G => 255, B => 0);
         Put (Plane, "Press any key for information, ESC to exit", X => 0, Y => 0);
         Render (Context);

         Input := Notcurses.Context.Get (Context);
         Set_Foreground_RGB (Plane, R => 255, G => 255, B => 255);
         Erase (Plane);
         Put (Plane, "Id:",
            X => 2, Y => 2);
         Put (Plane, Wide_Wide_Character'Pos (Input)'Wide_Wide_Image,
            X => 12, Y => 2);

         Render (Context);
         exit when Input = Ada.Characters.Wide_Wide_Latin_1.ESC;
      end loop;
   end Test_Input;

   procedure Test_Plane_Split is
      use Notcurses;
      use Notcurses.Plane;

      Left  : Notcurses_Plane := Create_Sub_Plane
         (Plane    => Standard_Plane,
          Position => (0, 0),
          Size     => Dimensions (Standard_Plane));

      Right : Notcurses_Plane := Create_Sub_Plane
         (Plane    => Standard_Plane,
          Position =>
            (X => Dimensions (Standard_Plane).X / 2,
             Y => 0),
          Size     =>
            (X => Dimensions (Standard_Plane).X / 2,
             Y => Dimensions (Standard_Plane).Y));
   begin
      Set_Foreground_RGB (Left, 0, 255, 0);
      Put (Left, "Left");

      Set_Foreground_RGB (Right, 255, 0, 0);
      Put_Aligned (Right, "Right", Align => Notcurses.Plane.Right);
      Notcurses.Context.Render (Notcurses.Plane.Context (Standard_Plane));
      delay 1.0;

      Destroy (Left);
      Destroy (Right);
      Erase (Standard_Plane);
   end Test_Plane_Split;

   procedure Test_Progress_Bar is
      use Notcurses.Progress_Bar;
      use Notcurses.Plane;
      use Notcurses;
      Context : constant Notcurses_Context := Notcurses.Plane.Context (Standard_Plane);
      Plane   : constant Notcurses_Plane := Create_Sub_Plane
         (Plane    => Standard_Plane,
          Position => (0, 0),
          Size     =>
            (X => Dimensions (Standard_Plane).X,
             Y => 1));
      Bar : Notcurses_Progress_Bar := Create (Plane);
   begin
      for I in 1 .. 100 loop
         Set_Progress (Bar, Progress_Value (Float (I) / 100.0));
         delay 0.01;
         Notcurses.Context.Render (Context);
      end loop;
      Destroy (Bar);
      Erase (Standard_Plane);
   end Test_Progress_Bar;

end Tests;
