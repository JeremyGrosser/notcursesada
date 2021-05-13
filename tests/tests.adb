--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Exceptions;
with Interfaces; use Interfaces;
with Notcurses.Context;
with Notcurses.Direct;
with Notcurses.Plane;
with Notcurses;

package body Tests is
   procedure Test_Version is
   begin
      Put_Line (Notcurses.Version);
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

   procedure Test_Dimensions is
      use Notcurses;
      use Notcurses.Plane;
      Plane : constant Notcurses_Plane := Standard_Plane;

      Red   : Unsigned_8 := 16#80#;
      Green : Unsigned_8 := 16#80#;
      Blue  : Unsigned_8 := 16#80#;

      Dims  : constant Coordinate := Dimensions (Plane);
   begin
      for Y in 0 .. Dims.Y loop
         for X in 0 .. Dims.X loop
            Set_Foreground_RGB (Plane, Red, Green, Blue);
            Set_Background_RGB (Plane, Blue, Red, Green);
            Put (Plane, "X", Y, X);
            Blue := Blue + 2;
            if Blue = Unsigned_8'Last then
               Green := Green + 2;
               if Green = Unsigned_8'Last then
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
      Dims    : constant Coordinate := Dimensions (Plane);
      Input   : Notcurses_Input;
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
         Erase_Region (Plane,
            Start => (X => 2, Y => 2),
            Size  => (X => Dims.X - 2, Y => Dims.Y - 2));
         Put (Plane, "Id:",
            X => 2, Y => 2);
         Put (Plane, Input.Id'Wide_Wide_Image,
            X => 12, Y => 2);

         Put (Plane, "XY:",
            X => 2, Y => 3);
         Put (Plane, Input.X'Wide_Wide_Image & "," & Input.Y'Wide_Wide_Image,
            X => 11, Y => 3);

         Put (Plane, "Seqnum:",
            X => 2, Y => 4);
         Put (Plane, Input.Seqnum'Wide_Wide_Image,
            X => 11, Y => 4);

         --  TODO: the modifiers never seem to be TRUE and sometimes have incorrect values
         --  if Input.Alt then
         --     Put (Plane, "ALT", X => 2, Y => 5);
         --  end if;

         --  if Input.Shift then
         --     Put (Plane, "SHIFT", X => 6, Y => 5);
         --  end if;

         --  if Input.Ctrl then
         --     Put (Plane, "CTRL", X => 12, Y => 5);
         --  end if;

         Render (Context);
         exit when Input.Id = Ada.Characters.Wide_Wide_Latin_1.ESC;
      end loop;
   end Test_Input;

   procedure Test_Plane_Split is
      use Notcurses;
      use Notcurses.Plane;

      Left  : constant Notcurses_Plane := Create_Sub_Plane
         (Plane    => Standard_Plane,
          Position => (0, 0),
          Size     => Dimensions (Standard_Plane));

      Right : constant Notcurses_Plane := Create_Sub_Plane
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
      Put (Right, "Right");
      Notcurses.Context.Render (Notcurses.Plane.Context (Standard_Plane));
      delay 1.0;

      Destroy (Left);
      Destroy (Right);
      Erase (Standard_Plane);
   end Test_Plane_Split;

   procedure Test_Direct is
      use Notcurses.Direct;
      Plane : Notcurses_Direct;
      Dims  : Notcurses.Coordinate;
   begin
      Initialize (Plane);
      Set_Cursor_Enabled (Plane, False);
      Dims := Dimensions (Plane);
      Put (Plane, "X=" & Dims.X'Image & ASCII.LF & "Y=" & Dims.Y'Image & ASCII.LF);
      Stop (Plane);
   exception
      when E : others =>
         Stop (Plane);
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Test_Direct;
end Tests;
