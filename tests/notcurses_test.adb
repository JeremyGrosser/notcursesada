--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Exceptions;
with Ada.Text_IO;

with Tests; use Tests;
with Notcurses.Context;
with Notcurses;

procedure Notcurses_Test is
begin
   begin
      Notcurses.Context.Initialize;
      Test_Version;
      Test_Hello_World;
      Test_Colors;
      Test_Palette;
      Test_Dimensions;
      Test_Plane_Split;
      Test_Progress_Bar;
      Test_Visual_File;
      Test_Visual_Bitmap;
      Test_Input;
      Notcurses.Context.Stop;
   exception
      when E : others =>
         Notcurses.Context.Stop;
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         return;
   end;

   Test_Direct;
end Notcurses_Test;
