with Ada.Exceptions;
with Ada.Text_IO;
with Notcurses;

procedure Tests is
   use type Notcurses.Coordinate;

   NC    : constant Notcurses.Context := Notcurses.Initialize;
   Root  : constant Notcurses.Plane := NC.Standard_Plane;

   Dim   : constant Notcurses.Coordinate := (1, 18);
   Child : constant Notcurses.Plane := Root.Create_Plane (Size => Dim);

   Pos : Notcurses.Coordinate := (0, 0);
   Vel : Notcurses.Coordinate := (1, 1);
begin
   --  NC.Enable_Cursor (NC.Cursor);
   Child.Put ("Hello, ");
   Child.Put ("notcurses",
      Style =>
         (Bold       => True,
          Underline  => True,
          others     => False),
      Foreground => Notcurses.RGB (16#FF#, 16#00#, 16#FF#));
   Child.Put ("!");
   NC.Render;

   loop
      Pos := Pos + Vel;
      if Pos.Y >= (Root.Dimensions.Y - Dim.Y) then
         Pos.Y := Root.Dimensions.Y - Dim.Y;
         Vel.Y := -Vel.Y;
      elsif Pos.Y < 0 then
         Pos.Y := 0;
         Vel.Y := -Vel.Y;
      end if;

      if Pos.X >= (Root.Dimensions.X - Dim.X) then
         Pos.X := Root.Dimensions.X - Dim.X;
         Vel.X := -Vel.X;
      elsif Pos.X < 0 then
         Pos.X := 0;
         Vel.X := -Vel.X;
      end if;

      Child.Move (Pos);
      NC.Render;
      delay 0.030;
   end loop;

   Child.Destroy;
   NC.Stop;
exception
   when E : others =>
      delay 1.0;
      NC.Stop;
      delay 1.0;
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Tests;
