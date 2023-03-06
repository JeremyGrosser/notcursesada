with Ada.Wide_Wide_Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;
with Notcurses;
with Notcurses_Keys;

procedure Tests is
   package NC renames Notcurses;
   use type NC.Coordinate;
   use type NC.Input_Action;

   Context : constant NC.Context := NC.Initialize;
   Root    : constant NC.Plane := Context.Standard_Plane;

   Dim   : constant NC.Coordinate := (1, 18);
   Child : constant NC.Plane := Root.Create_Plane (Size => Dim);
   Debug : constant NC.Plane := Root.Create_Plane (Size => (32, 80));
   Event : NC.Input_Event;

   Pos : NC.Coordinate := (0, 0);
   Vel : NC.Coordinate := (1, 1);
   Ch  : Wide_Wide_Character;
begin
   --  Context.Enable_Cursor (Context.Cursor);
   Child.Put ("Hello, ");
   Child.Put ("notcurses",
      Style =>
         (Bold       => True,
          Underline  => True,
          others     => False),
      Foreground => NC.RGB (16#FF#, 16#00#, 16#FF#));
   Child.Put ("!");
   Child.Erase;
   Context.Render;
   delay 0.5;

   Debug.Set_Scrolling (Enabled => True);

   loop
      Event := Context.Get_Blocking;
      --  exit when Event.Id = Notcurses_Keys.NCKEY_ESC;
      if not NC.Is_Key_Event (Event) then
         Ch := NC.To_Wide_Wide_Character (Event);
      end if;

      Debug.Erase;
      Debug.Put_String (Event'Image);
      Debug.New_Line;
      Debug.Put_String (Wide_Wide_Character'Pos (Ch)'Image);
      Debug.New_Line;
      if not Ada.Wide_Wide_Characters.Handling.Is_Control (Ch) then
         Debug.Put (Ch, Foreground => NC.RGB (0, 255, 0));
         Debug.New_Line;
      end if;
      Context.Render;

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

      --  Child.Move (Pos);
      --  Context.Render;
      --  delay 0.030;
   end loop;

   Child.Destroy;
   Context.Stop;
exception
   when E : others =>
      delay 10.0;
      Context.Stop;
      delay 10.0;
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Tests;
