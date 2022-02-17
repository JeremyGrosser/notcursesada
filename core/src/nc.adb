package body NC is
   function Is_Error (SC : Status_Code)
      return Boolean
   is (SC < 0);

   function Top
      (NC : not null access Context)
      return access Plane
   is (Pile_Top (Standard_Plane (NC)));

   function Bottom
      (NC : not null access Context)
      return access Plane
   is (Pile_Bottom (Standard_Plane (NC)));

   function Render
      (NC : not null access Context)
      return Status_Code
   is
      P : access Plane := Standard_Plane (NC);
   begin
      if Pile_Render (P) /= 0 then
         return -1;
      else
         return Pile_Rasterize (P);
      end if;
   end Render;

   function Has_Modifiers
      (Ni : not null access Input)
      return Boolean
   is (Ni.Alt or Ni.Ctrl or Ni.Shift);

   function Mice_Disable
      (N : not null access Context)
       return Status_Code
   is (Mice_Enable (N, Mouse_Event_Mask'(others => False)));

   function Plane_Resize_Simple
      (N : access Plane;
       Y_Len, X_Len : Interfaces.C.unsigned)
       return Interfaces.C.int
   is
      use Interfaces.C;
      Old_Y, Old_X : aliased unsigned;
      Keep_Len_Y, Keep_Len_X : unsigned;
   begin
      Dimensions (N, Old_Y'Access, Old_X'Access);
      Keep_Len_Y := (if Old_Y > Y_Len then Y_Len else Old_Y);
      Keep_Len_X := (if Old_X > X_Len then X_Len else Old_X);
      return Plane_Resize (N, 0, 0, Keep_Len_Y, Keep_Len_X, 0, 0, Y_Len, X_Len);
   end Plane_Resize_Simple;

   procedure Cell_Init
      (C : access Cell)
   is
   begin
      C.all := (others => <>);
   end Cell_Init;

   function Cell_Prime
      (N          : access Plane;
       C          : access Cell;
       GCluster   : chars_ptr;
       Style_Mask : Unsigned_16;
       Channels   : Unsigned_64)
       return Interfaces.C.int
   is
   begin
      C.Style_Mask := Style_Mask;
      C.Channels := Channels;
      return Cell_Load (N, C, GCluster);
   end Cell_Prime;

   function Plane_Move_Relative
      (N : not null access Plane;
       Y, X : Interfaces.C.int)
       return Interfaces.C.int
   is
      use Interfaces.C;
      OY, OX : aliased Interfaces.C.int;
   begin
      Plane_YX (N, OY'Access, OX'Access);
      return Plane_Move_YX (N, OY + Y, OX + X);
   end Plane_Move_Relative;

   function Plane_Descendant_P
      (N        : access constant Plane;
       Ancestor : access constant Plane)
       return Interfaces.C.int
   is
      Parent : access constant Plane := Plane_Parent_Constant (N);
   begin
      while Parent /= Ancestor loop
         if Plane_Parent_Constant (Parent) = Parent then
            return 0;
         end if;
         Parent := Plane_Parent_Constant (Parent);
      end loop;
      return 1;
   end Plane_Descendant_P;

end NC;
