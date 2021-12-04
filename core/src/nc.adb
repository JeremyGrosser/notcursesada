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

end NC;
