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
end NC;
