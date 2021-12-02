package body NC is
   function Is_Error (SC : Status_Code)
      return Boolean
   is (SC < 0);
end NC;
