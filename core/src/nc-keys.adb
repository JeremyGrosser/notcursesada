package body NC.Keys is

   function Is_Synthesized_Event
      (W : Unsigned_32)
      return Boolean
   is (W in PRETERUNICODEBASE .. NCKEY_EOF);

   function Is_Synthesized_Mouse_Event
      (W : Unsigned_32)
      return Boolean
   is (W in NCKEY_MOTION .. NCKEY_BUTTON11);

   function In_Private_Use_Area
      (W : Unsigned_32)
      return Boolean
   is (W in 16#E000# .. 16#F8FF#);

   function In_Supplementary_Private_Use_Area_A
      (W : Unsigned_32)
      return Boolean
   is (W in 16#F0000# .. 16#FFFFD#);

   function In_Supplementary_Private_Use_Area_B
      (W : Unsigned_32)
      return Boolean
   is (W in 16#100000# .. 16#10FFFD#);

end NC.Keys;
