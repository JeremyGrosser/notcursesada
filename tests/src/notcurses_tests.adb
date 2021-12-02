with Interfaces.C.Strings; use Interfaces.C.Strings;
with NC.Media;
with NC;

procedure Notcurses_Tests is
   Status  : NC.Status_Code;
   Options : aliased NC.Options :=
      (Log_Level => NC.Silent,
       Flags =>
         (Suppress_Banners => True,
          others           => False),
       others => <>);
   Context : access NC.Context;
begin
   Status := NC.Lex_Margins (New_String ("1,2,1,2"), Options'Access);
   if NC.Is_Error (Status) then
      return;
   end if;

   Context := NC.Media.Init (Options'Access);
   if Context = null then
      return;
   end if;

   Status := NC.Stop (Context);
end Notcurses_Tests;
