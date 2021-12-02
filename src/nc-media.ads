with Interfaces.C_Streams; use Interfaces.C_Streams;

package NC.Media is
   function Init
      (Opts : access Options;
       FP   : FILEs := stdout)
       return access Context
   with Import, Convention => C, External_Name => "notcurses_init";
   --  Initialize a Notcurses context on the connected terminal at FP. FP must
   --  be a tty. You'll usually want stdout. null can be supplied for FP, in
   --  which case /dev/tty will be opened. Returns null on error, including any
   --  failure initializing terminfo.
end NC.Media;
