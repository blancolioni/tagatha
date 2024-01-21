with Ada.Unchecked_Conversion;

package body Tagatha.Conversions is

   -------------------------------
   -- Floating_Point_To_Word_64 --
   -------------------------------

   function Floating_Point_To_Word_64
     (Value : Floating_Point_Constant)
      return Word_64
   is
      function To_Word_64 is
        new Ada.Unchecked_Conversion (Floating_Point_Constant, Word_64);
   begin
      return To_Word_64 (Value);
   end Floating_Point_To_Word_64;

   -----------------------
   -- Int_32_To_Word_64 --
   -----------------------

   function Int_32_To_Word_64
     (Value : Int_32)
      return Word_64
   is
   begin
      if Value >= 0 then
         return Word_64 (Value);
      elsif Value = Int_32'First then
         return 2 ** 63;
      else
         return 2 ** 32 - Word_64 (-Value)
           + (2 ** 32 - 1) * 2 ** 32;
      end if;
   end Int_32_To_Word_64;

   -------------------------------
   -- Word_64_To_Floating_Point --
   -------------------------------

   function Word_64_To_Floating_Point
     (Value : Word_64)
      return Floating_Point_Constant
   is
      function To_Floating_Point is
        new Ada.Unchecked_Conversion (Word_64, Floating_Point_Constant);
   begin
      return To_Floating_Point (Value);
   end Word_64_To_Floating_Point;

end Tagatha.Conversions;
