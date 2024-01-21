package Tagatha.Conversions is

   function Int_32_To_Word_64
     (Value : Int_32)
      return Word_64;

   function Floating_Point_To_Word_64
     (Value : Floating_Point_Constant)
      return Word_64;

   function Word_64_To_Floating_Point
     (Value : Word_64)
      return Floating_Point_Constant;

end Tagatha.Conversions;
