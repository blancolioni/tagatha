package Tagatha is

   type Word_8 is mod 2 ** 8;
   type Word_16 is mod 2 ** 16;
   type Word_32 is mod 2 ** 32;
   type Word_64 is mod 2 ** 64;

   type Int_8 is range -128 .. 127;
   type Int_16 is range -2 ** 15 .. 2 ** 15 - 1;
   type Int_32 is range -2 ** 31 .. 2 ** 31 - 1;
   type Int_64 is range -2 ** 63 .. 2 ** 63 - 1;

   type Floating_Point_Constant is new Long_Float;

   type Word_8_Array is array (Positive range <>) of Word_8;

   type Argument_Count is range 0 .. 99;
   type Local_Count    is range 0 .. 99;
   type Result_Count   is range 0 .. 99;
   type Return_Count   is range 0 .. 99;

   subtype Argument_Index is Argument_Count range 1 .. Argument_Count'Last;
   subtype Local_Index is Local_Count range 1 .. Local_Count'Last;
   subtype Result_Index is Result_Count range 1 .. Result_Count'Last;
   subtype Return_Index is Return_Count range 1 .. Return_Count'Last;

   type Temporary_Count is range 0 .. 9_999;
   subtype Temporary_Index is Temporary_Count range 1 .. Temporary_Count'Last;

   type Branch_Condition is (Always, Z, NZ);

   type Operator is
     (Op_Identity, Op_Negate, Op_Not, Op_Test,
      Op_Add, Op_Subtract, Op_Multiply, Op_Divide, Op_Mod,
      Op_Fadd, Op_Fsub, Op_Fmul, Op_Fdiv,
      Op_And, Op_Or, Op_Xor,
      Op_Dereference, Op_Store,
      Op_EQ, Op_NE, Op_LT, Op_LE, Op_GT, Op_GE);

   subtype Unary_Operator is Operator range Op_Identity .. Op_Test;
   subtype Binary_Operator is Operator range Op_Add .. Op_GE;
   subtype Compare_Operator is Operator range Op_EQ .. Op_GE;
   subtype Floating_Point_Operator is Operator range Op_Fadd .. Op_Fdiv;

   type Operand_Content is
     (General_Content,
      Floating_Point_Content);

   function Derive_Content
     (Content_1, Content_2, Content_3 : Operand_Content := General_Content)
      return Operand_Content
   is (Operand_Content'Max
       (Operand_Content'Max
          (Content_1, Content_2),
          Content_3));

end Tagatha;
