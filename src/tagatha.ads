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

   --  Per-slot content of a routine's frame.  A slot's content fixes its
   --  width: General_Content is one machine word, Floating_Point_Content is
   --  a double, which is two words on a 32-bit target.  A backend that lays
   --  arguments, results and locals out in fixed-size slots maps index to
   --  offset with a prefix sum over these widths, so it needs the content of
   --  every *preceding* slot, not just the one being accessed -- which is why
   --  this travels with Begin_Routine rather than with each operand.
   --
   --  Caller and callee must agree, so a float argument or result has to be
   --  declared even when the routine body never touches it (see
   --  Tagatha.Code.Set_Argument_Content).
   type Argument_Content_Array is array (Argument_Index) of Operand_Content;
   type Result_Content_Array   is array (Result_Index) of Operand_Content;
   type Local_Content_Array    is array (Local_Index) of Operand_Content;
   type Return_Content_Array   is array (Return_Index) of Operand_Content;

   General_Arguments : constant Argument_Content_Array :=
                         [others => General_Content];
   General_Results   : constant Result_Content_Array :=
                         [others => General_Content];
   General_Locals    : constant Local_Content_Array :=
                         [others => General_Content];
   General_Returns   : constant Return_Content_Array :=
                         [others => General_Content];

   type Frame_Layout is
      record
         Arguments : Argument_Content_Array := General_Arguments;
         Results   : Result_Content_Array   := General_Results;
         Locals    : Local_Content_Array    := General_Locals;
      end record;

   General_Frame : constant Frame_Layout := (others => <>);

   function Derive_Content
     (Content_1, Content_2, Content_3 : Operand_Content := General_Content)
      return Operand_Content
   is (Operand_Content'Max
       (Operand_Content'Max
          (Content_1, Content_2),
          Content_3));

end Tagatha;
